open Core
open! Int.Replace_polymorphic_compare
open Async

(* force to specify Core.Unix or Async.Unix. The preferred way to do syscalls is to use
   In_thread.run (fun () -> several syscalls using Core.Unix ). But if there is only
   one syscall to do, or if the syscall doesn't matter performance-wise, sticking to
   Async.Unix is fine.
   Beware that Path functions oftentimes look things up in the intern table, so calling
   them in an In_thread.run can race. *)
module Unix = struct end

module Heart = Tenacious.Heart
module Glass = Heart.Glass

let find_or_add_memoized ~name ~key_to_string ht ~key f =
  Hashtbl.find_or_add ht key ~default:(
    fun () ->
      Tenacious.memoize
        ~name:(lazy (
          sprintf "find_or_add_memoized %S: %s" name (key_to_string key)))
        (f key))

let table_of_memoized ~name ~key_to_string hashable f =
  Memo.general ~hashable (fun x ->
    Tenacious.memoize
      ~name:(lazy (
        sprintf "table_of_memoized %S: %s" name (key_to_string x))) (f x))

let unbreakable x = x,Heart.unbreakable

(* Naming...
   memo - dynamic cache of computations
   persist - persistent cache of values
*)

let ( *>>= ) t f = Tenacious.bind t ~f
let ( *>>| ) t f = Tenacious.map t ~f

module Kind = Db.Kind
module Mtime = Db.Mtime
module Stats = Db.Stats
module Listing = Db.Listing
module Digest = Db.Digest


(*----------------------------------------------------------------------
 System lstat - counted; exns caught
----------------------------------------------------------------------*)

let unix_stat_blocking ~follow_symlinks path =
  Metrics.Counter.incr Progress.lstat_counter;
  Result.try_with (fun () ->
    if follow_symlinks
    then Core.Unix.stat path
    else Core.Unix.lstat path)
;;

let stat_blocking ~follow_symlinks path =
  match unix_stat_blocking ~follow_symlinks path with
  | Ok u -> `ok (Stats.of_unix_stats u)
  | Error (Core.Unix.Unix_error ((ENOENT | ENOTDIR), _, _)) ->
    `does_not_exist
  | Error exn ->
    (* things like "permission denied" *)
    `unknown_error (Error.of_exn exn)

let stat ~follow_symlinks path =
  let path = Path.to_absolute_string path in
  In_thread.run (fun () -> stat_blocking ~follow_symlinks path)

let is_dir stats =
  match Stats.kind stats with
  | `Directory -> true
  | _ -> false

let is_digestable stats =
  match Stats.kind stats with
  | `File | `Link -> true
  | _ -> false

(*----------------------------------------------------------------------
 Ocaml_digest (count!)
----------------------------------------------------------------------*)

module Ocaml_digest : sig
  val init: Config.t -> unit
  val of_file : Path.t -> Md5.t Or_error.t Deferred.t
end = struct
  let throttle = ref None
  let set max_concurrent_jobs =
    throttle := Some (Throttle.create ~continue_on_error:true ~max_concurrent_jobs)
  ;;
  let init config =
    match !throttle with
    | Some _ -> failwith "Fs.Ocaml_digest.init called more than once"
    | None -> set (Config.d_number config)

  let of_file_blocking path =
    Or_error.try_with (fun () ->
      Metrics.Counter.incr Progress.digest_counter;
      Core.Md5.digest_file_blocking path)

  let of_file path =
    Throttle.enqueue (Option.value_exn !throttle) (fun () ->
      let path = Path.to_absolute_string path in
      In_thread.run (fun () -> of_file_blocking path))

end

(*----------------------------------------------------------------------
  Listing (count!)
  ----------------------------------------------------------------------*)

let run_ls_impl_blocking ~dir ~dir_as_string =
  Metrics.Counter.incr Progress.ls_counter;
  Or_error.try_with (fun () ->
    Exn.protectx (Core.Unix.opendir dir_as_string)
      ~finally:Core.Unix.closedir
      ~f:(fun dir_handle ->
         let rec loop acc =
           match Core.Unix.readdir_opt dir_handle with
           | Some ("." | "..") -> loop acc
           | None -> Listing.create ~dir ~elems:acc
           | Some base ->
             let path_string = dir_as_string ^ "/" ^ base in
             match unix_stat_blocking ~follow_symlinks:false path_string with
             | Error _ ->
               (* File disappeared between readdir & lstat system calls.
                  Handle as if readdir never told as about it *)
               loop acc
             | Ok u ->
               let kind = Async.Unix.File_kind.of_unix u.st_kind in
               loop (Listing.Elem.create ~base ~kind :: acc)
         in
         loop []))
;;

let run_ls ~dir =
  Locking.lock_directory_for_listing ~dir (fun () ->
    File_access.enqueue (fun () ->
      let dir_as_string = Path.to_string dir in
      In_thread.run (fun () ->
        run_ls_impl_blocking ~dir ~dir_as_string)))
;;

(*----------------------------------------------------------------------
 Watcher (inotify wrapper)
----------------------------------------------------------------------*)

let break_cache_glass ?(show=false) cache path =
  match Hashtbl.find cache path with
  | None -> ()
  | Some glass ->
    (if show then Message.file_changed ~desc:(Path.to_string path));
    assert (not (Glass.is_broken glass));
    (* remove glass from HT *before* breaking it *)
    Hashtbl.remove cache path;
    Glass.break glass

module Real_watcher : sig

  type t
  val create :
    (* ignore events completely for these paths *)
    ignore:(Path.t -> bool) ->
    (* pay attention to, but dont report file_changes for these events *)
    expect:(Path.t -> bool) ->
    t Deferred.t

  val watch_file_or_dir : t ->
    how:[`via_parent|`directly] ->
    Path.t ->
    Heart.t Or_error.t Deferred.t

  val clear_watcher_cache : t -> Path.t -> unit

end = struct

  module Inotify = Async_inotify

  type t = {
    ignore : (Path.t -> bool);
    expect : (Path.t -> bool);
    notifier : Inotify.t ;
    (* An entry [x, f] in [watching_via_parent] means we have set up an inotify watcher on
       [dirname x]. [f] will tell you when [x] might have changed according to that
       watcher.
       Note that this kind of watcher is sufficient for files because you get Modify
       events just by watching the parent, but not sufficient for
       directories. Also if a direct watcher has been established, events from the direct
       watcher will be reflected in [f].
    *)
    watching_via_parent : Glass.t Path.Table.t;
    (* An entry [x, f] in [watching_directly] means we have set up an inotify watcher on
       [x] itself and [f] will tell you when [x] might have changed.
       This kind of watcher is necessary for watching directories, but it's harder
       to establish: you can't start watching a file that does not exist.

       Note that due to a quirk of [Async_inotify] ignoring [Delete_self] events,
       this watcher might not be sufficient by itself.
    *)
    watching_directly : Glass.t Path.Table.t;
  }

  let paths_of_event t =
    let paths s = (* for the filename & the dir in which it resides *)
      let path = Path.of_absolute_string s in
      if (t.ignore path) then [] else
        let dir = Path.dirname path in
        [true, path; false, dir]
    in
    function
    | Inotify.Event.Queue_overflow ->
      Message.error "Inotify.Event.Queue_overflow"; [] (* at least log an error *)
    | Inotify.Event.Modified s -> [true, Path.of_absolute_string s] (* just the filename *)
    | Inotify.Event.Unlinked s -> paths s
    | Inotify.Event.Created s -> paths s
    | Inotify.Event.Moved m ->
      match m with
      | Inotify.Event.Away s -> paths s
      | Inotify.Event.Into s -> paths s
      | Inotify.Event.Move (s1,s2) -> paths s1 @ paths s2

  let clear_watcher_cache t path =
    break_cache_glass t.watching_via_parent path;
    break_cache_glass t.watching_directly (Path.dirname path)

  let suck_notifier_pipe t piper =
    don't_wait_for (
      Pipe.iter_without_pushback piper ~f:(fun event ->
        (* Message.unlogged "Watcher, event: %s" (Inotify.Event.to_string event); *)
        List.iter (paths_of_event t event) ~f:(fun (show, path) ->
          (* Show path events to which we are sensitized...
             Don't show events for targets of currently running actions.
             Will see events for externally changed files (i.e. edited source files
             or removed generated files), but also for paths affected by a running
             jenga action which are not declared as targets of that action. *)
          let show = show && not (t.expect path) in
          break_cache_glass ~show t.watching_via_parent path;
          break_cache_glass       t.watching_directly path (* why not show here? *)
        )))

  let create ~ignore ~expect =
    (* Have to pass a path at creation - what a pain, dont have one yet! *)
    Inotify.create
      ~modify_event_selector:`Closed_writable_fd
      ~recursive:false
      ~watch_new_dirs:false
      "/"
    >>| fun (notifier,_) ->
    let watching_via_parent = Path.Table.create () in
    let watching_directly = Path.Table.create () in
    let t = {ignore; expect; notifier; watching_via_parent; watching_directly} in
    suck_notifier_pipe t (Inotify.pipe notifier);
    t

  let dir_and_cache t ~how path =
    match how with
    | `via_parent -> Path.dirname path  , t.watching_via_parent
    | `directly -> path                , t.watching_directly

  let watch_file_or_dir t ~how path =
    let path = Path.of_absolute_string (Path.to_absolute_string path) in
    let dir, cache = dir_and_cache t ~how path in
    (* Message.unlogged "watch: %s (dir: %s)" (Path.to_string path) (Path.to_string dir); *)
    match (Hashtbl.find cache path) with
    | Some glass ->
      assert (not (Glass.is_broken glass));
      return (Ok (Heart.watch glass))
    | None ->
      (* A notifier is setup to watch [dir], computed from [path] & [what]
         The underlying Inotify module handles repeated setup for same path.
         So we make no attempt to avoid the repeats. *)
      let absolute_dir = Path.to_absolute_string dir in
      (* Message.unlogged "setup watcher: %s" absolute_dir; *)
      Monitor.try_with (fun () ->
        Inotify.add t.notifier absolute_dir
      ) >>| function
      | Error exn -> Error (Error.of_exn exn)
      | Ok () ->
        let default () = Glass.create () in
        let glass = Hashtbl.find_or_add cache path ~default in
        Ok (Heart.watch glass)

end

(*----------------------------------------------------------------------
 Blind_watcher
----------------------------------------------------------------------*)

module Blind_watcher : sig

  type t
  val create : no_fs_triggers:bool -> unit -> t

  val clear_watcher_cache : t -> Path.t -> needed_for_correctness:bool -> unit

  val dont_watch_file_or_dir : t ->
    how:[`via_parent|`directly] ->
    Path.t ->
    Heart.t

end = struct

  type t = {
    watching_via_parent : Glass.t Path.Table.t;
    watching_directly : Glass.t Path.Table.t;
    no_fs_triggers : bool;
  }

  let create ~no_fs_triggers () =
    let watching_via_parent = Path.Table.create () in
    let watching_directly = Path.Table.create () in
    let t = {watching_via_parent; watching_directly; no_fs_triggers} in
    t

  let clear_watcher_cache t path ~needed_for_correctness =
    if needed_for_correctness || not (t.no_fs_triggers) then begin
      break_cache_glass t.watching_via_parent path;
      break_cache_glass t.watching_directly (Path.dirname path);
    end

  let get_cache t ~how =
    match how with
    | `via_parent -> t.watching_via_parent
    | `directly -> t.watching_directly

  let dont_watch_file_or_dir t ~how path =
    let cache = get_cache t ~how in
    match Hashtbl.find cache path with
    | Some glass ->
      assert (not (Glass.is_broken glass));
      Heart.watch glass
    | None ->
      let glass = Glass.create () in
      Hashtbl.add_exn cache ~key:path ~data:glass;
      Heart.watch glass

end

(*----------------------------------------------------------------------
 Watcher -- adding support for no-notifiers
----------------------------------------------------------------------*)

module Watcher : sig

  type t
  val create :
    nono: bool ->
    ignore:(Path.t -> bool) ->
    expect:(Path.t -> bool) ->
    no_fs_triggers:bool ->
    t Deferred.t

  val watch_file_or_dir : t ->
    how:[`via_parent|`directly] ->
    Path.t ->
    Heart.t Or_error.t Deferred.t

  val clear_watcher_cache : t -> Path.t -> needed_for_correctness:bool -> unit

end = struct

  type t =
  | Real of Real_watcher.t
  | Blind of Blind_watcher.t

  let create ~nono ~ignore ~expect ~no_fs_triggers =
    if nono
    then return (Blind (Blind_watcher.create ~no_fs_triggers ()))
    else if no_fs_triggers
    then failwith "-no-fs-triggers in only valid with -no-notifiers"
    else Real_watcher.create ~ignore ~expect >>| fun w -> Real w

  let watch_file_or_dir t ~how path =
    match t with
    | Real w -> Real_watcher.watch_file_or_dir w ~how path
    | Blind w -> return (Ok (Blind_watcher.dont_watch_file_or_dir w ~how path))

  (* [clear_watcher_cache] - clears the cache to ensure the file will be stated
     again. Necessary when running without notifiers; sensible even with notifiers, to
     avoid relying on the delivery of inotify events. *)
  let clear_watcher_cache t path ~needed_for_correctness =
    match t with
    | Real w  -> Real_watcher.clear_watcher_cache w path
    | Blind w -> Blind_watcher.clear_watcher_cache w path ~needed_for_correctness

end

module Listing_result = struct
  module T = struct
    type t = [
      | `does_not_exist
      | `not_a_dir
      | `listing of Listing.t
    ] [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)
end

(** Stats for a path, as comes from [lstat]. *)
module Lstat_result = struct
  type t = [
  | `does_not_exist
  | `stats of Stats.t
  ] [@@deriving hash, compare, sexp]

  include Comparable.Make(struct
    type nonrec t = t [@@deriving hash, compare, sexp]
  end)
end

(** Stats for a path when all symlinks are resolved.
    It's not valid to have [`Symlink] as a kind here. *)
module Stat_result = Lstat_result

(** Inode and kind of the thing at a given path.
    Useful to know when you need to make a new inotify subscription. *)
module Inode_result = struct
  type t = [
  | `does_not_exist
  | `inode of (Kind.t * int * int)
  ] [@@deriving hash, compare, sexp]

  include Comparable.Make(struct
    type nonrec t = t [@@deriving hash, compare, sexp]
  end)
end

let cutoff_equal_ignore_errors = fun f a b -> match a, b with
  | Ok l1, Ok l2 -> f l1 l2
  | Error _, Error _ -> false
  | Ok _, Error _ -> false
  | Error _, Ok _ -> false

(** File system logic. *)
module Fs_memo : sig
  type t
  val create : Watcher.t -> t
  val stat : t -> Path.t -> Stat_result.t Or_error.t Tenacious.t
  val list_dir : t -> Path.t -> Listing_result.t Or_error.t Tenacious.t
end = struct

  open Tenacious.Result

  let plain_stat = stat

  type t =
    { stat : Path.t -> Stat_result.t Or_error.t Tenacious.t
    ; list_dir : Path.t -> Listing_result.t Or_error.t Tenacious.t
    }
  [@@deriving fields]

  let create watcher =
    let rec uncached_lstat path : Lstat_result.t Or_error.t Tenacious.t =
      let parent_is_a_directory () =
        Tenacious.lift (fun () ->
          let open Deferred in
          (* We start watching assuming it's a file (noticing the path appearing/
             disappearing/[being modified if it's a file]).
             If it happens to be a directory, we additionally have to watch the directory
             itself.
          *)
          Watcher.watch_file_or_dir watcher ~how:`via_parent path
          >>= function
          | Error err ->
            return (unbreakable (Error (
              Error.tag err
                ~tag:(sprintf "%S's parent directory exists, but watcher set up failed"
                (Path.to_string path)))))
          | Ok heart ->
            plain_stat ~follow_symlinks:false path
            >>= fun res ->
            (match res with
             | `ok stats when is_dir stats ->
               Watcher.watch_file_or_dir watcher ~how:`directly path
             | _ ->
               Deferred.return (Ok Heart.unbreakable))
            >>| function
            | Error err ->
              Error (
                Error.tag err
                  ~tag:(sprintf "%S exists, but watcher set up failed"
                     (Path.to_string path))), heart
            | Ok heart2 ->
              let heart = Heart.combine2 heart heart2 in
              (Ok res, heart))
        >>= function
        | `ok stats -> return (`stats stats)
        | `does_not_exist -> return `does_not_exist
        | `unknown_error err -> fail err
      in
      if Path.is_a_root path
      then
        parent_is_a_directory ()
      else
        (* watch for directory and its parents being
           deleted/created/moved/symlink destination changed *)
        inode (Path.dirname path) *>>= function
        | Error err -> Tenacious.return (Error err)
        | Ok `does_not_exist -> return `does_not_exist
        | Ok (`inode (`Directory, _, _)) ->
          parent_is_a_directory ()
        | Ok (`inode (`Link, _, _)) ->
          assert false (* Inoder is not allowed to return `Link *)
        | Ok (`inode (_, _, _)) ->
          return `does_not_exist

    (* Resolves symlinks. A symlink with target path missing is considered non-existing. *)
    and uncached_stat path : Stat_result.t Or_error.t Tenacious.t =
      lstat path
      >>= function
      | `does_not_exist -> return `does_not_exist
      | `stats stats ->
        match Stats.kind stats with
        | `Socket | `Directory | `Block | `Fifo | `Char | `File ->
          return (`stats stats)
        | `Link ->
          Tenacious.lift (fun () ->
            let open Deferred in
            Async.Unix.readlink (Path.to_absolute_string path)
            (* This 'unbreakable' is guarded by 'lstat' above:
               We assume 'readlink' will return the same result as long as the stat
               stays unchanged. *)
            >>| unbreakable
          )
          *>>= fun link_destination ->
          let dir = Path.dirname path in
          stat (Path.relative_or_absolute ~dir link_destination)

    (* Depends on inode across the symlinks *)
    and uncached_inode x : Inode_result.t Or_error.t Tenacious.t =
      Tenacious.cutoff
        ~equal:(cutoff_equal_ignore_errors Inode_result.(=))
        (stat x
         *>>| Result.map ~f:(function
           | `stats stat ->
             `inode (Stats.kind stat, Stats.dev stat, Stats.ino stat)
           | `does_not_exist -> `does_not_exist))

    and lstat =
      let r = lazy (
        table_of_memoized
          ~name:"lstat" ~key_to_string:Path.to_string Path.hashable uncached_lstat)
      in
      fun x -> Lazy.force r x
    and stat =
      let r = lazy (
        table_of_memoized
          ~name:"stat" ~key_to_string:Path.to_string  Path.hashable uncached_stat)
      in
      fun x -> Lazy.force r x
    and inode =
      let r = lazy (
        table_of_memoized
          ~name:"inode" ~key_to_string:Path.to_string  Path.hashable uncached_inode)
      in
      fun x -> Lazy.force r x
    in

    (* Depends on inode across the symlinks *)
    let uncached_list_dir dir =
      stat dir >>= function
      | `does_not_exist -> return `does_not_exist
      | `stats stats ->
        if not (is_dir stats)
        then return `not_a_dir
        else
          Tenacious.lift (fun () ->
            let open Deferred in
            (* listing is valid as long as stats are the same,
               and we depend on stats above, so [unbreakable] *)
            run_ls ~dir >>| unbreakable
          )
          >>| fun x -> `listing x
    in
    let list_dir =
      table_of_memoized
        ~name:"list_dir" ~key_to_string:Path.to_string  Path.hashable uncached_list_dir
    in
    { stat; list_dir }

end

(*----------------------------------------------------------------------
 Digest/Listing result types
----------------------------------------------------------------------*)

module Contents_result = struct
  type t = [
  | `file_read_error of Error.t
  | `is_a_dir
  | `contents of string
  ]
end

module Digest_result = struct
  type t = [
  | `stat_error of Error.t
  | `does_not_exist
  | `is_a_dir
  | `undigestable of Kind.t
  | `digest_error of Error.t
  | `digest of Digest.t
  ]
end

(*----------------------------------------------------------------------
 contents_file (without persistence)
----------------------------------------------------------------------*)

let contents_file fsm ~file =
  Fs_memo.stat fsm file *>>= function
  | Error e -> Tenacious.return (`file_read_error e)
  | Ok __stats ->
    Tenacious.lift (fun () ->
      File_access.enqueue (fun () ->
        Deferred.Or_error.try_with (fun () ->
          Reader.file_contents (Path.to_absolute_string file)))
      >>| unbreakable
    ) *>>| function
    | Error e -> `file_read_error e
    | Ok new_contents -> `contents new_contents



(*----------------------------------------------------------------------
 Digest_persist - w.r.t stat->digest mapping
----------------------------------------------------------------------*)

module Digest_persist : sig

  val digest_file :
    Persist.t ->
    Fs_memo.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

end = struct

  let digest_file persist fsm ~file =
    let db = Persist.db persist in
    let cache = Db.digest_cache db in
    let remove() = Hashtbl.remove cache file in
    Fs_memo.stat fsm file *>>= function
    | Error e -> (remove(); Tenacious.return (`stat_error e))
    | Ok `does_not_exist -> (remove(); Tenacious.return `does_not_exist)
    | Ok (`stats stats) ->
      let kind = Stats.kind stats in
      if (is_dir stats) then Tenacious.return `is_a_dir else
      if not (is_digestable stats) then Tenacious.return (`undigestable kind) else
        match (
          match (Hashtbl.find cache file) with
          | None -> None
          | Some (prev_stats,prev_proxy) ->
            if Stats.equal stats prev_stats
            then Some prev_proxy
            else None
        ) with
        | Some old_good_digest -> Tenacious.return (`digest old_good_digest)
        | None ->
          Tenacious.lift (fun () ->
            Ocaml_digest.of_file file >>| unbreakable
          ) *>>= function
          | Error e -> (remove(); Tenacious.return (`digest_error e))
          | Ok digest ->
            let digest = Digest.intern digest in
            Hashtbl.set (Persist.modify "digest" cache) ~key:file ~data:(stats,digest);
            Tenacious.return (`digest digest)

end


(*----------------------------------------------------------------------
 Contents_memo
----------------------------------------------------------------------*)

module Contents_memo : sig

  type t
  val create : unit -> t

  val contents_file :
    t ->
    Fs_memo.t ->
    file:Path.t ->
    Contents_result.t Tenacious.t

end = struct

  type computation = Contents_result.t Tenacious.t
  type t = {
    cache : computation Path.Table.t;
  }

  let contents_file t sm ~file =
    find_or_add_memoized
      ~name:"contents"
      ~key_to_string:Path.to_string
      t.cache ~key:file (fun file -> contents_file sm ~file)

  let create () = {
    cache = Path.Table.create ();
  }

end

(*----------------------------------------------------------------------
 Digest_memo
----------------------------------------------------------------------*)

module Digest_memo : sig

  type t
  val create : unit -> t

  val digest_file :
    t ->
    Persist.t ->
    Fs_memo.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

end = struct

  type computation = Digest_result.t Tenacious.t
  type t = {
    cache : computation Path.Table.t;
  }

  let digest_file t dp sm ~file =
    find_or_add_memoized
      ~name:"digest"
      ~key_to_string:Path.to_string
      t.cache ~key:file (fun file ->
        Digest_persist.digest_file dp sm ~file
      )

  let create () = {
    cache = Path.Table.create ();
  }

end


(*----------------------------------------------------------------------
 Glob
----------------------------------------------------------------------*)

module Glob = struct

  include Db.Glob

  module Key = struct
    module T = struct
      type t = {
        dir : Path.t;
        pat : Pattern.t;
        kinds : Kind.t list option;
      } [@@deriving sexp, hash, compare]
    end
    include T
    include Hashable.Make(T)
  end

  let pattern t = Listing.Restriction.pattern (restriction t)
  let kind_allows_file t = Listing.Restriction.kind_allows_file (restriction t)

  let raw_create ~dir ~kinds pat =
    let restriction = Listing.Restriction.create ~kinds pat in
    let t = create ~dir ~restriction in
    (*Message.message "Glob.create: %s" (to_string t);*)
    t

  (* cache glob construction *)
  let the_cache : (Key.t, t) Hashtbl.t = Key.Table.create()

  let cached_create key =
    match (Hashtbl.find the_cache key) with
    | Some glob -> glob
    | None ->
      let {Key.dir;kinds;pat} = key in
      let glob = raw_create ~dir ~kinds pat in
      Hashtbl.add_exn the_cache ~key ~data:glob;
      glob

  let create1 ~dir ~kinds ~glob_string =
    let pat = Pattern.create_from_glob_string glob_string in
    let key = {Key. dir; kinds; pat} in
    cached_create key

  let create ~dir ?kinds glob_string =
    create1 ~dir ~kinds ~glob_string

  let create_from_path ~kinds path =
    let dir = Path.dirname path in
    let pat = Pattern.create_from_literal_string (Path.basename path) in
    let key = {Key. dir; kinds; pat} in
    cached_create key

  let exec_no_cutoff glob fsm =
    Fs_memo.list_dir fsm (dir glob) *>>= function
    | Ok (`listing listing) ->
      let restricted =
        Listing.restrict listing (restriction glob)
      in
      Tenacious.return (Ok (`listing restricted))
    | x ->
      Tenacious.return x

  let exec glob fsm =
    Tenacious.cutoff
      ~equal:(cutoff_equal_ignore_errors Listing_result.equal)
      (exec_no_cutoff glob fsm)

end

(*----------------------------------------------------------------------
 Glob_memo
----------------------------------------------------------------------*)

module Glob_memo : sig

  type t
  val create : unit -> t

  val list_glob :
    t ->
    Fs_memo.t ->
    Glob.t ->
    Listing_result.t Or_error.t Tenacious.t

end = struct

  type computation = Listing_result.t Or_error.t Tenacious.t
  type t = {
    cache : computation Glob.Table.t;
  }

  let list_glob t fsm glob =
    find_or_add_memoized
      ~name:"glob"
      ~key_to_string:Glob.to_string
      t.cache ~key:glob (fun glob ->
        Glob.exec glob fsm
      )

  let create () = {
    cache = Glob.Table.create ();
  }

end

let stat_directory memo ~dir =
  Fs_memo.stat memo (Path.of_relative dir)
  *>>| function
  | (Error _ | Ok `does_not_exist) as e -> e
  | Ok (`stats stats) ->
    if is_dir stats
    then Ok `ok
    else Or_error.errorf !"%{Path.Rel} is not a directory" dir
;;

let mkdir_ignoring_eexist_blocking dir =
  Metrics.Counter.incr Progress.mkdir_counter;
  match Core.Unix.mkdir dir with
  | () -> Ok ()
  | exception (Core.Unix.Unix_error (EEXIST, _, _)) -> Ok ()
  | exception e -> Error (Error.of_exn e)

let mkdir_ignoring_eexist dir =
  let dir = Path.Rel.to_string dir in
  In_thread.run (fun () -> mkdir_ignoring_eexist_blocking dir)

let rec ensure_directory memo watcher ~mkdir_root ~dir : unit Or_error.t Tenacious.t =
  (* In theory, we should probably cache the prefixes of these computations. In practice,
     the paths are always two directories deep, so caching would only be a small constant
     factor of improvement. *)
  begin
    if Path.Rel.(=) mkdir_root dir
    then Tenacious.return (Ok ())
    else ensure_directory memo watcher ~mkdir_root ~dir:(Path.Rel.dirname dir)
  end *>>= function
  | Error _ as e -> Tenacious.return e
  | Ok () ->
    Tenacious.lift (fun () -> mkdir_ignoring_eexist dir >>| unbreakable)
    *>>= fun (_ : unit Or_error.t) ->
    Tenacious.cutoff
      (* We can't really cutoff the non-ok cases, otherwise this could happen: we stat
         the directory, missing so we mkdir, someone deletes the directory, we self
         triggered so we stat again, this directory is missing so we cutoff and return
         successfully. *)
      ~equal:(fun a b -> match a, b with Ok `ok, Ok `ok -> true | _ -> false)
      (stat_directory memo ~dir)
    *>>= function
    | Error _ as e -> Tenacious.return e
    | Ok `ok -> Tenacious.return (Ok ())
    | Ok `does_not_exist ->
      (* Here we potentially self trigger by breaking the stat above. Oh well. *)
      Tenacious.lift (fun () -> mkdir_ignoring_eexist dir >>| unbreakable)
;;

let ensure_directory memo watcher ~mkdir_root ~dir =
  Tenacious.cutoff
    ~equal:[%compare.equal: unit Or_error.t]
    (match mkdir_root with
     | Some mkdir_root ->
       assert (Path.Rel.is_descendant ~dir:mkdir_root dir);
       ensure_directory memo watcher ~mkdir_root ~dir
     | None ->
       stat_directory memo ~dir
       *>>| function
       | Error _ as e -> e
       | Ok `ok -> Ok ()
       | Ok `does_not_exist -> Or_error.errorf !"%{Path.Rel} does not exist" dir)
;;

module Memo : sig

  type t
  val create : Watcher.t -> t

  val contents_file :
    t ->
    file:Path.t ->
    Contents_result.t Tenacious.t

  val digest_file :
    t ->
    Persist.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

  val list_glob :
    t ->
    Glob.t ->
    Listing_result.t Or_error.t Tenacious.t

  val mtime_file :
    t ->
    file:Path.t ->
    Mtime.t option Tenacious.t

  val ensure_directory
    : t
    -> Watcher.t
    -> mkdir_root:Path.Rel.t option
    -> dir:Path.Rel.t
    -> unit Core.Or_error.t Tenacious.tenacious

end = struct

  type t = {
    fsm : Fs_memo.t;
    cm : Contents_memo.t;
    dm : Digest_memo.t;
    gm : Glob_memo.t;
  }

  let create watcher = {
    fsm = Fs_memo.create watcher ;
    cm = Contents_memo.create () ;
    dm = Digest_memo.create () ;
    gm = Glob_memo.create () ;
  }

  let contents_file t ~file = Contents_memo.contents_file t.cm t.fsm ~file

  let digest_file t per ~file = Digest_memo.digest_file t.dm per t.fsm ~file

  let list_glob t glob = Glob_memo.list_glob t.gm t.fsm glob

  let mtime_file t ~file =
    Fs_memo.stat t.fsm file *>>| function
    | Error _ -> None
    | Ok `does_not_exist -> None
    | Ok (`stats stats) -> Some (Stats.mtime stats)

  let ensure_directory t watcher ~mkdir_root ~dir =
    ensure_directory t.fsm watcher ~mkdir_root ~dir
end

(*----------------------------------------------------------------------
 Fs - combination of persistent & dynamic caches
----------------------------------------------------------------------*)

let tmp_jenga =
  let user = Core.Unix.getlogin() in
  sprintf "/tmp/jenga-%s" user

module Fs : sig

  type t
  val create : Config.t -> Persist.t -> t Deferred.t

  val contents_file :
    t ->
    file:Path.t ->
    Contents_result.t Tenacious.t

  val digest_file :
    t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

  val list_glob :
    t ->
    Glob.t ->
    Listing_result.t Or_error.t Tenacious.t

  val watch_sync_file : t -> Path.t -> Heart.t Deferred.t
  val nono : t -> bool

  val clear_watcher_cache : t -> Path.t -> needed_for_correctness:bool -> unit

  val mtime_file : t -> file:Path.t -> Mtime.t option Tenacious.t

  val ensure_directory
    : t
    -> mkdir_root:Path.Rel.t option
    -> dir:Path.Rel.t
    -> unit Core.Or_error.t Tenacious.tenacious

end = struct

  type t = {
    nono : bool;
    watcher : Watcher.t;
    memo : Memo.t;
    persist : Persist.t;
  } [@@deriving fields]

  let create config persist =
    let nono = not System.has_inotify || Config.no_notifiers config in
    let ignore = Special_paths.Dot_jenga.matches in
    let expect path =
      match Path.case path with
      | `relative rel -> Locking.is_action_running_for_path rel
      | `absolute abs -> String.is_prefix ~prefix:tmp_jenga (Path.Abs.to_string abs)
    in
    Watcher.create ~nono ~ignore ~expect ~no_fs_triggers:config.no_fs_triggers
    >>= fun watcher ->
    let memo = Memo.create watcher in
    let t = { nono; watcher; memo; persist; } in
    return t

  let contents_file t ~file = Memo.contents_file t.memo ~file

  let digest_file t ~file = Memo.digest_file t.memo t.persist ~file

  let list_glob t glob = Memo.list_glob t.memo glob

  let watch_sync_file t path =
    Watcher.watch_file_or_dir t.watcher ~how:`via_parent path
    >>| ok_exn

  let clear_watcher_cache t path ~needed_for_correctness =
    Watcher.clear_watcher_cache t.watcher path ~needed_for_correctness

  let mtime_file t ~file = Memo.mtime_file t.memo ~file

  let ensure_directory t ~mkdir_root ~dir =
    Memo.ensure_directory t.memo t.watcher ~mkdir_root ~dir

end

include Fs

external caml_batched_mtimes : string list -> len:int -> float array = "caml_batched_mtimes"
let caml_batched_mtimes l = caml_batched_mtimes l ~len:(List.length l)

let%test_module _ = (module struct

  let slow_caml_batched_mtimes_for_comparison l =
    Array.of_list (List.map l ~f:(fun path -> (Core.Unix.stat path).st_mtime))

  let caml_batched_mtimes_with_same_error l =
    try caml_batched_mtimes l
    with Core.Unix.Unix_error (a, b, c) ->
      raise (Core.Unix.Unix_error (a, b, sprintf "((filename %s))" c))

  let compare l =
    [%test_eq: float array Or_error.t]
      (Or_error.try_with (fun () -> caml_batched_mtimes_with_same_error l))
      (Or_error.try_with (fun () -> slow_caml_batched_mtimes_for_comparison l))

  let%test_unit _ = compare ["fs.ml"; "build.ml"]
  let%test_unit _ = compare ["fs.ml"; "not-a-file"]
end)

let mtime_files_right_now files =
  let files_as_strings = List.map files ~f:Path.to_string in
  In_thread.run (fun () ->
    match caml_batched_mtimes files_as_strings with
    | exception (Core.Unix.Unix_error (_, _, file)) -> Error file
    | mtimes -> Ok mtimes)
  >>| Result.map ~f:(fun mtimes ->
    List.mapi files ~f:(fun i file ->
      Metrics.Counter.incr Progress.lstat_counter;
      file, Mtime.of_float mtimes.(i)))


(*----------------------------------------------------------------------
 syncronize for delivery of inotify events when finish running an action
----------------------------------------------------------------------*)

let pid_string = Pid.to_string (Core.Unix.getpid ())
let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u)

let created_but_not_deleted = Bag.create ()
let () =
  Shutdown.at_shutdown (fun () ->
    Deferred.List.iter (Bag.to_list created_but_not_deleted) ~f:(fun (_path,f) ->
      try_with (fun () ->
        f()) >>| fun _ -> ()
    ) >>= fun () ->
    return ()
  )

let unless_shutting_down ~f =
  match Shutdown.shutting_down() with
  | `Yes _ -> return ()
  | `No -> f ()

let () =
  don't_wait_for (
    Async.Unix.mkdir ~p:() tmp_jenga
  )

let heart_broken ~timeout heart =
  Heart.or_broken heart (Clock.after timeout)
  >>| function
  | None -> `broken
  | Some () -> `timeout

(** Wait for all inotify events generated by actions up until
    now to get processed.

    Note that, as we currently don't listen for IN_MODIFY events, this provides limited
    guarantees: you can't assume that the file reads you made are consistent if there were
    no inotify events in between.
*)
let sync_inotify_delivery : (t -> unit Deferred.t) =
  (* After a deferred computation is run, synchronise until all inotify events triggered
     while running have been delivered (and acted upon) by this process

     Do this by causing and waiting for a specific file-events:
     - the creation & removal of a temp file especially created for the purpose.

     As a side benefit, the contents of the temp file will indicate to the external world
     exactly which of those tenacious values synchronised sync are running at any moment.

     The temp path chosen has 3 parameters:
     - the pid of the jenga instance
     - a counter (unique to the pid), incremented for each synced tenacious
     - a counter (unique to a synced tenacious), incremented for each rerun.
  *)
  fun t ->
    if Fs.nono t then return () else (* no notifiers, so cant/dont sync *)
      let u = genU () in
      let path_string = sprintf "%s/jenga-%s-%d.sync" tmp_jenga pid_string u in
      let path = Path.absolute path_string in
      (* Setup watcher for sync-file *)
      watch_sync_file t path >>= fun created_heart ->
      (* setup remove()... *)
      let remove () =
        (* ensure creation event has happened.. *)
        begin heart_broken ~timeout:(sec 10.) created_heart >>| function
        | `broken -> ()
        | `timeout -> Message.error "lost inotify event for creation of: %s" path_string
        end >>= fun () ->
        (* Setup new watcher; unlink the file; wait for the event *)
        watch_sync_file t path >>= fun deleted_heart ->
        File_access.enqueue (fun () ->
          Sys.remove path_string
        ) >>= fun () ->
        begin heart_broken ~timeout:(sec 10.) deleted_heart >>| function
        | `broken -> ()
        | `timeout -> Message.error "lost inotify event for unlink of: %s" path_string
        end
      in
      (* create the sync-file *)
      let bag_elem = Bag.add created_but_not_deleted (path_string,remove) in
      unless_shutting_down ~f:(fun () ->
        File_access.enqueue (fun () ->
          Async.Unix.with_file path_string ~mode:[ `Wronly; `Creat ]
            ~f:(fun (_ : Fd.t) -> Deferred.unit)
        )
      ) >>= fun () ->
      (* remove the sync-file.. *)
      unless_shutting_down ~f:(fun () ->
        Bag.remove created_but_not_deleted bag_elem;
        remove ()
      )
      (* Now we are synchronised! *)

let lock_targets_and_mask_updates t ~targets f =
  Locking.lock_targets_for_action ~targets (fun () ->
    f () >>= fun res ->
    (* After running an action, synchronise until all inotify events triggered while the
       action was run have been delivered (and acted upon) by this process.

       This allows [fs.ml] to call [Locking.is_action_running_for_path] to avoid
       writing "changed" message for files targeted by this action. *)
    sync_inotify_delivery t >>| fun () ->
    res)
