open Core.Std
open! No_polymorphic_compare
open Async.Std

module Heart = Tenacious.Heart
module Glass = Heart.Glass

let memoize_reified_with_cache ht ~key f =
  Hashtbl.find_or_add ht key ~default:(
    fun () -> Tenacious.reify (f key))

let memoize_reified hashable f =
  Memo.general ~hashable (fun x -> Tenacious.reify (f x))

let unbreakable x = x,Heart.unbreakable

let lstat_counter = Effort.Counter.create "stat"
let digest_counter = Effort.Counter.create "digest"
let ls_counter = Effort.Counter.create "ls"
let mkdir_counter = Effort.Counter.create "mkdir"

(* Naming...
   memo - dynamic cache of computations
   persist - persistent cache of values
*)

let ( *>>= ) = Tenacious.bind
let ( *>>| ) t f = Tenacious.map t ~f

module Kind = Db.Kind
module Mtime = Db.Mtime
module Stats = Db.Stats
module Listing = Db.Listing
module Digest = Db.Digest


(*----------------------------------------------------------------------
 System lstat - counted; exns caught
----------------------------------------------------------------------*)

let unix_stat ~follow_symlinks path =
  Effort.track lstat_counter (fun () ->
    try_with ~extract_exn:true (fun () ->
      (*Message.message "stat: %s" path;*)
      if follow_symlinks then Unix.stat path
      else Unix.lstat path
    )
  )

let stat ~follow_symlinks path =
  let path = Path.to_absolute_string path in
  unix_stat ~follow_symlinks path >>| function
  | Ok u -> `ok (Stats.of_unix_stats u)
  | Error (Unix.Unix_error ((ENOENT | ENOTDIR), _, _)) ->
    `does_not_exist
  | Error exn ->
    (* things like "permission denied" *)
    `unknown_error (Error.of_exn exn)

let is_dir stats =
  match Stats.kind stats with
  | `Directory -> true
  | _ -> false

let is_digestable stats =
  match Stats.kind stats with
  | `File -> true
  | `Link -> true
  | _ -> false

(*----------------------------------------------------------------------
 ensure_directory
----------------------------------------------------------------------*)

module Ensure_directory_result = struct
  type t = [`ok | `failed of Error.t | `not_a_dir]
end

let ensure_directory ~dir =
  stat ~follow_symlinks:false dir >>= function
  | `ok stats -> return (if is_dir stats then `ok else `not_a_dir)
  | `unknown_error err -> return (`failed err)
  | `does_not_exist ->
    (*Message.message "mkdir: %s" (Path.to_string dir);*)
    Effort.track mkdir_counter (fun () ->
      try_with (fun () ->
        Unix.mkdir ~p:() (Path.to_absolute_string dir)
      )
    ) >>= fun res ->
    match res with
    | Ok () -> return `ok
    | Error exn -> return (`failed (Error.of_exn exn))

(*----------------------------------------------------------------------
 Ocaml_digest (count!)
----------------------------------------------------------------------*)

module Ocaml_digest : sig
  val init: Config.t -> unit
  val of_file : Path.t -> string Or_error.t Deferred.t
end = struct
  let throttle = ref None
  let set max_concurrent_jobs =
    throttle := Some (Throttle.create ~continue_on_error:true ~max_concurrent_jobs)
  ;;
  let init config =
    match !throttle with
    | Some _ -> failwith "Fs.Ocaml_digest.init called more than once"
    | None -> set (Config.d_number config)

  external digest_fd : int -> string = "caml_md5_fd"

  let of_file path =
    Throttle.enqueue (Option.value_exn !throttle) (fun () ->
      File_access.enqueue (fun () ->
        Effort.track digest_counter (fun () ->
          Deferred.Or_error.try_with (fun () ->
            Unix.with_file (Path.to_absolute_string path) ~mode:[`Rdonly]
              ~f:(fun fd ->
                Fd.with_file_descr_deferred_exn fd (fun fd ->
                  let fd = Core.Std.Unix.File_descr.to_int fd in
                  In_thread.run (fun () -> digest_fd fd))))
          |> Deferred.Or_error.map ~f:Caml.Digest.to_hex)))

  TEST_UNIT =
    set 20;
    let open Core.Std in
    let cwd = Sys.getcwd () in
    let file = Filename.concat cwd (Filename.basename _here_.pos_fname) in
    let our_digest =
      Thread_safe.block_on_async_exn (fun () -> of_file (Path.absolute file) >>| ok_exn)
    in
    let actual_digest = Caml.Digest.to_hex (Caml.Digest.file file) in
    <:test_result< string >> our_digest ~expect:actual_digest;
    <:test_pred< string Or_error.t >> Result.is_error
      (Thread_safe.block_on_async_exn (fun () -> of_file (Path.absolute cwd)));
  ;;

end

(*----------------------------------------------------------------------
  Listing (count!)
----------------------------------------------------------------------*)

let run_ls ~dir =
  let open Listing in
  Locking.lock_directory_for_listing ~dir (fun () ->
    let path_string = Path.to_string dir in
    File_access.enqueue (fun () ->
      Effort.track ls_counter (fun () ->
        try_with (fun () -> Unix.opendir path_string) >>= function
        | Error exn -> return (Error (Error.of_exn exn)) (* opendir failed *)
        | Ok dir_handle ->
            (* opendir succeeded, we must be sure to close *)
          let close () =
            try_with (fun () -> Unix.closedir dir_handle) >>| function | Ok () -> ()
            | Error exn ->
              Message.error "Unix.closedir failed: %s\n%s" path_string (Exn.to_string exn)
          in
            (* catch all exns while processing so we can close in every case *)
          try_with (fun () ->
            let rec loop acc =
              try_with (fun () -> Unix.readdir dir_handle) >>= function
              | Ok "." -> loop acc
              | Ok ".." -> loop acc
              | Ok base ->
                begin
                  let path_string = Path.to_string dir ^ "/" ^ base in
                  unix_stat ~follow_symlinks:false path_string >>= function
                  | Error _e ->
                      (* File disappeared between readdir & lstat system calls.
                         Handle as if readdir never told as about it *)
                    loop acc
                  | Ok u ->
                    let kind = u.Unix.Stats.kind in
                    loop (Elem.create ~base ~kind :: acc)
                end
              | Error exn ->
                match Monitor.extract_exn exn with
                | End_of_file ->
                    (* no more filenames - normal; we are finished listing *)
                  return (create ~dir ~elems:acc)
                | exn ->
                    (* some other unexpected error; raise to outer handler *)
                  raise exn
            in
            loop []

          ) >>= fun res ->
            (* convert exn -> Or_error *)
          let ore =
            match res with
            | Error exn -> Error (Error.of_exn exn)
            | Ok x -> Ok x
          in
            (* close in every case *)
          close () >>= fun () ->
          return ore
      )))

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
    let path =
      Path.of_absolute_string (
        Path.to_absolute_string path)
    in
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
  val create : unit -> t

  val clear_watcher_cache : t -> Path.t -> unit

  val dont_watch_file_or_dir : t ->
    how:[`via_parent|`directly] ->
    Path.t ->
    Heart.t

end = struct

  type t = {
    watching_via_parent : Glass.t Path.Table.t;
    watching_directly : Glass.t Path.Table.t;
  }

  let create () =
    let watching_via_parent = Path.Table.create () in
    let watching_directly = Path.Table.create () in
    let t = {watching_via_parent; watching_directly} in
    t

  let clear_watcher_cache t path =
    break_cache_glass t.watching_via_parent path;
    break_cache_glass t.watching_directly (Path.dirname path)

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
    t Deferred.t

  val watch_file_or_dir : t ->
    how:[`via_parent|`directly] ->
    Path.t ->
    Heart.t Or_error.t Deferred.t

  val clear_watcher_cache : t -> Path.t -> unit

end = struct

  type t =
  | Real of Real_watcher.t
  | Blind of Blind_watcher.t

  let create ~nono ~ignore ~expect =
    if nono
    then return (Blind (Blind_watcher.create ()))
    else Real_watcher.create ~ignore ~expect >>| fun w -> Real w

  let watch_file_or_dir t ~how path =
    match t with
    | Real w -> Real_watcher.watch_file_or_dir w ~how path
    | Blind w -> return (Ok (Blind_watcher.dont_watch_file_or_dir w ~how path))

  (* [clear_watcher_cache] - clears the cache to ensure the file will be stated
     again. Necessary when running without notifiers; sensible even with notifiers, to
     avoid relying on the delivery of inotify events. *)
  let clear_watcher_cache t path =
    match t with
    | Real w -> Real_watcher.clear_watcher_cache w path
    | Blind w-> Blind_watcher.clear_watcher_cache w path

end

module Listing_result = struct
  type t = [
    | `does_not_exist
    | `not_a_dir
    | `listing of Listing.t
  ] with compare, sexp

  include Comparable.Make(struct
    type nonrec t = t with compare, sexp
  end)
end

(** Stats for a path, as comes from [lstat]. *)
module Lstat_result = struct
  type t = [
  | `does_not_exist
  | `stats of Stats.t
  ] with compare, sexp

  include Comparable.Make(struct
    type nonrec t = t with compare, sexp
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
  ] with compare, sexp

  include Comparable.Make(struct
    type nonrec t = t with compare, sexp
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
  with fields

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
                (sprintf "%S's parent directory exists, but watcher set up failed"
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
              unbreakable (Error (
                Error.tag err
                  (sprintf "%S exists, but watcher set up failed"
                     (Path.to_string path))))
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
            Unix.readlink (Path.to_absolute_string path)
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
      let r = lazy (memoize_reified Path.hashable uncached_lstat) in
      fun x -> Lazy.force r x
    and stat =
      let r = lazy (memoize_reified Path.hashable uncached_stat) in
      fun x -> Lazy.force r x
    and inode =
      let r = lazy (memoize_reified Path.hashable uncached_inode) in
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
      memoize_reified Path.hashable uncached_list_dir
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
          Reader.file_contents (Path.to_absolute_string file)
        )
      )
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
          | Ok digest_string ->
            let digest = Digest.intern digest_string in
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
    memoize_reified_with_cache t.cache ~key:file (fun file -> contents_file sm ~file)

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
    memoize_reified_with_cache t.cache ~key:file (fun file ->
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
      } with sexp, bin_io, compare
      let hash = Hashtbl.hash
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
    memoize_reified_with_cache t.cache ~key:glob (fun glob ->
      Glob.exec glob fsm
    )

  let create () = {
    cache = Glob.Table.create ();
  }

end

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

end

(*----------------------------------------------------------------------
 Fs - combination of persistent & dynamic caches
----------------------------------------------------------------------*)

let tmp_jenga =
  let user = Core.Std.Unix.getlogin() in
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

  val clear_watcher_cache : t -> Path.t -> unit

  val mtime_file : t -> file:Path.t -> Mtime.t option Tenacious.t

end = struct

  type t = {
    nono : bool;
    watcher : Watcher.t;
    memo : Memo.t;
    persist : Persist.t;
  } with fields

  let create config persist =
    let nono = not System.has_inotify || Config.no_notifiers config in
    let ignore = Special_paths.Dot_jenga.matches in
    let expect path =
      match Path.case path with
      | `relative rel -> Locking.is_action_running_for_path rel
      | `absolute abs -> String.is_prefix ~prefix:tmp_jenga (Path.Abs.to_string abs)
    in
    Watcher.create ~nono ~ignore ~expect >>= fun watcher ->
    let memo = Memo.create watcher in
    let t = { nono; watcher; memo; persist; } in
    return t

  let contents_file t ~file = Memo.contents_file t.memo ~file

  let digest_file t ~file = Memo.digest_file t.memo t.persist ~file

  let list_glob t glob = Memo.list_glob t.memo glob

  let watch_sync_file t path =
    Watcher.watch_file_or_dir t.watcher ~how:`via_parent path
    >>| ok_exn

  let clear_watcher_cache t path =
    Watcher.clear_watcher_cache t.watcher path

  let mtime_file t ~file = Memo.mtime_file t.memo ~file

end


include Fs

let ensure_directory (_:t) ~dir = (* why need t ? *)
  Tenacious.lift (fun () ->
    ensure_directory ~dir >>| unbreakable (* ?? *)
  )

let mtime_file_right_now ~file =
  stat ~follow_symlinks:true file >>| function
  | `ok stats -> Some (Stats.mtime stats)
  | `does_not_exist | `unknown_error _ -> None


(*----------------------------------------------------------------------
 syncronize for delivery of inotify events when finish running an action
----------------------------------------------------------------------*)

let pid_string = Pid.to_string (Unix.getpid ())
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
    Unix.mkdir ~p:() tmp_jenga
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
          Unix.with_file path_string ~mode:[ `Wronly; `Creat ]
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
