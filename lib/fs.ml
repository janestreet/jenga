
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Heart = Tenacious.Heart
module Glass = Heart.Glass

let memoize ht ~key f =
  match (Hashtbl.find ht key) with
  | Some tenacious -> tenacious
  | None ->
    let tenacious = f () in
    let tenacious = Tenacious.reify tenacious in
    Hashtbl.add_exn ht ~key ~data:tenacious;
    tenacious

let unbreakable x = x,Heart.unbreakable

let lstat_counter = Effort.Counter.create "stat"
let digest_counter = Effort.Counter.create "digest"
let ls_counter = Effort.Counter.create "ls"
let mkdir_counter = Effort.Counter.create "mkdir"

(* Naming...
   memo - dynamic cache of computations
   persist - persistent cache of values
*)

module Ocaml_digest = Digest

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

let unix_lstat path =
  Effort.track lstat_counter (fun () ->
    try_with (fun () ->
      (*Message.message "stat: %s" path;*)
      Unix.lstat path
    )
  )

let lstat path =
  let path = Path.to_absolute_string path in
  unix_lstat path >>= function
  | Ok u -> return (Ok (Stats.of_unix_stats u))
  | Error exn -> return (Error (Error.of_exn exn))

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
  type t = [`ok | `failed | `not_a_dir]
end

let ensure_directory ~dir =
  lstat dir >>= function
  | Ok stats -> return (if is_dir stats then `ok else `not_a_dir)
  | Error _e ->
    (*Message.message "mkdir: %s" (Path.to_string dir);*)
    Effort.track mkdir_counter (fun () ->
      try_with (fun () ->
        Unix.mkdir ~p:() (Path.to_absolute_string dir)
      )
    ) >>= fun res ->
    match res with
    | Ok () -> return `ok
    | Error _ -> return `failed

(*----------------------------------------------------------------------
 Digester (throttle)
----------------------------------------------------------------------*)

module Digester : sig

  val init: Config.t -> unit
  val throttle : (unit -> 'a Deferred.t) -> 'a Deferred.t

end = struct

  type t = {
    (* Throttle for external digest jobs.
       Independent from user's -j job throttle. *)
    throttle : unit Throttle.t;
  }

  let create config =
    let throttle =
      let max_concurrent_jobs = Config.d_number config in
      Throttle.create ~continue_on_error:true ~max_concurrent_jobs
    in
    { throttle }

  let t_opt_ref = ref None

  let the_t () = match !t_opt_ref with | None -> assert false | Some t -> t

  let init config =
    match !t_opt_ref with
    | Some _ -> failwith "Fs.init called more than once"
    | None -> t_opt_ref := Some (create config)

  let throttle f =
    let t = the_t() in
    Throttle.enqueue t.throttle (fun () ->
      f () >>= fun res ->
      Deferred.return res
    )

end

(*----------------------------------------------------------------------
 Compute_digest (count!)
----------------------------------------------------------------------*)

module type Compute_digest_sig = sig

  val of_file : Path.t -> string Or_error.t Deferred.t

end

module Ocaml__Compute_digest : Compute_digest_sig = struct

  let of_file path =
    File_access.enqueue (fun () ->
      Effort.track digest_counter (fun () ->
        In_thread.run (fun () ->
          try (
            let ic = Caml.open_in_bin (Path.to_absolute_string path) in
            let res =
              try (
                let d = Ocaml_digest.channel ic (-1) in
                let res = Ocaml_digest.to_hex d in
                Ok res
              )
              with exn -> Error (Error.of_exn exn)
            in
            Caml.close_in ic;
            res
          )
          with exn -> Error (Error.of_exn exn)
        )
      )
    )

end

module External__Compute_digest(X : sig
  val prog : string
end) : Compute_digest_sig = struct

  let putenv = []
  let dir = Path.the_root
  let prog = X.prog

  let of_file path =
    Effort.track digest_counter (fun () ->
      (*Message.message "digest: %s" (Path.to_string path);*)
      let arg = Path.to_absolute_string path in
      let err s =
        let message = sprintf "%s %s : %s" prog arg s in
        return (Error (Error.of_string message))
      in
      let args = [arg] in
      let request = Forker.Request.create ~putenv ~dir ~prog ~args in
      Digester.throttle (fun () -> Forker.run request)
      >>= fun {Forker.Reply. stdout;stderr=_;outcome} ->
      match outcome with
      | `error s -> err s
      | `success ->
        match (String.lsplit2 stdout ~on:' ') with
        | None -> err (sprintf "unexpected output: `%s'" stdout)
        | Some (res,_) -> return (Ok res)
    )

end

let use_ocaml_digest =
  match Core.Std.Sys.getenv "JENGA_USE_OCAML_DIGEST" with
  | None -> false
  | Some _ -> true

let m =
  match
    match use_ocaml_digest  with
    | true -> None
    | false -> System.md5_program_path
  with
  | None ->
    Printf.eprintf "using internal ocaml digest\n%!";
    (module Ocaml__Compute_digest : Compute_digest_sig)
  | Some prog ->
    (module External__Compute_digest(struct let prog = prog end) : Compute_digest_sig)

module Compute_digest = (val m : Compute_digest_sig)

(*----------------------------------------------------------------------
  Listing (count!)
----------------------------------------------------------------------*)

let run_ls ~dir =
  let open Listing in
  File_access.enqueue (fun () ->
    let path_string = Path.to_string dir in
    Locking.lock_directory_for_listing ~dir (fun () ->
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
                  unix_lstat path_string >>= function
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
    if show then Message.file_changed ~desc:(Path.to_string path);
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
    what:[`file|`dir] ->
    Path.t ->
    Heart.t Or_error.t Deferred.t

  val clear_watcher_cache : t -> Path.t -> unit

end = struct

  module Inotify = Async_inotify

  type t = {
    ignore : (Path.t -> bool);
    expect : (Path.t -> bool);
    notifier : Inotify.t ;
    file_glass : Glass.t Path.Table.t;
    dir_glass : Glass.t Path.Table.t;
  }

  let paths_of_event t =
    let paths s = (* for the filename & the dir in which it resides *)
      let path = Path.of_absolute_string s in
      if (t.ignore path) then [] else
        let dir = Path.dirname path in
        [path; dir]
    in
    function
    | Inotify.Event.Queue_overflow ->
      Message.error "Inotify.Event.Queue_overflow"; [] (* at least log an error *)
    | Inotify.Event.Modified s -> [Path.of_absolute_string s] (* just the filename *)
    | Inotify.Event.Unlinked s -> paths s
    | Inotify.Event.Created s -> paths s
    | Inotify.Event.Moved m ->
      match m with
      | Inotify.Event.Away s -> paths s
      | Inotify.Event.Into s -> paths s
      | Inotify.Event.Move (s1,s2) -> paths s1 @ paths s2

  let clear_watcher_cache t path =
    break_cache_glass t.file_glass path;
    break_cache_glass t.dir_glass (Path.dirname path)

  let suck_notifier_pipe t piper =
    don't_wait_for (
      Pipe.iter_without_pushback piper ~f:(fun event ->
        (*Message.unlogged "Watcher, event: %s" (Inotify.Event.to_string event);*)
        List.iter (paths_of_event t event) ~f:(fun path ->
          (* Show path events to which we are sensitized...
             Don't show events for targets of currently running actions.
             Will see events for externally changed files (i.e. edited source files
             or removed generated files), but also for paths affected by a running
             jenga action which are not declared as targets of that action. *)
          let show = not (t.expect path) in
          break_cache_glass ~show t.file_glass path;
          break_cache_glass       t.dir_glass path (* why not show here? *)
        )))

  let create ~ignore ~expect =
    (* Have to pass a path at creation - what a pain, dont have one yet! *)
    Inotify.create ~recursive:false ~watch_new_dirs:false "/" >>= fun (notifier,_) ->
    let file_glass = Path.Table.create () in
    let dir_glass = Path.Table.create () in
    let t = {ignore; expect; notifier; file_glass; dir_glass} in
    suck_notifier_pipe t (Inotify.pipe notifier);
    return t

  let dir_and_cache t ~what path =
    match what with
    | `file -> Path.dirname path  , t.file_glass
    | `dir -> path                , t.dir_glass

  let watch_file_or_dir t ~what path =
    let path = Path.relativize_if_possible path in
    let dir, cache = dir_and_cache t ~what path in
    (*Message.unlogged "watch: %s (dir: %s)" (Path.to_string path) (Path.to_string dir);*)
    match (Hashtbl.find cache path) with
    | Some glass ->
      assert (not (Glass.is_broken glass));
      return (Ok (Heart.watch glass))
    | None ->
      (* A notifier is setup to watch [dir], computed from [path] & [what]
         The underlying Inotify module handles repeated setup for same path.
         So we make no attempt to avoid the repeats. *)
      let absolute_dir = Path.to_absolute_string dir in
      (*Message.unlogged "setup watcher: %s" absolute_dir;*)
      Monitor.try_with (fun () ->
        Inotify.add t.notifier absolute_dir
      ) >>| function
      | Error exn ->
        Message.error "Unable to watch path: %s" absolute_dir;
        Error (Error.of_exn exn)
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
    what:[`file|`dir] ->
    Path.t ->
    Heart.t

end = struct

  type t = {
    file_glass : Glass.t Path.Table.t;
    dir_glass : Glass.t Path.Table.t;
  }

  let create () =
    let file_glass = Path.Table.create () in
    let dir_glass = Path.Table.create () in
    let t = {file_glass; dir_glass} in
    t

  let clear_watcher_cache t path =
    break_cache_glass t.file_glass path;
    break_cache_glass t.dir_glass (Path.dirname path)

  let get_cache t ~what =
    match what with
    | `file -> t.file_glass
    | `dir -> t.dir_glass

  let dont_watch_file_or_dir t ~what path =
    let cache = get_cache t ~what in
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
    what:[`file|`dir] ->
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

  let watch_file_or_dir t ~what path =
    match t with
    | Real w -> Real_watcher.watch_file_or_dir w ~what path
    | Blind w -> return (Ok (Blind_watcher.dont_watch_file_or_dir w ~what path))

  (* [clear_watcher_cache] - clears the cache to ensure the file will be stated
     again. Necessary when running without notifiers; sensible even with notifiers, to
     avoid relying on the delivery of inotify events. *)
  let clear_watcher_cache t path =
    match t with
    | Real w -> Real_watcher.clear_watcher_cache w path
    | Blind w-> Blind_watcher.clear_watcher_cache w path

end

(*----------------------------------------------------------------------
 Stat_memo, making use of inotify watcher
----------------------------------------------------------------------*)

module Stat_memo : sig

  type t
  val create : Watcher.t -> t
  (* Caller must declare what the lstat is expected to be for,
     so that the correct kind of watcher can be set up *)
  val lstat : t -> what:[`file|`dir] -> Path.t -> Stats.t Or_error.t Tenacious.t

end = struct

  type computation = Stats.t Or_error.t Tenacious.t
  type t = {
    watcher : Watcher.t;
    file_watch_cache : computation Path.Table.t;
    dir_watch_cache : computation Path.Table.t;
  }

  let create watcher = {
    watcher;
    file_watch_cache = Path.Table.create ();
    dir_watch_cache = Path.Table.create ();
  }

  let lstat t ~what path =
    let cache =
      match what with
      | `file -> t.file_watch_cache
      | `dir -> t.dir_watch_cache
    in
    memoize cache ~key:path (fun () ->
      let tenacious =
        Tenacious.lift (fun () ->
          Watcher.watch_file_or_dir t.watcher ~what path
          >>= function
          | Error exn ->
            return (unbreakable (Error exn))
          | Ok heart ->
            lstat path >>| fun res -> (res,heart)
        )
      in
      tenacious
    )

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
  | `is_a_dir
  | `undigestable of Kind.t
  | `digest_error of Error.t
  | `digest of Digest.t
  ]
end

module Listing_result = struct
  type t = [
  | `stat_error of Error.t
  | `not_a_dir
  | `listing_error of Error.t
  | `listing of Listing.t
  ]

  let equal t1 t2 = (* must avoid comparing functional values *)
    match t1,t2 with
    | `not_a_dir,`not_a_dir -> true
    | `stat_error _, `stat_error _ -> true
    | `listing_error _, `listing_error _ -> true
    | `listing x1, `listing x2 -> Listing.equal x1 x2
    | _,_-> false

end

(*----------------------------------------------------------------------
 contents_file (without persistence)
----------------------------------------------------------------------*)

let contents_file sm ~file =
  Stat_memo.lstat sm ~what:`file file *>>= function
  | Error e -> Tenacious.return (`file_read_error e)
  | Ok __stats ->
    Tenacious.lift (fun () ->
      File_access.enqueue (fun () ->
        try_with (fun () ->
          Reader.file_contents (Path.to_absolute_string file)
        )
        >>| (function Ok x -> Ok x | Error exn -> Error (Error.of_exn exn))
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
    Stat_memo.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

end = struct

  let digest_file persist sm ~file =
    let db = Persist.db persist in
    let cache = Db.digest_cache db in
    let remove() = Hashtbl.remove cache file in
    Stat_memo.lstat sm ~what:`file file *>>= function
    | Error e -> (remove(); Tenacious.return (`stat_error e))
    | Ok stats ->
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
            Compute_digest.of_file file >>| unbreakable
          ) *>>= function
          | Error e -> (remove(); Tenacious.return (`digest_error e))
          | Ok digest_string ->
            let digest = Digest.intern digest_string in
            Hashtbl.set (Persist.modify "digest" cache) ~key:file ~data:(stats,digest);
            Tenacious.return (`digest digest)

end


(*----------------------------------------------------------------------
 Listing_persist - w.r.t stat->listing mapping
----------------------------------------------------------------------*)

module Listing_persist : sig

  val list_dir :
    Stat_memo.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  let list_dir sm ~dir =
    let tenacious =
      Stat_memo.lstat sm ~what:`dir dir *>>= function
      | Error e -> Tenacious.return (`stat_error e)
      | Ok stats ->
        if not (is_dir stats) then Tenacious.return `not_a_dir else
          Tenacious.lift (fun () ->
            run_ls ~dir >>| unbreakable
          ) *>>= function
          | Error e -> Tenacious.return (`listing_error e)
          | Ok new_listing -> Tenacious.return (`listing new_listing)
    in
    tenacious

end

(*----------------------------------------------------------------------
 Contents_memo
----------------------------------------------------------------------*)

module Contents_memo : sig

  type t
  val create : unit -> t

  val contents_file :
    t ->
    Stat_memo.t ->
    file:Path.t ->
    Contents_result.t Tenacious.t

end = struct

  type computation = Contents_result.t Tenacious.t
  type t = {
    cache : computation Path.Table.t;
  }

  let contents_file t sm ~file =
    memoize t.cache ~key:file (fun () ->
      contents_file sm ~file
    )

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
    Stat_memo.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

end = struct

  type computation = Digest_result.t Tenacious.t
  type t = {
    cache : computation Path.Table.t;
  }

  let digest_file t dp sm ~file =
    memoize t.cache ~key:file (fun () ->
      Digest_persist.digest_file dp sm ~file
    )

  let create () = {
    cache = Path.Table.create ();
  }

end

(*----------------------------------------------------------------------
 Listing_memo
----------------------------------------------------------------------*)

module Listing_memo : sig

  type t
  val create : unit -> t

  val list_dir :
    t ->
    Stat_memo.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  type computation = Listing_result.t Tenacious.t
  type t = {
    cache : computation Path.Table.t;
  }

  let list_dir t sm ~dir =
    memoize t.cache ~key:dir (fun () ->
      Listing_persist.list_dir sm ~dir
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

  let exec_no_cutoff glob lm sm  =
    Listing_memo.list_dir lm sm ~dir:(dir glob) *>>= function
    | `listing listing ->
      let restricted = Listing.restrict listing (restriction glob) in
      Tenacious.return (`listing restricted)
    | x ->
      Tenacious.return x

  let exec glob lm sm =
    Tenacious.cutoff
      ~equal:Listing_result.equal
      (exec_no_cutoff glob lm sm)

end

(*----------------------------------------------------------------------
 Glob_memo
----------------------------------------------------------------------*)

module Glob_memo : sig

  type t
  val create : unit -> t

  val list_glob :
    t ->
    Listing_memo.t ->
    Stat_memo.t ->
    Glob.t ->
    Listing_result.t Tenacious.t

end = struct

  type computation = Listing_result.t Tenacious.t
  type t = {
    cache : computation Glob.Table.t;
  }

  let list_glob t lm sm glob =
    memoize t.cache ~key:glob (fun () ->
      Glob.exec glob lm sm
    )

  let create () = {
    cache = Glob.Table.create ();
  }

end

(*----------------------------------------------------------------------
 Memo = combination of 4 memos - stat, digest, listing, glob
----------------------------------------------------------------------*)

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
    Listing_result.t Tenacious.t

  val mtime_file :
    t ->
    file:Path.t ->
    Mtime.t option Tenacious.t

end = struct

  type t = {
    sm : Stat_memo.t;
    cm : Contents_memo.t;
    dm : Digest_memo.t;
    lm : Listing_memo.t;
    gm : Glob_memo.t;
  }

  let create watcher = {
    sm = Stat_memo.create watcher ;
    cm = Contents_memo.create () ;
    dm = Digest_memo.create () ;
    lm = Listing_memo.create () ;
    gm = Glob_memo.create () ;
  }

  let contents_file t ~file = Contents_memo.contents_file t.cm t.sm ~file

  let digest_file t per ~file = Digest_memo.digest_file t.dm per t.sm ~file

  let list_glob t glob = Glob_memo.list_glob t.gm t.lm t.sm glob

  let mtime_file t ~file =
    Stat_memo.lstat t.sm ~what:`file file *>>| function
    | Error _ -> None
    | Ok stats -> Some (Stats.mtime stats)

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
    Listing_result.t Tenacious.t

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
    let ignore = Path.is_special_jenga_path in
    let expect path =
      match Path.case path with
      | `relative rel -> Locking.is_action_running_for_target rel
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
    Watcher.watch_file_or_dir t.watcher ~what:`file path
    >>= function
    | Ok heart -> return heart
    | Error e -> raise (Error.to_exn e)

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
  lstat file >>| function
  | Ok stats -> Some (Stats.mtime stats)
  | Error _ -> None


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
          Writer.save path_string ~contents:""
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

       This allows [fs.ml] to call [Locking.is_action_running_for_target] to avoid
       writing "changed" message for files targeted by this action. *)
    sync_inotify_delivery t >>| fun () ->
    res)
