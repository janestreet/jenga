
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let equal_using_compare compare = fun x1 x2 -> Int.(=) 0 (compare x1 x2)
let equal_pair f g = fun (a1,b1) (a2,b2) -> f a1 a2 && g b1 b2

let lstat_counter = Effort.Counter.create "stat"
let digest_counter = Effort.Counter.create "digest"
let ls_counter = Effort.Counter.create "ls"
let mkdir_counter = Effort.Counter.create "mkdir"


(* Naming...
   memo - dynamic cache of computations
   persist - persistent cache of values
*)

module External_digest = Digest

let ( *>>= ) = Tenacious.bind

let do_trace =
  match Core.Std.Sys.getenv "JENGA_TRACE_FS" with
  | None -> false
  | Some _ -> true

let unlogged =
  if do_trace then
    fun fmt ->
      ksprintf (fun string ->
        Message.unlogged "Fs: %s" string
      ) fmt
  else
    fun fmt ->
      ksprintf (fun _string ->
        ()
      ) fmt

(*----------------------------------------------------------------------
 Kind
----------------------------------------------------------------------*)

module Kind = struct
  type t =
    [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  with sexp, bin_io, compare
  let to_string t = Sexp.to_string (sexp_of_t t)
end


(*----------------------------------------------------------------------
 System lstat - counted
----------------------------------------------------------------------*)

let unix_lstat path =
  Effort.track lstat_counter (fun () ->
    Unix.lstat (Path.to_absolute_string path)
  )

let unix_lstat_kind path =
  unix_lstat path >>| fun u -> u.Unix.Stats.kind


(*----------------------------------------------------------------------
  Reduced Stat info
----------------------------------------------------------------------*)

module Stats : sig

  type t with sexp, bin_io
  val lstat : Path.t -> t Or_error.t Deferred.t
  val equal : t -> t -> bool
  val kind : t -> Kind.t

end = struct

  (* should contain enough info from the Unix.Stats.t to reliably indicate that
     the path must be re-digested because it might have changed.

     we should like this structure to be small, as we will store lots of them

     we're happy to accept that sometimes a change after the stat, we will have to
     redigest a file only to get the same digest

     what we dont want is for the chance that the stat info remains the same, BUT
     if we were to re-digest we would get different contents.

     This is possible in theory - i.e. to keep the same inode & mtime
     but have different content.
     But this is pretty unusual (how exactly can it happen?)

     And we are happy to not regard these as changes which will trigger a rebuild.

     Some of the info in this stat structure is not for the it-might-have-changed
     detection -- i.e. the kind -- but we need this to know how to treat the path
     just statted.
  *)

  type t = {
    (* can we do without dev & size ?? *)
    dev : int;
    ino : int;
    kind : Kind.t;
    size : int64;
    mtime : float;
  } with sexp, bin_io, compare

  let equal = equal_using_compare compare

  let kind t = t.kind

  let of_unix_stats u =
    let module U = Unix.Stats in
    {
      dev = U.dev u;
      ino = U.ino u;
      kind = U.kind u;
      size = U.size u;
      mtime = Time.to_float (U.mtime u);
    }

  let lstat path =
    try_with (fun () ->
      unix_lstat path >>| fun u -> of_unix_stats u
    ) >>= fun res ->
    match res with
    | Ok x -> return (Ok x)
    | Error exn -> return (Error (Error.of_exn exn))

end

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
  Stats.lstat dir >>= function
  | Ok stats -> return (if is_dir stats then `ok else `not_a_dir)
  | Error _e ->
    (*Message.message "mkdir: %s" (Path.to_rrr_string dir);*)
    Effort.track mkdir_counter (fun () ->
      try_with (fun () ->
        Unix.mkdir ~p:() (Path.to_absolute_string dir)
      )
    ) >>= fun res ->
    match res with
    | Ok () -> return `ok
    | Error _ -> return `failed

(*----------------------------------------------------------------------
 Compute_digest (count!)
----------------------------------------------------------------------*)

module Digest : sig  (* proxy for the contents of a file in the file-system *)

  type t with sexp, bin_io, compare
  val of_string : string -> t
  val equal : t -> t -> bool

end = struct

  type t = string with sexp, bin_io
  let of_string x = x
  let compare = String.compare
  let equal = String.equal

end

module Compute_digest : sig

  val of_file : Path.t -> Digest.t Or_error.t Deferred.t

end = struct

  let of_file path =
    Effort.track digest_counter (fun () ->
      unlogged "DIGEST: %s..." (Path.to_absolute_string path);
      In_thread.run (fun () ->
        try (
          let ic = Caml.open_in_bin (Path.to_absolute_string path) in
          let res =
            try (
              let d = External_digest.channel ic (-1) in
              let res = External_digest.to_hex d in
              unlogged "DIGEST: %s... %s" (Path.to_absolute_string path) res;
              Ok (Digest.of_string res)
            )
            with exn -> Error (Error.of_exn exn)
          in
          Caml.close_in ic;
          res
        )
        with exn -> Error (Error.of_exn exn)
      )
    )

end


(*----------------------------------------------------------------------
  Listing (count!)
----------------------------------------------------------------------*)

module Listing : sig

  type t with sexp, bin_io, compare

  val run_ls : dir:Path.t -> t Or_error.t Deferred.t

  val paths_and_kinds : t -> (Path.t * Kind.t) list
  val paths : t -> Path.t list

  val equal : t -> t -> bool

  module Restriction : sig
    type t with sexp, bin_io
    val create : kinds:Kind.t list option -> glob_string:string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

  val restrict : t -> Restriction.t -> t

end = struct

  type t = {
    listing : (Path.t * Kind.t) list
  } with sexp, bin_io, compare

  let run_ls ~dir =
    Effort.track ls_counter (fun () ->
     (*Message.message "LS: %s..." (Path.to_absolute_string dir);*)
      try_with (fun () ->
        let path_string = Path.to_absolute_string dir in
        Unix.opendir path_string >>= fun dir_handle ->
        let close () =
          don't_wait_for (
            try_with (fun () -> Unix.closedir dir_handle) >>| function | Ok () -> ()
            | Error exn ->
              Message.error "Unix.closedir failed: %s\n%s" path_string (Exn.to_string exn)
          )
        in
        let rec loop acc =
          try_with (fun () -> Unix.readdir dir_handle) >>= function
          | Ok "." -> loop acc
          | Ok ".." -> loop acc
          | Ok string ->
            let path = Path.relative ~dir string in
            unix_lstat_kind path >>= fun kind ->
            loop ((path,kind) :: acc)
          | Error exn ->
            match Monitor.extract_exn exn with
            | End_of_file -> close(); return acc
            | exn -> close(); raise exn
        in
        loop []
      )
      >>= function
      | Error exn -> return (Error (Error.of_exn exn))
      | Ok listing -> return (Ok { listing })
    )

  let paths t = List.map ~f:fst t.listing
  let paths_and_kinds t = t.listing

  let equal = equal_using_compare compare

  module Restriction = struct

    type t = {
      kinds : Kind.t list option; (* None means any kind *)
      pat : Pattern.t;
    } with sexp, bin_io, compare

    let create ~kinds ~glob_string =
      let pat = Pattern.create_from_glob_string glob_string in
      { kinds; pat; }

    let to_string t =
      sprintf "%s %s"
        (match t.kinds with
        | None -> "(any-kind)"
        | Some kinds ->
          sprintf "(%s)" (String.concat ~sep:"," (List.map kinds ~f:Kind.to_string)))
        (Pattern.to_string t.pat)


  end

  let restrict t r =
    let match_kind =
      match r.Restriction.kinds with
      | None -> (fun _ -> true)
      | Some kinds -> List.mem kinds
    in
    {
      listing =
        List.filter t.listing ~f:(fun (path,kind) ->
          match_kind kind
          && Pattern.matches r.Restriction.pat (Path.basename path)
        )
    }

end

(*----------------------------------------------------------------------
 Watcher (inotify wrapper) - works on absolute path strings
----------------------------------------------------------------------*)

module Watcher : sig

  type t
  val create :
    (* ignore events completely for these paths *)
    ignore:(path:string -> bool) ->
    (* pay attention to, but dont report file_changes for these events *)
    expect:(path:string -> bool) ->
    t Deferred.t

  val watch_file_or_dir : t ->
    what:[`file|`dir] ->
    path:string ->
    desc:string ->
    Heart.t Or_error.t Deferred.t

end = struct

  module Inotify = Async_inotify

  type what = [`file|`dir]

  type t = {
    ignore : (path:string -> bool);
    expect : (path:string -> bool);
    notifier : Inotify.t ;
    glass_hearts : (what * Heart.Glass.t) String.Table.t;
  }

  let paths_of_event t =
    let paths path = (* for the filename & the dir in which it resides *)
      if (t.ignore ~path) then [] else
        let dir = Filename.dirname path in
        [path; dir]
    in
    function
    | Inotify.Event.Modified s -> [s] (* just the filename *)
    | Inotify.Event.Unlinked s -> paths s
    | Inotify.Event.Created s -> paths s
    | Inotify.Event.Moved m ->
      match m with
      | Inotify.Event.Away s -> paths s
      | Inotify.Event.Into s -> paths s
      | Inotify.Event.Move (s1,s2) -> paths s1 @ paths s2

  let suck_notifier_pipe t piper =
    don't_wait_for (
      Pipe.iter piper ~f:(fun event ->
        (* mustn't log the watcher event, because writing to the log
           file will cause a new event !! *)
        unlogged "Watcher, event: %s" (Inotify.Event.to_string event);
        List.iter (paths_of_event t event) ~f:(fun path ->
          unlogged "Watcher, event/path: %s" path;
          match (Hashtbl.find t.glass_hearts path) with
          | None -> ()
          | Some (what,glass) ->
            assert (not (Heart.Glass.is_broken glass));
            begin
              match what with
              | `dir -> ()
              | `file ->
                (*
                  Show path events to which we are sensitized...

                  Don't show events for targets of currently running actions.

                  Will see events for externally changed files (i.e. edited source files
                  or removed generated files), but also for paths affected by a running
                  jenga action which are not declared as targets of that action.

                *)
                if not (t.expect ~path) then (
                  Message.file_changed ~desc:(Heart.Glass.desc glass)
                )
            end;
            Heart.Glass.break glass;
            Hashtbl.remove t.glass_hearts path;
        );
        return ()
      )
    )

  let create ~ignore ~expect =
    (* Have to pass a path at creation - what a pain, dont have one yet! *)
    Inotify.create ~recursive:false ~watch_new_dirs:false "/" >>= fun (notifier,_) ->
    let glass_hearts = String.Table.create () in
    let t = {ignore; expect; notifier; glass_hearts} in
    suck_notifier_pipe t (Inotify.pipe notifier);
    return t

  let watch_file_or_dir t ~what ~path ~desc =
    let dir =
      (* A notifier is setup to watch [dir], computed from [path] & [what]
         The underlying Inotify module handles repeated setup for same path.
         So we make no attempt to avoid the repeats.
      *)
      match what with
      | `file -> Filename.dirname path
      | `dir -> path
    in
    unlogged "watch: %s (dir: %s)" path dir;
    match (Hashtbl.find t.glass_hearts path) with
    | Some (_what,glass) ->
      assert (not (Heart.Glass.is_broken glass));
      return (Ok (Heart.of_glass glass))
    | None ->
      let absolute_dir = dir in (* do we need absolute dir ??? *)
      unlogged "setup watcher: %s" absolute_dir;
      Monitor.try_with (fun () ->
        Inotify.add t.notifier absolute_dir
      ) >>= function
      | Error exn ->
        Message.error "Unable to watch path: %s" absolute_dir;
        return (Error (Error.of_exn exn))
      | Ok () ->
        let glass = Heart.Glass.create ~desc in
        Hashtbl.add_exn t.glass_hearts ~key:path ~data:(what,glass);
        return (Ok (Heart.of_glass glass))


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
    match (Hashtbl.find cache path) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious =
        Tenacious.lift (fun () ->
          Watcher.watch_file_or_dir t.watcher ~what
            ~path:(Path.to_absolute_string path)
            ~desc:(Path.to_rrr_string path)
          >>= function
          | Error exn ->
            return (Error exn, Heart.unbreakable)
          | Ok heart ->
            Stats.lstat path >>= fun res ->
            return (res,heart)
        )
      in
      Hashtbl.add_exn cache ~key:path ~data:tenacious;
      tenacious

end

(*----------------------------------------------------------------------
 Digest/Listing result types
----------------------------------------------------------------------*)

module Digest_result = struct
  type t = [
  | `stat_error of Error.t
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
 Digest_persist - w.r.t stat->digest mapping
----------------------------------------------------------------------*)

module Digest_persist : sig

  type t with sexp, bin_io

  val create : unit -> t
  val equal : t -> t -> bool
  val copy : t -> t

  val digest_file :
    t ->
    Stat_memo.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

end = struct

  type t = {
    cache : (Stats.t * Digest.t) Path.Table.t;
  } with sexp, bin_io

  let create () = {
    cache = Path.Table.create ();
  }

  let equal t1 t2 =
    Hashtbl.equal t1.cache t2.cache
      (equal_pair Stats.equal Digest.equal)

  let copy t = {
    cache = Path.Table.copy t.cache;
  }

  let digest_file t sm ~file =
    let remove() = Hashtbl.remove t.cache file in
    Stat_memo.lstat sm ~what:`file file *>>= function
    | Error e -> (remove(); Tenacious.return (`stat_error e))
    | Ok stats ->
      let kind = Stats.kind stats in
      if not (is_digestable stats) then Tenacious.return (`undigestable kind) else
        match (
          match (Hashtbl.find t.cache file) with
          | None -> None
          | Some (prev_stats,prev_proxy) ->
            if Stats.equal stats prev_stats
            then Some prev_proxy
            else None
        ) with
        | Some old_good_digest -> Tenacious.return (`digest old_good_digest)
        | None ->
          Tenacious.lift (fun () ->
            Compute_digest.of_file file >>| fun digest -> (digest,Heart.unbreakable)
          ) *>>= function
          | Error e -> (remove(); Tenacious.return (`digest_error e))
          | Ok new_digest ->
            Hashtbl.set t.cache ~key:file ~data:(stats,new_digest);
            Tenacious.return (`digest new_digest)

end


(*----------------------------------------------------------------------
 Listing_persist - w.r.t stat->listing mapping
----------------------------------------------------------------------*)

module Listing_persist : sig

  type t with sexp, bin_io

  val create : unit -> t
  val equal : t -> t -> bool
  val copy : t -> t

  val list_dir :
    t ->
    Stat_memo.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t


end = struct

  type t = {
    cache : (Stats.t * Listing.t) Path.Table.t;
  } with sexp, bin_io

  let create () = {
    cache = Path.Table.create ();
  }

  let equal t1 t2 =
    Hashtbl.equal t1.cache t2.cache
      (equal_pair Stats.equal Listing.equal)

  let copy t = {
    cache = Path.Table.copy t.cache;
  }

  let list_dir t sm ~dir =
    let remove() = Hashtbl.remove t.cache dir in
    Stat_memo.lstat sm ~what:`dir dir *>>= function
    | Error e -> (remove(); Tenacious.return (`stat_error e))
    | Ok stats ->
      if not (is_dir stats) then Tenacious.return `not_a_dir else
        match (
          match (Hashtbl.find t.cache dir) with
          | None -> None
          | Some (prev_stats,prev_proxy) ->
            if Stats.equal stats prev_stats
            then Some prev_proxy
            else None
        ) with
        | Some old_good_listing -> Tenacious.return (`listing old_good_listing)
        | None ->
          Tenacious.lift (fun () ->
            Listing.run_ls ~dir >>| fun listing -> (listing,Heart.unbreakable)
          ) *>>= function
          | Error e -> (remove(); Tenacious.return (`listing_error e))
          | Ok new_listing ->
            Hashtbl.set t.cache ~key:dir ~data:(stats,new_listing);
            Tenacious.return (`listing new_listing)


end

(*----------------------------------------------------------------------
Persist - combination of Digest & Listing persists
----------------------------------------------------------------------*)

module Persist : sig

  type t with sexp, bin_io
  val create : unit -> t
  val equal : t -> t -> bool
  val copy : t -> t

  val digest_file :
    t ->
    Stat_memo.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

  val list_dir :
    t ->
    Stat_memo.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  type t = {
    digests : Digest_persist.t;
    listings : Listing_persist.t;
  } with sexp, bin_io

  let create () = {
    digests = Digest_persist.create();
    listings = Listing_persist.create();
  }

  let equal t1 t2 =
    Digest_persist.equal t1.digests t2.digests
    && Listing_persist.equal t1.listings t2.listings

  let copy t = {
    digests = Digest_persist.copy t.digests;
    listings = Listing_persist.copy t.listings;
  }

  let digest_file t sm ~file = Digest_persist.digest_file t.digests sm ~file

  let list_dir t sm ~dir = Listing_persist.list_dir t.listings sm ~dir

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
    match (Hashtbl.find t.cache file) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious = Persist.digest_file dp sm ~file in
      Hashtbl.add_exn t.cache ~key:file ~data:tenacious;
      tenacious

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
    Persist.t ->
    Stat_memo.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  type computation = Listing_result.t Tenacious.t
  type t = {
    cache : computation Path.Table.t;
  }

  let list_dir t dp sm ~dir =
    match (Hashtbl.find t.cache dir) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious = Persist.list_dir dp sm ~dir in
      Hashtbl.add_exn t.cache ~key:dir ~data:tenacious;
      tenacious

  let create () = {
    cache = Path.Table.create ();
  }

end


(*----------------------------------------------------------------------
 Glob
----------------------------------------------------------------------*)

module Glob : sig

  type t with sexp, bin_io
  include Hashable with type t := t

  val create : dir:Path.t -> kinds: Kind.t list option -> glob_string:string -> t
  val to_string : t -> string
  val compare : t -> t -> int

  val exec : t ->
    Listing_memo.t ->
    Persist.t ->
    Stat_memo.t ->
    Listing_result.t Tenacious.t

end = struct

  module Key = struct
    module T = struct
      type t = {
        dir : Path.t;
        glob_string : string;
        kinds : Kind.t list option;
      } with sexp, bin_io, compare
      let hash = Hashtbl.hash
    end
    include T
    include Hashable.Make(T)
  end

  module T = struct
    type t = {
      dir : Path.t;
      restriction : Listing.Restriction.t;
    } with sexp, bin_io, compare
    let hash = Hashtbl.hash
  end

  include T
  include Hashable.Make(T)

  let to_string t =
    sprintf "glob: %s/ %s"
      (Path.to_rrr_string t.dir)
      (Listing.Restriction.to_string t.restriction)

  let raw_create ~dir ~kinds ~glob_string =
    let restriction = Listing.Restriction.create ~kinds ~glob_string in
    let t = { dir ; restriction } in
    (*Message.message "Glob.create: %s" (to_string t);*)
    t

  (* cache glob construction *)
  let the_cache : (Key.t, t) Hashtbl.t = Key.Table.create()

  let create ~dir ~kinds ~glob_string =
    let key = {Key. dir; kinds; glob_string} in
    match (Hashtbl.find the_cache key) with
    | Some glob -> glob
    | None ->
      let glob = raw_create ~dir ~kinds ~glob_string in
      Hashtbl.add_exn the_cache ~key ~data:glob;
      glob


  let exec_no_cutoff glob lm per sm  =
    Listing_memo.list_dir lm per sm ~dir:glob.dir *>>= function
    | `listing listing ->
      let restricted = Listing.restrict listing glob.restriction in
      Tenacious.return (`listing restricted)
    | x ->
      Tenacious.return x

  let exec glob lm per sm =
    Tenacious.lift (fun () ->
      let my_glass =
        Heart.Glass.create ~desc:(to_string glob)
      in
      Tenacious.exec (exec_no_cutoff glob lm per sm) >>= fun (res,heart) ->
      don't_wait_for (
        let rec loop heart =
          Heart.when_broken heart >>= fun () ->
          Tenacious.exec (exec_no_cutoff glob lm per sm) >>= fun (res2,heart) ->
          if (Listing_result.equal res res2)
          then loop heart
          else (
            Message.file_changed ~desc:(to_string glob);
            Heart.Glass.break my_glass;
            Deferred.return ()
          )
        in
        loop heart
      );
      let my_heart = Heart.of_glass my_glass in
      Deferred.return (res, my_heart)
    )

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
    Persist.t ->
    Stat_memo.t ->
    Glob.t ->
    Listing_result.t Tenacious.t

end = struct

  type computation = Listing_result.t Tenacious.t
  type t = {
    cache : computation Glob.Table.t;
  }

  let list_glob t lm per sm glob =
    match (Hashtbl.find t.cache glob) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious = Glob.exec glob lm per sm in
      Hashtbl.add_exn t.cache ~key:glob ~data:tenacious;
      tenacious

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

  val digest_file :
    t ->
    Persist.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

  val list_glob :
    t ->
    Persist.t ->
    Glob.t ->
    Listing_result.t Tenacious.t

end = struct

  type t = {
    sm : Stat_memo.t;
    dm : Digest_memo.t;
    lm : Listing_memo.t;
    gm : Glob_memo.t;
  }

  let create watcher = {
    sm = Stat_memo.create watcher;
    dm = Digest_memo.create();
    lm = Listing_memo.create();
    gm = Glob_memo.create();
  }

  let digest_file t per ~file = Digest_memo.digest_file t.dm per t.sm ~file

  let list_glob t per glob = Glob_memo.list_glob t.gm t.lm per t.sm glob

end

(*----------------------------------------------------------------------
 Fs - combination of persistent & dynamic caches
----------------------------------------------------------------------*)

module Fs : sig

  type t
  val create : Persist.t -> t Deferred.t

  val digest_file :
    t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

  val list_glob :
    t ->
    Glob.t ->
    Listing_result.t Tenacious.t

  val active_targets : t -> unit Tenacious.t Path.Table.t

  val watch_sync_file : t -> path:string -> Heart.t Deferred.t

end = struct

  type t = {
    watcher : Watcher.t;
    active_targets : unit Tenacious.t Path.Table.t;
    memo : Memo.t;
    persist : Persist.t;
  }

  let create persist =
    let active_targets = Path.Table.create () in

    let ignore ~path =
      match (Path.create_from_absolute path) with
      | None -> false
      | Some path -> Path.is_special_jenga_path path
    in
    let expect ~path =
      String.is_prefix ~prefix:"/tmp" path
      (* to avoid reporting as changed jenga-sync files
         Does it matter that we ignore everything in /tmp? - Dont think so.
         At the moment, the only places we watch apart from /tmp are repo paths.
      *)
      ||
      match (Path.create_from_absolute path) with
      | None -> false
      | Some path -> Hashtbl.mem active_targets path
    in

    Watcher.create ~ignore ~expect >>= fun watcher ->
    let memo = Memo.create watcher in
    let t = { watcher; active_targets; memo; persist; } in
    return t

  let digest_file t ~file = Memo.digest_file t.memo t.persist ~file

  let list_glob t glob = Memo.list_glob t.memo t.persist glob

  let active_targets t = t.active_targets

  let watch_sync_file t ~path =
    Watcher.watch_file_or_dir t.watcher ~what:`file ~path ~desc:path
    >>= function
    | Ok heart -> return heart
    | Error e -> raise (Error.to_exn e)

end


include Fs

let ensure_directory (_:t) ~dir = (* why need t ? *)
  Tenacious.lift (fun () ->
    ensure_directory ~dir >>= fun res ->
    return (res,Heart.unbreakable) (* ?? *)
  )

(*----------------------------------------------------------------------
 syncronize for delivery of inotify events when finish running an action
----------------------------------------------------------------------*)

let pid_string = Pid.to_string (Unix.getpid ())
let genU1 = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u)

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

let sync_inotify_delivery
    : (t -> sync_contents:string -> 'a Tenacious.t -> 'a Tenacious.t) =

  (* After a tenacious is run, synchronise until all inotify events triggered while
     running have been delivered (and acted upon) by this process

     Do this by causing and waiting for a specific file-events:
     - the creation & removal of a temp file especially created for the purpose.

     As a side benefit, the contents of the temp file will indicate to the external world
     exactly which of those tenacious values synchronised sync are running at any moment.

     The temp path chosen has 3 parameters:
     - the pid of the jenga instance
     - a counter (unique to the pid), incremented for each synced tenacious
     - a counter (unique to a synced tenacious), incremented for each rerun.
  *)
  fun t ~sync_contents tenacious ->
    let u1 = genU1 () in
    let genU2 = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
    Tenacious.lift (fun () ->
      let u2 = genU2 () in
      let path = sprintf "/tmp/jenga-%s-%d-%d.sync" pid_string u1 u2 in

      (* Setup watcher for sync-file *)
      watch_sync_file t ~path >>= fun created_heart ->

      (* setup remove()... *)
      let remove () =
        (* esnure creation event has happenned.. *)
        Heart.when_broken created_heart >>= fun () ->
        (* Setup new watcher; unlink the file; wait for the event *)
        watch_sync_file t ~path >>= fun deleted_heart ->
        Sys.remove path >>= fun () ->
        Heart.when_broken deleted_heart >>= fun () ->
        return ()
      in

      (* create the sync-file *)
      let bag_elem = Bag.add created_but_not_deleted (path,remove) in

      unless_shutting_down ~f:(fun () ->
        Writer.save path ~contents:sync_contents
      ) >>= fun () ->

      (* run the tenacious *)
      Tenacious.exec tenacious >>= fun res ->

      (* remove the sync-file.. *)
      unless_shutting_down ~f:(fun () ->
        Bag.remove created_but_not_deleted bag_elem;
        remove ()
      ) >>= fun () ->

      (* Now we are synchronised! *)
      return res
    )
