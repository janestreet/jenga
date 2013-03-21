
open Core.Std
open Async.Std

let lstat_counter = Effort.Counter.create "lstat"
let digest_counter = Effort.Counter.create "digest"
let ls_counter = Effort.Counter.create "ls"


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

let trace =
  if do_trace then
    fun fmt ->
      ksprintf (fun string ->
        Message.trace "Fs: %s" string
      ) fmt
  else
    fun fmt ->
      ksprintf (fun _string ->
        ()
      ) fmt

module Kind = struct
  type t =
    [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  with sexp, compare
  (*let to_string t = Sexp.to_string (sexp_of_t t)*)
end

(*----------------------------------------------------------------------
  Reduced Stats (count!)
----------------------------------------------------------------------*)

module Stats : sig

  type t with sexp
  val lstat : Path.t -> t Or_error.t Deferred.t
  val equal : t -> t -> bool
  val kind : t -> Kind.t

end = struct

  (* should contain enough info from the Unix.Stats.t to relyable indicate that
     the path must be re-digested becaue it might have changed.

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
  } with sexp

  let equal = (=)

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
    Effort.track lstat_counter (fun () ->
      try_with (fun () ->
        Unix.lstat (Path.to_absolute_string path) >>= fun u ->
        return (of_unix_stats u)
      ) >>= fun res ->
      match res with
      | Ok x -> return (Ok x)
      | Error exn -> return (Error (Error.of_exn exn))
    )

end


(*----------------------------------------------------------------------
 Compute_digest (count!)
----------------------------------------------------------------------*)

module Digest : sig
  type t with sexp (* proxy for the contents of a file in the file-system *)
  val of_string : string -> t
  val equal : t -> t -> bool
end = struct
  type t = string with sexp
  let of_string x = x
  let equal = String.equal
end

module Compute_digest : sig

  val of_file : Path.t -> Digest.t Or_error.t Deferred.t

end = struct

  let of_file path =
    Effort.track digest_counter (fun () ->
      trace "DIGEST: %s..." (Path.to_absolute_string path);
      In_thread.run (fun () ->
        try (
          let ic = Caml.open_in_bin (Path.to_absolute_string path) in
          let res =
            try (
              let d = External_digest.channel ic (-1) in
              let res = External_digest.to_hex d in
              trace "DIGEST: %s... %s" (Path.to_absolute_string path) res;
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

  type t with sexp

  val run_ls : dir:Path.t -> t Or_error.t Deferred.t

  module Restriction : sig
    type t with sexp
    val create : Kind.t list -> Pattern.t -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

  val restrict : t -> Restriction.t -> t
  (*val inspect : t -> (Path.t * Kind.t) list*)
  val paths : t -> Path.t list
  val equal : t -> t -> bool

end = struct

  module Find = Copy_of_find

  type t = {
    listing : (Path.t * Kind.t) list
  } with sexp

  let run_ls ~dir =
    Effort.track ls_counter (fun () ->
      trace "RUN_LS: %s" (Path.to_absolute_string dir);
      try_with (fun () ->
        Find.find_all
          ~options:{
            Find.Options.
            max_depth = Some 0;
            follow_links = true;
            on_open_errors = Find.Options.Ignore;
            on_stat_errors = Find.Options.Ignore;
            filter = None;
            skip_dir = None
          }
          (Path.to_absolute_string dir)
      )
      >>= fun res ->
      match res with
      | Error exn -> return (Error (Error.of_exn exn))
      | Ok xs ->
        let listing =
          List.filter_map xs ~f:fun (name,stats) ->
            match name with
            | "." | ".." -> None  (* removing . and .. *)
            | _ ->
              let path = Path.create_from_absolute name in
              let kind = Unix.Stats.kind stats in
              Some (path,kind)
        in
        trace "RUN_LS: %s - done" (Path.to_absolute_string dir);
        return (Ok { listing })
    )

  module Restriction = struct
    type t = {
      kinds : Kind.t list;
      pat : Pattern.t;
    } with sexp, compare

    let create kinds pat = { kinds; pat; }

    (*let to_string t =
      sprintf "%s(%s)"
        (Pattern.to_string t.pat)
        (String.concat ~sep:"," (List.map t.kinds ~f:Kind.to_string))*)

    let to_string t = sprintf "%s" (Pattern.to_string t.pat)

  end

  let restrict t r = {
    listing =
      List.filter t.listing ~f:fun (path,kind) ->
        List.mem r.Restriction.kinds kind
        && Pattern.matches r.Restriction.pat (Path.basename path)
  }

  let equal t1 t2 = Int.(=) 0 (compare t1 t2)

  (*let inspect t = t.listing*)
  let paths t = List.map t.listing ~f:fst

end

(*----------------------------------------------------------------------
 Watcher (inotify wrapper)
----------------------------------------------------------------------*)

module Watcher : sig

  type t
  val create : unit -> t Deferred.t
  val watch : t -> what:[`file|`dir] -> Path.t -> Heart.t

end = struct

  module Inotify = Async_inotify

  type what = [`file|`dir]

  type t = {
    notifier : Inotify.t ;
    glass_hearts : (Path.t, what * Heart.Glass.t) Hashtbl.t;
  }

  let paths_of_event =
    let paths s = (* for the filename & the dir in which it resides *)
      let path = Path.create_from_absolute s in
      (* special case to avoid writing db/log from waking up top level globs *)
      if (Path.is_special_jenga_path path) then [] else
        let dir = Path.dirname path in
        [path; dir]
    in
    function
    | Inotify.Event.Modified s -> [Path.create_from_absolute s] (* just the filename *)
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
        (* mustn't trace the watcher event, because writing to the log
           file will cause a new event !! *)
        (*trace "Watcher, event: %s" (Inotify.Event.to_string event);*)
        List.iter (paths_of_event event) ~f:(fun path ->
          match (Hashtbl.find t.glass_hearts path) with
          | None -> () (*trace "Watcher, %s -- IGNORED" (Path.to_absolute_string path)*)
          | Some (_what,glass) ->
            if not (Heart.Glass.is_broken glass) then (
              (*(match what with
              | `dir -> ()
              | `file -> Message.file_changed (Heart.Glass.desc glass)
              );*)
              Heart.Glass.break glass
            )
        );
        return ()
      )
    )

  let create () =
    (* Have to pass a path at creation - what a pain, dont have one yet! *)
    Inotify.create ~recursive:false ~watch_new_dirs:false "/" >>= fun (notifier,_) ->
    let glass_hearts = Hashtbl.Poly.create () in
    let t = {notifier; glass_hearts} in
    suck_notifier_pipe t (Inotify.pipe notifier);
    return t

  (*let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u)*)

  let watch1 t ~what path ~dir =
    trace "watch: %s (dir: %s)" (Path.to_rrr_string path) (Path.to_rrr_string dir);
    match (
      match (Hashtbl.find t.glass_hearts path) with
      | None -> None
      | Some (_what,glass) ->
        let heart = Heart.of_glass glass in
        if Heart.is_broken heart then None else Some heart
    ) with
    | Some old_unbroken_heart -> old_unbroken_heart
    | None ->
      (*let u = genU() in*)
      let desc_string = (*sprintf "%d/" u ^ *)Path.to_rrr_string path in
      (*Message.message "setup new watcher: %s" desc_string;*)
      let desc = Heart.Desc.create desc_string in
      let glass = Heart.Glass.create desc in
      Hashtbl.set t.glass_hearts ~key:path ~data:(what,glass); (* always replacing *)
      let path_string = Path.to_absolute_string dir in
      don't_wait_for (
        Monitor.try_with (fun () ->
          Inotify.add t.notifier path_string
        ) >>| function
        | Ok () -> ()
        | Error exn ->
          Message.error "Unable to watch path: %s\n%s" path_string (Exn.to_string exn)
      );
      Heart.of_glass glass

  let watch t ~what path =
    match what with
    | `file -> watch1 t ~what path ~dir:(Path.dirname path)
    | `dir -> watch1 t ~what path ~dir:path

end

(*----------------------------------------------------------------------
 Stat_memo, making use of inotify watcher
----------------------------------------------------------------------*)

module Stat_memo : sig

  type t
  val create : Watcher.t -> t
  (* Caller mist declare what the lstat is expected to be for,
     so that the correct kind of watcher can be set up *)
  val lstat : t -> what:[`file|`dir] -> Path.t -> Stats.t Or_error.t Tenacious.t

end = struct

  type computation = Stats.t Or_error.t Tenacious.t
  type t = {
    watcher : Watcher.t;
    cache : (Path.t, computation) Hashtbl.Poly.t;
  }

  let create watcher = {
    watcher;
    cache = Hashtbl.Poly.create ();
  }

  let lstat t ~what path =
    match (Hashtbl.find t.cache path) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious =
        Tenacious.lift (fun () ->
          let heart = Watcher.watch t.watcher ~what path in
          Stats.lstat path >>= fun res ->
          return (res,heart)
        )
      in
      Hashtbl.add_exn t.cache ~key:path ~data:tenacious;
      tenacious

end

(*----------------------------------------------------------------------
 Digest/Listing result types
----------------------------------------------------------------------*)

module Digest_result = struct
  type t = [
  | `stat_error of Error.t
  | `not_a_file
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

  type t with sexp
  val create : unit -> t

  val digest_file :
    t ->
    Stat_memo.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

end = struct

  type t = {
    cache : (Path.t, Stats.t * Digest.t) Hashtbl.Poly.t;
  } with sexp

  let create () = {
    cache = Hashtbl.Poly.create ();
  }

  let is_file stats =
    match Stats.kind stats with
    | `File -> true
    | _ -> false

  let digest_file t sm ~file =
    let remove() = Hashtbl.remove t.cache file in
    Stat_memo.lstat sm ~what:`file file *>>= function
    | Error e -> (remove(); Tenacious.return (`stat_error e))
    | Ok stats ->
      if not (is_file stats) then Tenacious.return `not_a_file else
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

  type t with sexp
  val create : unit -> t

  val list_dir :
    t ->
    Stat_memo.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  type t = {
    cache : (Path.t, Stats.t * Listing.t) Hashtbl.Poly.t;
  } with sexp

  let create () = {
    cache = Hashtbl.Poly.create ();
  }

  let is_dir stats =
    match Stats.kind stats with
    | `Directory -> true
    | _ -> false

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

  type t with sexp
  val create : unit -> t

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
  } with sexp

  let create () = {
    digests = Digest_persist.create();
    listings = Listing_persist.create();
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
    cache : (Path.t, computation) Hashtbl.Poly.t;
  }

  let digest_file t dp sm ~file =
    match (Hashtbl.find t.cache file) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious = Persist.digest_file dp sm ~file in
      Hashtbl.add_exn t.cache ~key:file ~data:tenacious;
      tenacious

  let create () = {
    cache = Hashtbl.Poly.create ();
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
    cache : (Path.t, computation) Hashtbl.Poly.t;
  }

  let list_dir t dp sm ~dir =
    match (Hashtbl.find t.cache dir) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious = Persist.list_dir dp sm ~dir in
      Hashtbl.add_exn t.cache ~key:dir ~data:tenacious;
      tenacious

  let create () = {
    cache = Hashtbl.Poly.create ();
  }

end

(*----------------------------------------------------------------------
 Memo = combination of 3 memos - stat, digest, listing
----------------------------------------------------------------------*)

module Memo : sig

  type t
  val create : Watcher.t -> t

  val digest_file :
    t ->
    Persist.t ->
    file:Path.t ->
    Digest_result.t Tenacious.t

  val list_dir :
    t ->
    Persist.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  type t = {
    sm : Stat_memo.t;
    dm : Digest_memo.t;
    lm : Listing_memo.t;
  }

  let create watcher = {
    sm = Stat_memo.create watcher;
    dm = Digest_memo.create();
    lm = Listing_memo.create();
  }

  let digest_file t per ~file = Digest_memo.digest_file t.dm per t.sm ~file

  let list_dir t per ~dir = Listing_memo.list_dir t.lm per t.sm ~dir

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

  val list_dir :
    t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  type t = {
    memo : Memo.t;
    persist : Persist.t;
  }

  let create persist =
    Watcher.create () >>= fun watcher ->
    let memo = Memo.create watcher in
    let t = { memo; persist; } in
    return t

  let digest_file t ~file = Memo.digest_file t.memo t.persist ~file

  let list_dir t ~dir = Memo.list_dir t.memo t.persist ~dir

end


(*----------------------------------------------------------------------
 Glob
----------------------------------------------------------------------*)

module Glob : sig

  type t with sexp
  val create : dir:Path.t -> glob_string:string -> t

  val to_string : t -> string

  val exec : t -> Fs.t -> Listing_result.t Tenacious.t

  val compare : t -> t -> int

end = struct

  type t = {
    dir : Path.t;
    restriction : Listing.Restriction.t;
  } with sexp, compare

  let to_string t =
    sprintf "%s/%s"
      (Path.to_rrr_string t.dir)
      (Listing.Restriction.to_string t.restriction)

  let create ~dir ~glob_string =
    let kinds = [`File;`Directory] in
    let pat = Pattern.create_from_glob_string glob_string in
    let restriction = Listing.Restriction.create kinds pat in
    { dir ; restriction }

  let exec_no_cutoff glob fs  =
    Fs.list_dir fs ~dir:glob.dir *>>= function
    | `listing listing ->
      Tenacious.return (`listing (Listing.restrict listing glob.restriction))
    | x ->
      Tenacious.return x

  let exec_cutoff glob fs =
    Tenacious.lift (fun () ->
      let my_glass =
        let desc = Heart.Desc.create (to_string glob) in
        Heart.Glass.create desc
      in
      Tenacious.exec (exec_no_cutoff glob fs) >>= fun (res,heart) ->
      don't_wait_for (
        let rec loop heart =
          Heart.when_broken heart >>= fun () ->
          Tenacious.exec (exec_no_cutoff glob fs) >>= fun (res2,heart) ->
          if (Listing_result.equal res res2)
          then loop heart
          else (
            (*Message.file_changed (Heart.Glass.desc my_glass);*)
            Heart.Glass.break my_glass;
            Deferred.return ()
          )
        in
        loop heart
      );
      let my_heart = Heart.of_glass my_glass in
      Deferred.return (res, my_heart)
    )

  let exec = exec_cutoff

end


include Fs

let list_glob t glob = Glob.exec glob t


(* old non-tenacious interface... *)

let old_digest_file t ~file = Tenacious.exec (digest_file t ~file)
let old_list_glob t glob = Tenacious.exec (list_glob t glob)
