
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

(* memoization of tenacious computations.. unless we are running without notifers *)

let yes_memo ht ~key f =
    match (Hashtbl.find ht key) with
    | Some tenacious -> tenacious
    | None ->
      let tenacious = f () in
      let tenacious = Tenacious.reify tenacious in
      Hashtbl.add_exn ht ~key ~data:tenacious;
      tenacious

let no_memo _ht ~key:_ f = f ()

let memoize ~nono = if nono then no_memo else yes_memo

let equal_using_compare compare = fun x1 x2 -> Int.(=) 0 (compare x1 x2)
let unbreakable x = x,Heart.unbreakable

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
let ( *>>| ) t f = Tenacious.map t ~f

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
 System lstat - counted; exns caught
----------------------------------------------------------------------*)

let unix_lstat path =
  Effort.track lstat_counter (fun () ->
    try_with (fun () ->
      Unix.lstat path
    )
  )

(*----------------------------------------------------------------------
  Reduced Stat info
----------------------------------------------------------------------*)

module Stats : sig

  type t with sexp, bin_io
  val lstat : Path.X.t -> t Or_error.t Deferred.t
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
    let path = Path.X.to_absolute_string path in
    unix_lstat path >>= function
    | Ok u -> return (Ok (of_unix_stats u))
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
    (*Message.message "mkdir: %s" (Path.X.to_string dir);*)
    Effort.track mkdir_counter (fun () ->
      try_with (fun () ->
        Unix.mkdir ~p:() (Path.X.to_absolute_string dir)
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

module type Compute_digest_sig = sig

  val of_file : Path.X.t -> Digest.t Or_error.t Deferred.t

end

module Ocaml__Compute_digest : Compute_digest_sig = struct

  let of_file path =
    File_access.enqueue (fun () ->
      Effort.track digest_counter (fun () ->
        In_thread.run (fun () ->
          try (
            let ic = Caml.open_in_bin (Path.X.to_absolute_string path) in
            let res =
              try (
                let d = External_digest.channel ic (-1) in
                let res = External_digest.to_hex d in
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
    )

end

module External__Compute_digest : Compute_digest_sig = struct

  let putenv = []
  let prog = "/usr/bin/md5sum"
  let dir = Path.the_root

  let of_file path =
    Effort.track digest_counter (fun () ->
      let args = [Path.X.to_absolute_string path] in
      let request = Forker.Request.create ~putenv ~dir ~prog ~args in
      Digester.throttle (fun () -> Forker.run request)
      >>= fun {Forker.Reply. stdout;stderr=_;outcome} ->
      match outcome with
      | `error s -> return (Error (Error.of_string s))
      | `success ->
        match (String.lsplit2 stdout ~on:' ') with
        | None -> return (Error (Error.of_string
                                   (sprintf "unexpected output from md5sum: %s" stdout)))
        | Some (res,_) -> return (Ok (Digest.of_string res))
    )

end

let use_ocaml =
  match Core.Std.Sys.getenv "JENGA_USE_OCAML_DIGEST" with
  | None -> false (* default no - use external *)
  | Some _ -> true

let m =
  match use_ocaml with
  | true -> (module Ocaml__Compute_digest : Compute_digest_sig)
  | false -> (module External__Compute_digest : Compute_digest_sig)

let () =
  if use_ocaml then (
    Printf.eprintf "using internal ocaml digest instead of external call to md5sum\n%!"
  )

module Compute_digest = (val m : Compute_digest_sig)


(*----------------------------------------------------------------------
  Listing (count!)
----------------------------------------------------------------------*)

module Listing : sig

  type t with sexp, bin_io, compare

  val equal : t -> t -> bool
  val run_ls : dir:Path.t -> t Or_error.t Deferred.t
  val paths : t -> Path.t list

  module Restriction : sig
    type t with sexp, bin_io
    val create : kinds:Kind.t list option -> Pattern.t -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

  val restrict : t -> Restriction.t -> t

end = struct

  module Elem = struct
    type t = {
      base : string;
      kind : Kind.t;
    } with sexp, bin_io, compare
  end

  type t = {
    dir : Path.t;
    listing : Elem.t list;
  } with sexp, bin_io, compare

  let run_ls ~dir =
    File_access.enqueue (fun () ->
      let path_string = Path.to_absolute_string dir in
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
                  let path_string = Path.to_absolute_string dir ^ "/" ^ base in
                  unix_lstat path_string >>= function
                  | Error _e ->
                    (* File disappeared between readdir & lstat system calls.
                       Handle as if readdir never told as about it *)
                    loop acc
                  | Ok u ->
                    let kind = u.Unix.Stats.kind in
                    loop ({Elem.base;kind} :: acc)
                end
              | Error exn ->
                match Monitor.extract_exn exn with
                | End_of_file ->
                  (* no more filenames - normal; we are finished listing *)
                  return { dir; listing = acc }
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
      )
    )

  let paths t =
    List.map t.listing ~f:(fun e ->
      Path.relative ~dir:t.dir e.Elem.base
    )

  let equal = equal_using_compare compare

  module Restriction = struct
    type t = {
      kinds : Kind.t list option; (* None means any kind *)
      pat : Pattern.t;
    } with sexp, bin_io, compare

    let create ~kinds pat = { kinds; pat; }

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
      dir = t.dir;
      listing =
        List.filter t.listing ~f:(fun e ->
          match_kind e.Elem.kind
          && Pattern.matches r.Restriction.pat e.Elem.base
        )
    }

end

(*----------------------------------------------------------------------
 Watcher (inotify wrapper) - works on absolute path strings
----------------------------------------------------------------------*)

module Watcher_real : sig

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

  type t = {
    ignore : (path:string -> bool);
    expect : (path:string -> bool);
    notifier : Inotify.t ;
    file_glass : Heart.Glass.t String.Table.t;
    dir_glass : Heart.Glass.t String.Table.t;
  }

  let paths_of_event t =
    let paths path = (* for the filename & the dir in which it resides *)
      if (t.ignore ~path) then [] else
        let dir = Filename.dirname path in
        [path; dir]
    in
    function
    | Inotify.Event.Queue_overflow ->
      Message.error "Inotify.Event.Queue_overflow"; [] (* at least log an error *)
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

          begin match (Hashtbl.find t.dir_glass path) with
          | None -> ()
          | Some glass ->
            assert (not (Heart.Glass.is_broken glass));
            Heart.Glass.break glass;
            Hashtbl.remove t.dir_glass path;
          end;

          begin match (Hashtbl.find t.file_glass path) with
          | None -> ()
          | Some glass ->
            assert (not (Heart.Glass.is_broken glass));
            (*
              Show path events to which we are sensitized...

              Don't show events for targets of currently running actions.

              Will see events for externally changed files (i.e. edited source files
              or removed generated files), but also for paths affected by a running
              jenga action which are not declared as targets of that action.

            *)
            if not (t.expect ~path) then (
              Message.file_changed ~desc:(Heart.Glass.desc glass)
            );
            Heart.Glass.break glass;
            Hashtbl.remove t.file_glass path;
          end

        );

        return ()
      )
    )

  let create ~ignore ~expect =
    (* Have to pass a path at creation - what a pain, dont have one yet! *)
    Inotify.create ~recursive:false ~watch_new_dirs:false "/" >>= fun (notifier,_) ->
    let file_glass = String.Table.create () in
    let dir_glass = String.Table.create () in
    let t = {ignore; expect; notifier; file_glass; dir_glass} in
    suck_notifier_pipe t (Inotify.pipe notifier);
    return t

  let watch_file_or_dir t ~what ~path ~desc =
    let dir, glass_cache =
      (* A notifier is setup to watch [dir], computed from [path] & [what]
         The underlying Inotify module handles repeated setup for same path.
         So we make no attempt to avoid the repeats.
      *)
      match what with
      | `file -> Filename.dirname path  , t.file_glass
      | `dir -> path                    , t.dir_glass
    in
    unlogged "watch: %s (dir: %s)" path dir;
    match (Hashtbl.find glass_cache path) with
    | Some glass ->
      (*assert (not (Heart.Glass.is_broken glass));*)
      return (Ok (Heart.of_glass glass))
    | None ->
      let absolute_dir = dir in (* do we need absolute dir ??? *)
      unlogged "setup watcher: %s" absolute_dir;
      Monitor.try_with (fun () ->
        Inotify.add t.notifier absolute_dir
      ) >>| function
      | Error exn ->
        Message.error "Unable to watch path: %s" absolute_dir;
        Error (Error.of_exn exn)
      | Ok () ->
        let default () = Heart.Glass.create ~desc in
        let glass = Hashtbl.find_or_add glass_cache path ~default in
        Ok (Heart.of_glass glass)

end

(*----------------------------------------------------------------------
 Watcher -- adding support for no-notifiers
----------------------------------------------------------------------*)

module Watcher : sig

  type t
  val create :
    nono: bool ->
    ignore:(path:string -> bool) ->
    expect:(path:string -> bool) ->
    t Deferred.t

  val watch_file_or_dir : t ->
    what:[`file|`dir] ->
    path:string ->
    desc:string ->
    Heart.t Or_error.t Deferred.t

end = struct

  type t = Watcher_real.t option

  let create ~nono ~ignore ~expect =
    if nono
    then return None
    else Watcher_real.create ~ignore ~expect >>| fun w -> Some w

  let watch_file_or_dir t ~what ~path ~desc =
    match t with
    | None -> return (Ok Heart.unbreakable)
    | Some w -> Watcher_real.watch_file_or_dir w ~what ~path ~desc

end

(*----------------------------------------------------------------------
 Stat_memo, making use of inotify watcher
----------------------------------------------------------------------*)

module Stat_memo : sig

  type t
  val create : nono:bool -> Watcher.t -> t
  (* Caller must declare what the lstat is expected to be for,
     so that the correct kind of watcher can be set up *)
  val lstat : t -> what:[`file|`dir] -> Path.X.t -> Stats.t Or_error.t Tenacious.t

end = struct

  type computation = Stats.t Or_error.t Tenacious.t
  type t = {
    nono : bool;
    watcher : Watcher.t;
    file_watch_cache : computation Path.X.Table.t;
    dir_watch_cache : computation Path.X.Table.t;
  }

  let create ~nono watcher = {
    nono;
    watcher;
    file_watch_cache = Path.X.Table.create ();
    dir_watch_cache = Path.X.Table.create ();
  }

  let lstat t ~what path =
    let cache =
      match what with
      | `file -> t.file_watch_cache
      | `dir -> t.dir_watch_cache
    in
    memoize ~nono:t.nono cache ~key:path (fun () ->
      let tenacious =
        Tenacious.lift (fun () ->
          Watcher.watch_file_or_dir t.watcher ~what
            ~path:(Path.X.to_absolute_string path)
            ~desc:(Path.X.to_string path)
          >>= function
          | Error exn ->
            return (unbreakable (Error exn))
          | Ok heart ->
            Stats.lstat path >>| fun res -> (res,heart)
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
          Reader.file_contents (Path.X.to_absolute_string file)
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

  type t with sexp, bin_io

  val create : unit -> t

  val digest_file :
    t ->
    Stat_memo.t ->
    file:Path.X.t ->
    Digest_result.t Tenacious.t

end = struct

  type t = {
    cache : (Stats.t * Digest.t) Path.X.Table.t;
  } with sexp, bin_io

  let create () = {
    cache = Path.X.Table.create ();
  }

  let digest_file t sm ~file =
    let remove() = Hashtbl.remove t.cache file in
    Stat_memo.lstat sm ~what:`file file *>>= function
    | Error e -> (remove(); Tenacious.return (`stat_error e))
    | Ok stats ->
      let kind = Stats.kind stats in
      if (is_dir stats) then Tenacious.return `is_a_dir else
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
            Compute_digest.of_file file >>| unbreakable
          ) *>>= function
          | Error e -> (remove(); Tenacious.return (`digest_error e))
          | Ok new_digest ->
            Hashtbl.set (Misc.mod_persist t.cache) ~key:file ~data:(stats,new_digest);
            Tenacious.return (`digest new_digest)

end


(*----------------------------------------------------------------------
 Listing_persist - w.r.t stat->listing mapping
----------------------------------------------------------------------*)

module Listing_persist : sig

  type t with sexp, bin_io

  val create : unit -> t

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

  let list_dir t sm ~dir =
    let remove() = Hashtbl.remove t.cache dir in
    let tenacious =
      Stat_memo.lstat sm ~what:`dir (Path.X.of_relative dir) *>>= function
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
              Listing.run_ls ~dir >>| unbreakable
            ) *>>= function
            | Error e -> (remove(); Tenacious.return (`listing_error e))
            | Ok new_listing ->
              if Path.(equal the_root dir) then (
              (* Don't save the result of listing the root of the repo because having .jenga
                 files in the root means that the stat will always differ and so we can
                 never use the listing saved.  The cost of saving the listing for root is
                 that the persistent state will always change and hence need writing.  *)
              ) else (
                Hashtbl.set (Misc.mod_persist t.cache) ~key:dir ~data:(stats,new_listing);
              );
              Tenacious.return (`listing new_listing)
    in
    (*Tenacious.reify*) (* reify without memoize is pointless! *)
    tenacious


end

(*----------------------------------------------------------------------
Persist - combination of Digest & Listing persists
----------------------------------------------------------------------*)

module Persist : sig

  type t with sexp, bin_io
  val create : unit -> t

  val digest_file :
    t ->
    Stat_memo.t ->
    file:Path.X.t ->
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

  let digest_file t sm ~file = Digest_persist.digest_file t.digests sm ~file

  let list_dir t sm ~dir = Listing_persist.list_dir t.listings sm ~dir

end

(*----------------------------------------------------------------------
 Contents_memo
----------------------------------------------------------------------*)

module Contents_memo : sig

  type t
  val create : nono:bool -> t

  val contents_file :
    t ->
    Stat_memo.t ->
    file:Path.X.t ->
    Contents_result.t Tenacious.t

end = struct

  type computation = Contents_result.t Tenacious.t
  type t = {
    nono : bool;
    cache : computation Path.X.Table.t;
  }

  let contents_file t sm ~file =
    memoize ~nono:t.nono t.cache ~key:file (fun () ->
      contents_file sm ~file
    )

  let create ~nono = {
    nono;
    cache = Path.X.Table.create ();
  }

end

(*----------------------------------------------------------------------
 Digest_memo
----------------------------------------------------------------------*)

module Digest_memo : sig

  type t
  val create : nono:bool -> t

  val digest_file :
    t ->
    Persist.t ->
    Stat_memo.t ->
    file:Path.X.t ->
    Digest_result.t Tenacious.t

end = struct

  type computation = Digest_result.t Tenacious.t
  type t = {
    nono: bool;
    cache : computation Path.X.Table.t;
  }

  let digest_file t dp sm ~file =
    memoize ~nono:t.nono t.cache ~key:file (fun () ->
      Persist.digest_file dp sm ~file
    )

  let create ~nono = {
    nono;
    cache = Path.X.Table.create ();
  }

end

(*----------------------------------------------------------------------
 Listing_memo
----------------------------------------------------------------------*)

module Listing_memo : sig

  type t
  val create : nono:bool -> t

  val list_dir :
    t ->
    Persist.t ->
    Stat_memo.t ->
    dir:Path.t ->
    Listing_result.t Tenacious.t

end = struct

  type computation = Listing_result.t Tenacious.t
  type t = {
    nono: bool;
    cache : computation Path.Table.t;
  }

  let list_dir t dp sm ~dir =
    memoize ~nono:t.nono t.cache ~key:dir (fun () ->
      Persist.list_dir dp sm ~dir
    )

  let create ~nono = {
    nono;
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
  val create_from_path : kinds: Kind.t list option -> Path.t -> t
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
        pat : Pattern.t;
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
      (Path.to_string t.dir)
      (Listing.Restriction.to_string t.restriction)

  let raw_create ~dir ~kinds pat =
    let restriction = Listing.Restriction.create ~kinds pat in
    let t = { dir ; restriction } in
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

  let create ~dir ~kinds ~glob_string =
    let pat = Pattern.create_from_glob_string glob_string in
    let key = {Key. dir; kinds; pat} in
    cached_create key

  let create_from_path ~kinds path =
    create ~dir:(Path.dirname path) ~kinds ~glob_string:(Path.basename path) (*OLD *)
    (*let dir = Path.dirname path in
    let pat = Pattern.create_from_literal_string (Path.basename path) in
    let key = {Key. dir; kinds; pat} in
    cached_create key*)

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
      Tenacious.exec (exec_no_cutoff glob lm per sm) >>| fun (res,heart) ->
      let rec loop heart =
        Heart.when_broken heart >>> fun () ->
        Tenacious.exec (exec_no_cutoff glob lm per sm) >>> fun (res2,heart) ->
        if (Listing_result.equal res res2)
        then loop heart
        else (
          Message.file_changed ~desc:(to_string glob);
          Heart.Glass.break my_glass
        )
      in
      loop heart;
      let my_heart = Heart.of_glass my_glass in
      (res, my_heart)
    )

end

(*----------------------------------------------------------------------
 Glob_memo
----------------------------------------------------------------------*)

module Glob_memo : sig

  type t
  val create : nono:bool -> t

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
    nono : bool;
    cache : computation Glob.Table.t;
  }

  let list_glob t lm per sm glob =
    memoize ~nono:t.nono t.cache ~key:glob (fun () ->
      Glob.exec glob lm per sm
    )

  let create ~nono = {
    nono;
    cache = Glob.Table.create ();
  }

end

(*----------------------------------------------------------------------
 Memo = combination of 4 memos - stat, digest, listing, glob
----------------------------------------------------------------------*)

module Memo : sig

  type t
  val create : nono:bool -> Watcher.t -> t

  val contents_file :
    t ->
    file:Path.X.t ->
    Contents_result.t Tenacious.t

  val digest_file :
    t ->
    Persist.t ->
    file:Path.X.t ->
    Digest_result.t Tenacious.t

  val list_glob :
    t ->
    Persist.t ->
    Glob.t ->
    Listing_result.t Tenacious.t

end = struct

  type t = {
    sm : Stat_memo.t;
    cm : Contents_memo.t;
    dm : Digest_memo.t;
    lm : Listing_memo.t;
    gm : Glob_memo.t;
  }

  let create ~nono watcher = {
    sm = Stat_memo.create ~nono watcher ;
    cm = Contents_memo.create ~nono;
    dm = Digest_memo.create ~nono;
    lm = Listing_memo.create ~nono;
    gm = Glob_memo.create ~nono;
  }

  let contents_file t ~file = Contents_memo.contents_file t.cm t.sm ~file

  let digest_file t per ~file = Digest_memo.digest_file t.dm per t.sm ~file

  let list_glob t per glob = Glob_memo.list_glob t.gm t.lm per t.sm glob

end

(*----------------------------------------------------------------------
 Fs - combination of persistent & dynamic caches
----------------------------------------------------------------------*)

module Fs : sig

  type t
  val create : Config.t -> Persist.t -> t Deferred.t

  val contents_file :
    t ->
    file:Path.X.t ->
    Contents_result.t Tenacious.t

  val digest_file :
    t ->
    file:Path.X.t ->
    Digest_result.t Tenacious.t

  val list_glob :
    t ->
    Glob.t ->
    Listing_result.t Tenacious.t

  val watch_sync_file : t -> path:string -> Heart.t Deferred.t
  val nono : t -> bool

end = struct

  type t = {
    nono : bool;
    watcher : Watcher.t;
    memo : Memo.t;
    persist : Persist.t;
  } with fields

  let create config persist =
    let nono = Config.no_notifiers config in
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
      | Some path -> Path.Key.locked (Path.get_key path)
    in
    Watcher.create ~nono ~ignore ~expect >>= fun watcher ->
    let memo = Memo.create ~nono watcher in
    let t = { nono; watcher; memo; persist; } in
    return t

  let contents_file t ~file = Memo.contents_file t.memo ~file

  let digest_file t ~file = Memo.digest_file t.memo t.persist ~file

  let list_glob t glob = Memo.list_glob t.memo t.persist glob

  let watch_sync_file t ~path =
    Watcher.watch_file_or_dir t.watcher ~what:`file ~path ~desc:path
    >>= function
    | Ok heart -> return heart
    | Error e -> raise (Error.to_exn e)

end


include Fs

let ensure_directory (_:t) ~dir = (* why need t ? *)
  Tenacious.lift (fun () ->
    ensure_directory ~dir >>| unbreakable (* ?? *)
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


let tmp_jenga =
  let user = Core.Std.Unix.getlogin() in
  sprintf "/tmp/jenga-%s" user

let () =
  don't_wait_for (
    Unix.mkdir ~p:() tmp_jenga
  )

let heart_broken ~timeout heart =
  choose [
    choice (Heart.when_broken heart) (fun () -> `broken);
    choice (Clock.after timeout) (fun () -> `timeout);
  ]

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
    if Fs.nono t then tenacious else (* no notifiers, so cant/dont sync *)
    let u1 = genU1 () in
    let genU2 = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
    Tenacious.lift (fun () ->
      let u2 = genU2 () in
      let path = sprintf "%s/jenga-%s-%d-%d.sync" tmp_jenga pid_string u1 u2 in

      (* Setup watcher for sync-file *)
      watch_sync_file t ~path >>= fun created_heart ->

      (* setup remove()... *)
      let remove () =
        (* ensure creation event has happened.. *)
        begin heart_broken ~timeout:(sec 10.) created_heart >>| function
        | `broken -> ()
        | `timeout -> Message.error "lost inotify event for creation of: %s" path
        end >>= fun () ->

        (* Setup new watcher; unlink the file; wait for the event *)
        watch_sync_file t ~path >>= fun deleted_heart ->
        File_access.enqueue (fun () ->
          Sys.remove path
        ) >>= fun () ->
        begin heart_broken ~timeout:(sec 10.) deleted_heart >>| function
        | `broken -> ()
        | `timeout -> Message.error "lost inotify event for unlink of: %s" path
        end
      in

      (* create the sync-file *)
      let bag_elem = Bag.add created_but_not_deleted (path,remove) in

      unless_shutting_down ~f:(fun () ->
        File_access.enqueue (fun () ->
          Writer.save path ~contents:sync_contents
        )
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

