open Core.Std
open Async.Std
module Stats = Unix.Stats

let xlstat_counter = Effort.Counter.create "xlstat"

type file_info = string * Unix.Stats.t

module Options = struct
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (string -> unit Deferred.t)

  type t = {
      max_depth: int option;
      follow_links: bool;
      on_open_errors: error_handler;
      on_stat_errors: error_handler;
      filter: (file_info -> bool Deferred.t) option;
      skip_dir: (file_info -> bool Deferred.t) option;
    }

  let default = {
      max_depth = None;
      follow_links = false;
      on_open_errors = Raise;
      on_stat_errors = Raise;
      filter = None;
      skip_dir = None
    }

  let ignore_errors = { default with
      on_open_errors = Ignore;
      on_stat_errors = Ignore
    }
end
module O = Options

type t = {
  options: Options.t;
  already_seen: ((int * int), unit) Hashtbl.t;  (* device num * inode *)
  mutable to_visit: (string * int) list;  (* dir to traverse and the depth it is at *)
  mutable current_dir: string;
  mutable current_handle: Unix.dir_handle;
  mutable depth: int;
  mutable closed: bool;
}

let open_next_dir t =
  let i = Ivar.create () in
  let rec loop t =
    match t.to_visit with
    | [] -> Ivar.fill i None
    | (dir_name, depth) :: rest ->
        upon (Monitor.try_with ~rest:`Raise (fun () ->
          t.to_visit <- rest;
          Unix.opendir dir_name >>| (fun handle ->
          t.current_handle <- handle;
          t.current_dir <- dir_name;
          t.depth <- depth;
          Some ())
        )) (function
        | Ok r -> Ivar.fill i r
        | Error e ->
          let e = Monitor.extract_exn e in
          match t.options.O.on_open_errors with
          | O.Ignore -> loop t
          | O.Raise -> raise e
          | O.Handle_with f ->
            upon (f dir_name) (fun () -> loop t)
          | O.Print ->
            Print.eprintf "unable to open %s - %s\n" dir_name (Exn.to_string e);
            loop t)
  in
  loop t;
  Ivar.read i
;;

let closedir t =
  Monitor.try_with ~rest:`Raise (fun () -> Unix.closedir t.current_handle) >>| (fun _ -> ())
;;

let close t =
  if not t.closed then
    begin
      t.closed <- true;
      closedir t >>| fun () ->
      Hashtbl.clear t.already_seen;
      t.to_visit <- [];
    end
  else Deferred.unit
;;

(* return None if [fn] is a directory and has already been seen, otherwise Some info *)
let is_new t (fn,stats) =
  if stats.Stats.kind <> `Directory then
    return (Some (fn,stats))
  else begin
    let uid = (stats.Stats.dev, stats.Stats.ino) in
    return
      (match Hashtbl.find t.already_seen uid with
      | Some () -> None
      | None ->
        Hashtbl.replace t.already_seen ~key:uid ~data:();
        Some (fn,stats))
  end
;;

let stat t fn =
  Effort.track xlstat_counter (fun () ->
    Monitor.try_with ~rest:`Raise (fun () ->
      let stat = if t.options.O.follow_links then Unix.stat else Unix.lstat in
      stat fn >>| (fun stat -> Some (fn, stat))
    ) >>= (fun res ->
      match res with
      | Ok r -> return r
      | Error e ->
        let e = Monitor.extract_exn e in
        match t.options.O.on_stat_errors with
        | O.Ignore -> return None
        | O.Raise -> raise e
        | O.Handle_with f -> f fn >>| (fun () -> None)
        | O.Print ->
          Print.eprintf "unable to stat %s - %s\n" fn (Exn.to_string e);
          return None)
  )
;;

let handle_dirs t ((fn,stats) as info) =
  let visit () =
    t.to_visit <- (fn, (t.depth + 1)) :: t.to_visit;
    return (Some info)
  in
  let maybe_visit () =
    match t.options.O.skip_dir with
    | None   -> visit ()
    | Some f ->
      f info
      >>= fun skip ->
      if skip then return None else visit ()
  in
  let maybe_return_info () =
    match t.options.O.skip_dir with
    | None   -> return (Some info)
    | Some f ->
      f info
      >>= fun skip ->
      if skip then return None else return (Some info)
  in
  if stats.Stats.kind = `Directory then begin
    match t.options.O.max_depth with
    | None           -> maybe_visit ()
    | Some max_depth ->
      if t.depth < max_depth then maybe_visit () else begin
        maybe_return_info ()
      end
  end else
    return (Some info)
;;

let filter t file =
  match file with
  | None      -> return None
  | Some file ->
      match t.options.O.filter with
      | None   -> return (Some file)
      | Some f -> f file >>| (fun keep -> if keep then Some file else None)
;;

exception Attempt_to_use_closed_find of [`Most_recent_dir of string] with sexp ;;

let ensure_not_closed t = if t.closed then
  raise (Attempt_to_use_closed_find (`Most_recent_dir t.current_dir)) ;;

(* returns the next file from the conceptual stream and updates the state of t - this
   is the only way that t should ever be updated *)
let next t =
  ensure_not_closed t;
  let i = Ivar.create () in
  let rec loop () =
    upon (Monitor.try_with ~rest:`Raise (fun () ->
      Unix.readdir t.current_handle >>= function
      | "." | ".." -> return None
      | basename   ->
          let fn = Filename.concat t.current_dir basename in
          (* each function in this bind returns None if the file should be skipped, and
            Some f i if it thinks it's ok to emit - possibly updating the state or
            transforming f along the way *)
          let (>>>=) v f =
            v
            >>= function
            | None   -> return None
            | Some v -> f v
          in
          stat t fn
          >>>= is_new t
          >>>= handle_dirs t
          >>= filter t
    )) (function
    | Ok None -> loop ()
    | Ok r -> Ivar.fill i r
    | Error e ->
      upon (closedir t) (fun () ->
        match Monitor.extract_exn e with
        | End_of_file ->
          upon (open_next_dir t) (function
            | Some () -> loop ()
            | None -> upon (close t) (fun () -> Ivar.fill i None))
        | e -> raise e))
  in
  loop ();
  Ivar.read i
;;

let create ?(options=Options.default) dir =
  Unix.opendir dir >>| fun dh ->
  {
    options = options;
    already_seen = Hashtbl.Poly.create () ~size:11;
    to_visit = [];
    current_dir = dir;
    current_handle = dh;
    depth = 0;
    closed = false;
  }
;;

let fold t ~init ~f =
  Deferred.create (fun i ->
    let rec loop acc =
      upon (next t) (function
      | None -> Ivar.fill i acc
      | Some file -> upon (f acc file) loop)
    in
    loop init)
;;

let iter t ~f = fold t ~init:() ~f:(fun () file -> f file)

let to_list t =
  (fold t ~init:[] ~f:(fun acc file -> return (file :: acc))) >>| List.rev
;;

let find_all ?options dir =
  create ?options dir >>= to_list
;;
