
open Core
open! Int.Replace_polymorphic_compare
open Async

module Digest = Db.Digest

module Version = struct
  let current = "6"
end

let jenga_show_persist =
  Option.is_some (Core.Sys.getenv "JENGA_SHOW_PERSIST")

let trace = Message.(if jenga_show_persist then message else trace)
let trace fmt = ksprintf (fun s -> trace "Persist: %s" s) fmt

let time_async f =
  let start_time = Time.now() in
  f () >>= fun res ->
  let duration = Time.diff (Time.now()) start_time in
  return (res,duration)

(* avoid saving persistent db unless it has been modified *)
let r_is_modified = ref false
let is_modified () = !r_is_modified
let set_is_saved () = r_is_modified := false
let modify who x =
  if !r_is_modified
  then x
  else (trace "setting MODIFIED flag, for: %s" who; r_is_modified := true ; x)

module Quality = struct
  type t = [`initial | `good] [@@deriving sexp_of]
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module State : sig

  type t [@@deriving bin_io]
  val snapshot : Db.t -> t
  val value : t -> Db.t

end = struct

  type t = Db.With_index.t Digest.With_store.t Path.With_store.t
  [@@deriving bin_io]

  let snapshot db =
    db
    |> Db.With_index.snapshot
    |> Digest.With_store.snapshot
    |> Path.With_store.snapshot

  let value x =
    x
    |> Path.With_store.value
    |> Digest.With_store.value
    |> Db.With_index.value

end

module Ops : sig

  val load_db_with_quality : db_filename:string -> (Db.t * Quality.t) Or_error.t Deferred.t
  val save_db : Db.t -> db_filename:string -> unit Deferred.t

end = struct

  let max_len = 800 * 1024 * 1024 (* default 100MiB is too small *)

  let load_db_with_quality ~db_filename =
    Sys.file_exists db_filename >>= function
    | `No ->
      trace "load: %s: does not exist" db_filename;
      return (Ok (Db.create(), `initial))
    | `Unknown ->
      return (Error (Error.createf "access to %S failed" db_filename))
    | `Yes ->
      Deferred.Or_error.try_with (fun () ->
        Reader.with_file db_filename ~f:(fun r ->
          Reader.read_bin_prot ~max_len r State.bin_reader_t
        )
      ) >>| function
      | Ok (`Ok state) -> Ok (State.value state, `good)
      | Ok `Eof -> Or_error.error_string "reached eof while bin_read'ing"
      | Error _ as e -> e

  let save_db db ~db_filename =
    if Jenga_options.t.turn_off_db_saves
    then Deferred.unit
    else (
      Metrics.Counter.incr Progress.saves_done;
      Monitor.try_with (fun () ->
        Writer.with_file_atomic db_filename ~f:(fun w ->
          (* snapshot & write must be in same async-block *)
          let state = State.snapshot db in
          Writer.write_bin_prot w State.bin_writer_t state;
          set_is_saved();
          return ()
        )
      ) >>| function
      | Ok () -> ()
      | Error exn ->
        Message.error "exception thrown while saving %s:\n%s"
          db_filename (Exn.to_string exn)
    )

end

let get_db_filename () =
  Path.to_absolute_string (Path.of_relative (
    Special_paths.Dot_jenga.db ~version:Version.current))

let load_db () =
  Ops.load_db_with_quality ~db_filename:(get_db_filename ())
  >>| Or_error.map ~f:(fun (db,_quality) -> db)

type t = {
  db : Db.t;
  quality : Quality.t;
  db_filename : string;
  mutable save_status : [ `saving of unit Deferred.t | `not_saving ];
  mutable periodic_saving_enabled : bool;
} [@@deriving fields]

let create () =
  let db_filename = get_db_filename () in
  trace "LOAD_DB: %s..." db_filename;
  time_async (fun () -> Ops.load_db_with_quality ~db_filename)
  >>| fun (result, duration) ->
  Or_error.map result ~f:(fun (db,quality) ->
    trace "LOAD_DB: %s... done (%s), quality=%s"
      db_filename (Time.Span.to_string duration)
      (Quality.to_string quality);
    {
      db;
      quality;
      db_filename;
      save_status = `not_saving;
      periodic_saving_enabled = true;
    })

let save_if_changed t =
  match is_modified() with
  | false ->
    trace "SAVE_DB: unchanged so not saving";
    Deferred.unit
  | true ->
    trace "SAVE_DB: %s..." (db_filename t);
    time_async (fun () ->
      Ops.save_db (db t) ~db_filename:(db_filename t)
    ) >>= fun ((),duration) ->
    trace "SAVE_DB: %s... done (%s)" (db_filename t) (Time.Span.to_string duration);
    return ()

let rec save_now t =
  match t.save_status with

  | `saving wait ->
    trace "waiting for in-flight save to complete";
    wait >>= fun () ->
    trace "doing save again";
    save_now t

  | `not_saving ->
    let ivar = Ivar.create () in
    t.save_status <- `saving (Ivar.read ivar);
    save_if_changed t >>= fun () ->
    Ivar.fill ivar ();
    t.save_status <- `not_saving;
    return ()

let save_periodically ~save_span t =
  don't_wait_for (
    let rec loop () =
      Clock.after save_span >>= fun () ->
      if t.periodic_saving_enabled
      then (
        save_now t >>= fun () ->
        loop ()
      )
      else loop ()
    in
    loop ()
  )

let create_saving_periodically save_span =
  Deferred.Or_error.map (create ()) ~f:(fun t ->
    save_periodically ~save_span t;
    t)

let disable_periodic_saving_and_save_now t =
  t.periodic_saving_enabled <- false;
  save_now t

let re_enable_periodic_saving t = t.periodic_saving_enabled <- true
