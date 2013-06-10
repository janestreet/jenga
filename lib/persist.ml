
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let jenga_show_persist =
  match Core.Std.Sys.getenv "JENGA_SHOW_PERSIST" with
  | None -> false
  | Some _ -> true

let message = Message.(if jenga_show_persist then message else trace)
let message fmt = ksprintf (fun s -> message "Persist: %s" s) fmt

let error fmt = ksprintf (fun s -> Message.error "Persist: %s" s) fmt

let time_async f =
  let start_time = Time.now() in
  f () >>= fun res ->
  let duration = Time.diff (Time.now()) start_time in
  return (res,duration)

module State : sig

  type t

  val fs_persist : t -> Fs.Persist.t
  val build_persist : t -> Build.Persist.t

  val equal : t -> t -> bool
  val copy : t -> t

  val load_db : db_filename:string -> t Deferred.t
  val save_db : t -> db_filename:string -> unit Deferred.t

  val sexp_of_t : t -> Sexp.t

end = struct

  type t = {
    fs_persist : Fs.Persist.t;
    build_persist : Build.Persist.t
  } with sexp, bin_io, fields

  let empty () = {
    fs_persist = Fs.Persist.create ();
    build_persist = Build.Persist.create ();
  }

  let equal t1 t2 =
    Fs.Persist.equal t1.fs_persist t2.fs_persist
    && Build.Persist.equal t1.build_persist t2.build_persist

  let copy t = {
    fs_persist = Fs.Persist.copy t.fs_persist;
    build_persist = Build.Persist.copy t.build_persist;
  }

  (* load/save bin_prot.. *)

  exception Unable_to_load_persist_db

  let max_len = 200 * 1024 * 1024 (* default 100Mb is too small *)

  let load_db ~db_filename =
    Sys.file_exists db_filename >>= function
    | `No | `Unknown ->
      message "load: %s - failed (does not exist; will create)" db_filename;
      return (empty())
    | `Yes ->
    try_with (fun () ->
      Reader.with_file db_filename ~f:(fun r ->
        Reader.read_bin_prot ~max_len r bin_reader_t
      )
    ) >>= function
      | Ok (`Ok t) -> return t (* successful load *)
      | Ok `Eof ->
        error "load: %s - failed: `Eof" db_filename;
        raise Unable_to_load_persist_db
      | Error exn ->
        error "load: %s - failed:\n%s" db_filename (Exn.to_string exn);
        raise Unable_to_load_persist_db

  let save_db t ~db_filename =
    Effort.track Build.persist_saves_done (fun () ->
      Writer.with_file_atomic db_filename ~f:(fun w ->
        Writer.write_bin_prot w bin_writer_t t;
        return ()
      )
    )

end

type t = {
  (* [state_mem] is the state read & modified by the build process. *)
  state_mem : State.t;

  (* [state_disk] is a copy of the state, which tracks its value when last synced to disk.
     To perform a save, [state_mem] is copied (in one async-atomic operation) to
     [state_disk], which can then be written to disk asyncronously over multiple
     async-cycles without blocking updates to [state_mem]. *)
  mutable state_disk : State.t;

  db_filename : string;
  mutable save_status : [ `saving of unit Deferred.t | `not_saving ];
  mutable periodic_saving_enabled : bool;
}

let fs_persist t = State.fs_persist t.state_mem
let build_persist t = State.build_persist t.state_mem

let db_filename t = t.db_filename

let create ~root_dir =
  let db_filename = root_dir ^/ Path.db_basename in

  message "LOAD_DB: %s..." db_filename;
  time_async (fun () -> State.load_db ~db_filename) >>= fun (state,duration) ->
  message "LOAD_DB: %s... done (%s)" db_filename (Time.Span.to_string duration);

  let t = {
    state_mem = state;
    state_disk = State.copy state;
    db_filename;
    save_status = `not_saving;
    periodic_saving_enabled = true;
  } in

  return t

let save_if_changed t =
  match (State.equal t.state_disk t.state_mem) with
  | true ->
    message "SAVE_DB: unchanged so not saving";
    Deferred.unit
  | false ->
    message "SAVE_DB: %s..." (db_filename t);
    time_async (fun () ->
      (* copy the in-mem state & save.. *)
      t.state_disk <- State.copy t.state_mem;
      State.save_db t.state_disk ~db_filename:(db_filename t)
    ) >>= fun ((),duration) ->
    message "SAVE_DB: %s... done (%s)" (db_filename t) (Time.Span.to_string duration);
    return ()

let save_now t =
  (* ensuring that subsequent saves wait until any in-flight save is done *)
  begin match t.save_status with
  | `not_saving -> Deferred.unit
  | `saving fin -> message "save_now - waiting for in-flight save to complete"; fin
  end >>= fun () ->
  match t.save_status with
  | `saving _fin ->
    (* someone beat us. shouldn't happen! *)
    assert false; (*fin*)
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
        message "save_periodically, calling Persist.save_now...";
        save_now t >>= fun () ->
        loop ()
      )
      else loop ()
    in
    loop ()
  )

let create_saving_periodically ~root_dir save_span =
  create ~root_dir >>= fun t ->
  save_periodically ~save_span t;
  return t

let disable_periodic_saving_and_save_now t =
  t.periodic_saving_enabled <- false;
  message "disable_periodic_saving_and_save_now, calling Persist.save_now...";
  save_now t

let re_enable_periodic_saving t = t.periodic_saving_enabled <- true
