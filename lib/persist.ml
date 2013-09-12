
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let jenga_show_persist =
  match Core.Std.Sys.getenv "JENGA_SHOW_PERSIST" with
  | None -> false
  | Some _ -> true

let trace = Message.(if jenga_show_persist then message else trace)
let trace fmt = ksprintf (fun s -> trace "Persist: %s" s) fmt

let time_async f =
  let start_time = Time.now() in
  f () >>= fun res ->
  let duration = Time.diff (Time.now()) start_time in
  return (res,duration)

module State : sig

  type t

  val fs_persist : t -> Fs.Persist.t
  val build_persist : t -> Build.Persist.t

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

  (* load/save bin_prot.. *)

  let max_len = 800 * 1024 * 1024 (* default 100Mb is too small *)

  let load_db ~db_filename =
    Sys.file_exists db_filename >>= function
    | `No | `Unknown ->
      trace "load: %s: does not exist" db_filename;
      return (empty())
    | `Yes ->
    try_with (fun () ->
      Reader.with_file db_filename ~f:(fun r ->
        Reader.read_bin_prot ~max_len r bin_reader_t
      )
    ) >>= fun res ->
      match (
        match res with
        | Ok (`Ok t) -> Some t (* successful load *)
        | Ok `Eof -> trace "load: %s: `Eof" db_filename; None
        | Error exn -> trace "load: %s:\n%s" db_filename (Exn.to_string exn); None
      ) with
      | Some t -> return t
      | None ->
        Message.message "format of persistant state has changed; everything will rebuild";
        return (empty())

  let save_db t ~db_filename =
    Effort.track Build.persist_saves_done (fun () ->
      Writer.with_file_atomic db_filename ~f:(fun w ->
        Writer.write_bin_prot w bin_writer_t t;
        Misc.set_persist_is_saved();
        return ()
      )
    )

end

type t = {
  state_mem : State.t;
  db_filename : string;
  mutable save_status : [ `saving of unit Deferred.t | `not_saving ];
  mutable periodic_saving_enabled : bool;
}

let fs_persist t = State.fs_persist t.state_mem
let build_persist t = State.build_persist t.state_mem

let db_filename t = t.db_filename

let create ~root_dir =
  let db_filename = root_dir ^/ Path.db_basename in

  trace "LOAD_DB: %s..." db_filename;
  time_async (fun () -> State.load_db ~db_filename) >>= fun (state,duration) ->
  trace "LOAD_DB: %s... done (%s)" db_filename (Time.Span.to_string duration);

  let t = {
    state_mem = state;
    db_filename;
    save_status = `not_saving;
    periodic_saving_enabled = true;
  } in

  return t

let save_if_changed t =
  match Misc.persist_is_modified() with
  | false ->
    trace "SAVE_DB: unchanged so not saving";
    Deferred.unit
  | true ->
    trace "SAVE_DB: %s..." (db_filename t);
    time_async (fun () ->
      State.save_db t.state_mem ~db_filename:(db_filename t)
    ) >>= fun ((),duration) ->
    trace "SAVE_DB: %s... done (%s)" (db_filename t) (Time.Span.to_string duration);
    return ()

let save_now t =
  (* ensuring that subsequent saves wait until any in-flight save is done *)
  begin match t.save_status with
  | `not_saving -> Deferred.unit
  | `saving fin -> trace "save_now - waiting for in-flight save to complete"; fin
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
        trace "save_periodically, calling Persist.save_now...";
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
  trace "disable_periodic_saving_and_save_now, calling Persist.save_now...";
  save_now t

let re_enable_periodic_saving t = t.periodic_saving_enabled <- true
