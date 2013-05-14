
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let jenga_show_persist =
  match Core.Std.Sys.getenv "JENGA_SHOW_PERSIST" with
  | None -> false
  | Some _ -> true

let message = Message.(if jenga_show_persist then message else trace)
let message fmt = ksprintf (fun s -> message "Persist: %s" s) fmt

module State = struct

  type t = {
    fs_persist : Fs.Persist.t;
    build_persist : Build.Persist.t
  } with sexp,fields

  let empty () = {
    fs_persist = Fs.Persist.create ();
    build_persist = Build.Persist.create ();
  }

  let try_load_dont_convert ~filename =
    Sys.file_exists filename >>= function
    | `No | `Unknown ->
      message "load: %s - failed (does not exist; will create)" filename;
      return None
    | `Yes ->
      try_with (fun () ->
        Reader.load_sexp_exn filename Fn.id
      ) >>= function
      | Ok sexp -> return (Some sexp)
      | Error _ ->
        message "load: %s - failed (corrupted; ignoring)" filename;
        return None

  let try_load_dont_convert ~filename =
    message "loading: %s..." filename;
    let start_time = Time.now() in
    try_load_dont_convert ~filename >>= fun res ->
    let duration = Time.diff (Time.now()) start_time in
    message "loading: %s... done (%s)" filename (Time.Span.to_string duration);
    return res

  let try_load ~filename =
    try_load_dont_convert ~filename >>= function
    | None -> return None
    | Some sexp ->
      return (
        try Some (t_of_sexp sexp)
        with _ ->
          message "load: %s - failed (format changed; ignoring)" filename;
          None
      )

  let load ~filename =
    try_load ~filename >>= function
    | None -> return (empty())
    | Some t -> return t

  let save_if_changed t ~filename =
    let sexp = sexp_of_t t in
    (
      try_load_dont_convert ~filename >>= function
      | None -> return false
      | Some sexp_old ->
        return (Sexp.equal sexp sexp_old)
    ) >>= fun unchanged ->
    if unchanged
    then (
      message "unchanged so not saving: %s" filename;
      Deferred.unit
    )
    else (
      message "saving: %s..." filename;
      let start_time = Time.now() in
      Writer.save_sexp ~fsync:true ~hum:true filename sexp >>= fun () ->
      let duration = Time.diff (Time.now()) start_time in
      message "saving: %s... done (%s)" filename (Time.Span.to_string duration);
      return ()
    )

end


type t = {
  state : State.t;
  filename : string;
  mutable save_status : [ `saving of unit Deferred.t | `not_saving ];
  mutable periodic_saving_enabled : bool;
}

let create ~filename =
  State.load ~filename >>= fun state ->
  let t = {
    state;
    filename;
    save_status = `not_saving;
    periodic_saving_enabled = true;
  } in
  return t

let fs_persist t = State.fs_persist t.state
let build_persist t = State.build_persist t.state

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
    State.save_if_changed t.state ~filename:t.filename >>= fun () ->
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
        message "save_periodically, calling Persist.save_now...done";
        loop ()
      )
      else loop ()
    in
    loop ()
  )

let create_saving_periodically ~filename save_span =
  create ~filename >>= fun t ->
  save_periodically ~save_span t;
  return t

let disable_periodic_saving_and_save_now t =
  t.periodic_saving_enabled <- false;
  message "disable_periodic_saving_and_save_now, calling Persist.save_now...";
  save_now t >>= fun () ->
  message "disable_periodic_saving_and_save_now, calling Persist.save_now...done";
  return ()

let re_enable_periodic_saving t =
  message "when_rebuilding, calling: re_enable_periodic_saving";

  t.periodic_saving_enabled <- true
