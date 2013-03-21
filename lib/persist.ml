
open Core.Std
open Async.Std

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
      Message.trace "Persist: load: %s - failed (does not exist; will create)" filename;
      return None
    | `Yes ->
      try_with (fun () ->
        Reader.load_sexp_exn filename Fn.id
      ) >>= function
      | Ok sexp -> return (Some sexp)
      | Error _ ->
        Message.error "Persist: load: %s - failed (corrupted; ignoring)" filename;
        return None

  let try_load ~filename =
    try_load_dont_convert ~filename >>= function
    | None -> return None
    | Some sexp ->
      return (
        try Some (t_of_sexp sexp)
        with _ ->
          Message.error "Persist: load: %s - failed (format changed; ignoring)" filename;
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
      Message.trace "Persist: unchanged so not saving: %s" filename;
      Deferred.unit
    )
    else (
      Message.trace "Persist: saving: %s" filename;
      Writer.save_sexp ~fsync:true ~hum:true filename sexp
    )

end

type t = {
  state : State.t;
  mutable save_now : unit Ivar.t;
  mutable save_done : unit Ivar.t;
}

let create_saving_periodically ~filename save_span =
  State.load ~filename >>= fun state ->
  let t = { state; save_now = Ivar.create (); save_done = Ivar.create ();} in
  don't_wait_for (
    let rec loop () =
      Deferred.any [Clock.after save_span; Ivar.read t.save_now] >>= fun () ->
      State.save_if_changed state ~filename >>= fun () ->
      Ivar.fill t.save_done (); t.save_done <- Ivar.create ();
      loop ()
    in
    loop ()
  );
  return t

let save_now t =
  Ivar.fill t.save_now (); t.save_now <- Ivar.create ();
  Ivar.read t.save_done

let fs_persist t = State.fs_persist t.state
let build_persist t = State.build_persist t.state
