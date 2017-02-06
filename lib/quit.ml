
open! Core
open Async
open! Int.Replace_polymorphic_compare

let is_quitting_ivar = Ivar.create ()

let is_quitting () = not (Ivar.is_empty is_quitting_ivar)

let quit exit_code =
  if not (is_quitting ()) then (
    Ivar.fill is_quitting_ivar ();
    Message.clear_transient();
    Message.message "Quit; exit_code = %d" exit_code;
    Message.flushed () >>> fun () ->
    Shutdown.shutdown exit_code;
  )

let exit exit_code = quit exit_code; Deferred.never ()

let ignore_exn_while_quitting f =
  try_with f >>= function
  | Ok x -> return x
  | Error exn ->
    if is_quitting () then Deferred.never()
    else raise exn

let with_prevent_quitting f =
  let d = Deferred.Or_error.try_with (fun () -> f (Ivar.read is_quitting_ivar)) in
  if not (is_quitting ()) then
    Shutdown.don't_finish_before (d >>| ignore);
  d >>| ok_exn
