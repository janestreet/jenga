
open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std

let is_quitting_ref = ref false

let quit exit_code =
  if not !is_quitting_ref then (
    is_quitting_ref := true;
    Message.message "Quit; exit_code = %d" exit_code;
    Message.flushed () >>> fun () ->
    Shutdown.shutdown exit_code;
  )

let is_quitting () = !is_quitting_ref

let ignore_exn_while_quitting f =
  try_with f >>= function
  | Ok x -> return x
  | Error exn ->
    if !is_quitting_ref then Deferred.never()
    else raise exn
