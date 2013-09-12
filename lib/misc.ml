
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let jenga_root_basename =
  match Core.Std.Sys.getenv "JENGA_ROOT_BASENAME" with
  | None -> "jengaroot.ml"
  | Some x -> x

let in_async ~f = (* used by: jem / offline, not jenga *)
  Deferred.unit >>> (fun () ->
    f () >>> (fun n ->
      Shutdown.shutdown n (* only called by: jem / offline, not jenga *)
    )
  );
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())

(* global ref to avoid saving persistent db unless it has been modified *)
let r_persist_is_modified = ref false
let persist_is_modified () = !r_persist_is_modified
let set_persist_is_saved () = r_persist_is_modified := false
let mod_persist x = r_persist_is_modified := true ; x
