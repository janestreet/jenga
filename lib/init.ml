
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let jenga_root_basename =
  match Core.Std.Sys.getenv "JENGA_ROOT_BASENAME" with
  | None -> "jengaroot.ml"
  | Some x -> x

let in_async ~f =
  Deferred.unit >>> (fun () ->
    f () >>> (fun n ->
      Shutdown.shutdown n
    )
  );
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())
