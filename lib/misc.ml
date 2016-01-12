
open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

let in_async ~f = (* used by: jem / offline, not jenga *)
  Deferred.unit >>> (fun () ->
    f () >>> (fun n ->
      Shutdown.shutdown n (* only called by: jem / offline, not jenga *)
    )
  );
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())
