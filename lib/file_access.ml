
open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std

let the_open_file_throttle =
  (* There is no point having this as anything other than a global because
     it must be shared pervasively across all jenga code - builtin & user extended
  *)
  Throttle.create ~continue_on_error:true ~max_concurrent_jobs:500

let enqueue f =
  Throttle.enqueue the_open_file_throttle (fun () ->
    f ()
  )
