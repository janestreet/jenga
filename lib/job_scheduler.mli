
open Core.Std
open Async.Std

type t

val create : delay_for_dev:Time.Span.t option -> max_concurrent_jobs:int -> t

val shell : t -> need:string -> dir: Path.t -> prog:string -> args:string list ->
  (unit,
   [
   | `non_zero_status of Core_extended.Std.Shell.Process.status
   | `other_error of exn
   ]
  ) Result.t Deferred.t
