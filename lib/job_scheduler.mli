
open Core.Std
open Async.Std

type t

val create : delay_for_dev:Time.Span.t option -> max_concurrent_jobs:int -> t

val shell : t -> need:string -> dir: Path.t -> prog:string -> args:string list ->
  (unit, [ `non_zero_status | `other_error of exn]) Result.t Deferred.t

(* for scanners *)
val shell_stdout : t -> need:string -> dir: Path.t -> prog:string -> args:string list ->
  (string, [ `non_zero_status | `other_error of exn]) Result.t Deferred.t

