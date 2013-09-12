
open Core.Std
open Async.Std

val quit : Exit_code.t -> unit
val is_quitting : unit -> bool
val ignore_exn_while_quitting : (unit -> 'a Deferred.t) -> 'a Deferred.t

