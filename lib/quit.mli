
open! Core
open! Async

val quit : Exit_code.t -> unit
val exit : Exit_code.t -> 'a Deferred.t
val is_quitting : unit -> bool
val ignore_exn_while_quitting : (unit -> 'a Deferred.t) -> 'a Deferred.t

(** [with_prevent_quitting f] runs [f] with a deferred that will be determined when
    [quit] is called. The returned deferred will be determined when the output of [f]
    is determined. If the process quits during the execution of f, then shutdown will
    be delayed until the function is done running (or shutdown is forced, as usual). *)
val with_prevent_quitting : (unit Deferred.t -> unit Deferred.t) -> unit Deferred.t
