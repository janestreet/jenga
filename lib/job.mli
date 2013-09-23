
open Core.Std
open Async.Std

exception Shutdown

val external_jobs_run : Effort.Counter.t

module Output : sig

  (* Policy for treating the command output *)
  type 'a t

  (* no stdout expected; command being run for effect *)
  val ignore : unit t

  (* stdout expected & wanted *)
  val stdout : string t

  (* given an ouput policy of a specific type, cause no output *)
  val none : 'a t -> 'a

end

val run :
  config:Config.t ->
  need:string ->
  putenv : (string * string) list ->
  xaction : Description.Xaction.t ->
  output : 'a Output.t ->
  ('a, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t
