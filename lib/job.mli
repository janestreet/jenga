
open Core.Std
open Async.Std

exception Shutdown

val external_jobs_run : Effort.Counter.t

module Run_external_job : sig

  val shell :
    Config.t ->
    need:string ->
    rel_path_semantics:Forker.Rel_path_semantics.t ->
    putenv : (string * string) list ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    (unit, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t

  val shell_stdout :
    Config.t ->
    need:string ->
    rel_path_semantics:Forker.Rel_path_semantics.t ->
    putenv : (string * string) list ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    (string, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t

end

module Run_now : sig
  val run_action_now : Description.Action.t -> unit Deferred.t
  val run_action_now_stdout : Description.Action.t -> string Deferred.t
end
