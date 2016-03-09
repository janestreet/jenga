
open! Core.Std
open! Async.Std

(* [Action.t] - things which a build-rule can do:
   - [shell] run a process (a [Job.t])
   - [save] save a file

   Although some actions can be run directly by the jenga process (i.e. file-save), it is
   always possible to obtain the equivalent [Job.t], which is important when we want to
   externalize the action, for example, when extracting a Makefile. *)
type t [@@deriving sexp_of]

val job : t -> Job.t

val run : t ->
  message:(unit->unit) ->
  output: 'a Job.Output.t ->
  putenv: (string * string option) list ->
  progress: Progress.t ->
  need: string ->
  ('a,
   [
   | `command_failed of Job_summary.t
   | `other_error of exn
   ]
  ) Result.t Deferred.t

val of_job : Job.t -> t
val process : dir:Path.t -> prog:string -> args:string list -> ignore_stderr:bool -> t
val save : ?chmod_x:unit -> string -> target:Path.t -> t
