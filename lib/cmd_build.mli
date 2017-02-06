open! Core
open! Async

(** Entry point to run Jenga as directed by the command line *)

val command
  :  toplevel:bool           (** [false] for [jenga build] *)
  -> run:(Config.t -> unit)
  -> unit
  -> Command.t

val config_param : Config.t Command.Param.t
