
open! Core.Std
open! Async.Std

val is_loading : unit -> bool

module Spec : sig
  type t
  val ml_file : ml:Path.t -> t
  val config_file : conf:Path.t -> mls:Path.t list -> t
end

val get_env : Config.t -> Spec.t -> Env.t Or_error.t Deferred.t
