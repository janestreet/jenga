
open Core.Std
open Async.Std

val is_loading : unit -> bool

module Spec : sig
  type t
  val ml_file : ml:Path.X.t -> t
  val config_file : conf:Path.X.t -> mls:Path.X.t list -> t
end

val get_env : Spec.t -> Description.Env.t Or_error.t Deferred.t
