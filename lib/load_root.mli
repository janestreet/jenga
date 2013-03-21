
open Core.Std
open Async.Std

val get_env : Path.LR.t -> Description.Env.t Or_error.t Deferred.t
