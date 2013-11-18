
open Core.Std
open Async.Std

val is_loading : unit -> bool

val get_env : Path.X.t -> Description.Env.t Or_error.t Deferred.t
