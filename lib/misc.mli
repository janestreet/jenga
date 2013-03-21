
open Core.Std
open Async.Std

val try_load : string -> (Sexp.t -> 'a) -> Path.t -> 'a Or_error.t Deferred.t
val try_save : 'a -> string -> ('a -> Sexp.t) -> Path.t -> unit Or_error.t Deferred.t
