
open Core.Std
open Async.Std

val jenga_root_basename : string

val in_async : f:(unit -> int Deferred.t) -> 'a
