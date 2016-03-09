
open! Core.Std
open! Async.Std

val in_async : f:(unit -> int Deferred.t) -> 'a
