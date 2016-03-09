
open! Core.Std
open! Async.Std

val enqueue : (unit -> 'a Deferred.t) -> 'a Deferred.t
