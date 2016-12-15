(** A global throttle for the file system operations in jenga, to avoid exceeding
    the open file descriptor limits. *)

open! Core.Std
open! Async.Std

val enqueue : (unit -> 'a Deferred.t) -> 'a Deferred.t
