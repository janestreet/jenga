
open Core.Std
open Async.Std

val go : root_dir:string -> Build.Progress.t -> unit Deferred.t
