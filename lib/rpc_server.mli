
open Core.Std
open Async.Std

val go : Config.t -> root_dir:string -> Build.Progress.t -> unit Deferred.t
