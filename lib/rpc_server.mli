open! Core.Std
open! Async.Std

val go : Config.t -> root_dir:Path.Abs.t -> Progress.t -> unit Deferred.t
