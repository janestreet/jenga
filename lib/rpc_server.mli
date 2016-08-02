open! Core.Std
open! Async.Std

val heartbeat_config : Rpc.Connection.Heartbeat_config.t

val go : Config.t -> root_dir:Path.Abs.t -> Progress.t -> unit Deferred.t
