(** This module starts the rpc server (unless configured not to), and
    ties together the modules that implements the various rpcs. *)

open! Core
open! Async

val heartbeat_config : Rpc.Connection.Heartbeat_config.t

val versions : Int.Set.t String.Map.t

val go : Config.t -> root_dir:Path.Abs.t -> Progress.t -> unit Deferred.t
