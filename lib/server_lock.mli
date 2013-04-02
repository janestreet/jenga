
open Core.Std
open Async.Std

val lock_running_server : root_dir:string -> port:int -> unit Deferred.t

val server_location : root_dir:string -> [
| `server_not_running
| `hostname_and_port of string * int
] Deferred.t
