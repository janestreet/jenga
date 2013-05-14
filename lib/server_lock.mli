
open Core.Std
open Async.Std

val lock_running_server : root_dir:string -> port:int -> unit Deferred.t

module Info : sig
  type t
  val host : t -> string
  val port : t -> int
end

val server_location : root_dir:string -> [
| `server_not_running
| `info of Info.t
] Deferred.t
