
open Core.Std
open Async.Std

val lock_running_server : root_dir:Path.Abs.t -> port:int -> unit Deferred.t

module Info : sig
  type t
  val host : t -> string
  val port : t -> int
  val pid : t -> Pid.t
end

val server_location : root_dir:Path.Abs.t -> [
| `server_not_running
| `info of Info.t
] Deferred.t
