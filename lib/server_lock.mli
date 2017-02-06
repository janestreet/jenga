(** This module prevents running multiple jengas in the same directory, as that would
    not work.
    It also writes to disk where the jenga server is listening, which can be queried
    by anything that needs to find the server. *)

open! Core
open! Async

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
