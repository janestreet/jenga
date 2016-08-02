open Core.Std
open Async.Std

module Connection_error : sig
  type t =
    | Not_running
    | No_server_mode
    | Rpc_menu_failed of string * Error.t
    | Rpc_failed of string * exn
    | Tcp_failed of string * exn

  val to_string : t -> string
  val may_retry : t -> bool
  val exit_code : t -> int
  val to_error : t -> Error.t
end

val with_menu_connection_with_detailed_error
   : root_dir:Path.Abs.t
  -> f:(Versioned_rpc.Connection_with_menu.t -> 'a Deferred.t)
  -> ('a, Connection_error.t) Result.t Deferred.t

val with_menu_connection
   : root_dir:Path.Abs.t
  -> f:(Versioned_rpc.Connection_with_menu.t -> 'a Deferred.t)
  -> 'a Or_error.t Deferred.t
