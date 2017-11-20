open Core
open Async
open! Int.Replace_polymorphic_compare

module Connection_error = struct
  type t =
    | Not_running
    | No_server_mode
    | Rpc_menu_failed of string * Error.t
    | Rpc_failed of string * exn
    | Tcp_failed of string * exn
  [@@deriving sexp_of]

  let to_string = function
    | Not_running -> "jenga not running"
    | No_server_mode -> "jenga running in -no-server mode"
    | Rpc_menu_failed(server_name, error) ->
      sprintf !"with_rpc_connection: %s\n%{sexp:Error.t}" server_name error
    | Rpc_failed(server_name, exn) ->
      sprintf !"with_rpc_connection: %s\n%{Exn}" server_name exn
    | Tcp_failed(server_name, exn) ->
      sprintf !"failed to connect with: %s\n%{Exn}" server_name exn

  let may_retry = function
    | Not_running | No_server_mode -> true
    | Rpc_menu_failed _ | Rpc_failed _ | Tcp_failed _ -> false

  let exit_code err =
    if may_retry err then 1 else 2

  let to_error e = match e with
    | Not_running | No_server_mode -> Error.of_string (to_string e)
    | Rpc_menu_failed (_,error) -> error
    | Rpc_failed _ | Tcp_failed _ -> Error.t_of_sexp (sexp_of_t e)
end

let with_menu_connection_with_detailed_error ~root_dir ~f =
  Server_lock.server_location ~root_dir >>= function
  | `server_not_running -> return (Error Connection_error.Not_running)
  | `info info ->
    let host = Server_lock.Info.host info in
    let port = Server_lock.Info.port info in
    let host = if String.(host = Unix.gethostname()) then "localhost" else host in
    match port with
    | 0 -> return (Error Connection_error.No_server_mode)
    | _ ->
      let server_name = sprintf "%s:%d" host port in
      try_with (fun () ->
        Rpc.Connection.with_client
          ~heartbeat_config:Rpc_server.heartbeat_config
          (Tcp.Where_to_connect.of_host_and_port {host; port})
          (fun conn ->
             Versioned_rpc.Connection_with_menu.create conn >>=? fun cwm ->
             f cwm >>| fun x ->
             Ok x
          )
        >>| function
        | Ok (Ok _ as ok) -> ok
        | Ok (Error error) -> Error (Connection_error.Rpc_menu_failed(server_name, error))
        | Error exn -> Error (Connection_error.Rpc_failed(server_name, exn))
      )
      >>| function
      | Ok res -> res
      | Error exn -> Error (Connection_error.Tcp_failed(server_name, exn))
;;

let with_menu_connection ~root_dir ~f =
  with_menu_connection_with_detailed_error ~root_dir ~f
  >>| function
  | Ok _ as ok -> ok
  | Error e -> Error (Connection_error.to_error e)
;;
