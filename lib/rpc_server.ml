
open Core.Std
open Async.Std

let make_periodic_pipe_writer span ~f =
  let r =
    Pipe.init (fun w ->
      let rec loop () =
        let value = f () in
        choose [
          choice (Pipe.closed w) (fun () -> `aborted);
          choice
            (
              Pipe.write w value
              >>= fun () ->
              Clock.after span
            )
            (fun () -> `again)
        ]
        >>= function
        | `aborted -> return ()
        | `again -> loop ()
      in
      loop ()
    )
  in
  return (Ok r)

(* [State.t] is a place holder until the time we really need some connection state. *)
module State : sig
  type t
  val create : unit -> t
end = struct
  type t = unit
  let create () = ()
end

let getenv =
  Rpc.Rpc.implement Rpc_intf.Getenv.rpc (fun _state q -> return (Var.Getenv.run q))

let setenv =
  Rpc.Rpc.implement Rpc_intf.Setenv.rpc (fun _state q -> return (Var.Setenv.run q))

let env_info =
  Rpc.Rpc.implement Rpc_intf.Env_info.rpc (fun _state () -> return (Var.all_registered ()))

let really_go ~root_dir progress =
  let progress_report_span = sec (0.3) in
  let progress_stream =
    Rpc.Pipe_rpc.implement Rpc_intf.Progress_stream.rpc
      (fun (_ : State.t) () ->
         make_periodic_pipe_writer progress_report_span ~f:(fun () ->
           Progress.snap progress
         )
      )
  in
  let dump_tenacious_graph =
    Rpc.Rpc.implement Rpc_intf.Dump_tenacious_graph.rpc
      (fun _state () -> return (Tenacious_lib.Graph.Dump.collect ()))
  in
  let implementations =
    Rpc.Implementations.create ~on_unknown_rpc:`Close_connection ~implementations: [
      progress_stream;
      dump_tenacious_graph;
      getenv;
      setenv;
      env_info;
    ]
  in
  match implementations with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok implementations ->
    let start_server () =
      Tcp.Server.create Tcp.on_port_chosen_by_os ~on_handler_error:`Ignore
        (fun _addr reader writer ->
           Rpc.Connection.server_with_close reader writer ~implementations
             ~connection_state:(fun (_ : Rpc.Connection.t) -> State.create ())
             ~on_handshake_error:`Ignore)
    in
    start_server () >>= fun inet ->
    let port = Tcp.Server.listening_on inet in
    (* write the server lock file - will shutdown if not possible *)
    Server_lock.lock_running_server ~root_dir ~port

let go config ~root_dir progress =
  if Config.no_server config (* jem will interpret port=0 to mean no server is running *)
  then Server_lock.lock_running_server ~root_dir ~port:0
  else really_go ~root_dir progress
