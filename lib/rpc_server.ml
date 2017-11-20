open Core
open Async
open! Int.Replace_polymorphic_compare

let heartbeat_config =
  Rpc.Connection.Heartbeat_config.create
    ~timeout: Time_ns.Span.max_value
    ~send_every: (Time_ns.Span.of_sec 10.)

let make_periodic_pipe_writer span ~f =
  let r =
    Pipe.create_reader ~close_on_exception:true (fun w ->
      let rec loop () =
        if Pipe.is_closed w then return ()
        else begin
          choose [
            choice (Pipe.closed w) (fun () -> `aborted);
            choice (Pipe.write w (f ()) >>= fun () -> Clock.after span) (fun () -> `again)
          ]
          >>= function
          | `aborted -> return ()
          | `again -> loop ()
        end
      in
      loop ()
    )
  in
  return (Ok r)

module State = struct
  type t = { progress : Progress.t }
end

let error_pipe_handler (_ : State.t) () =
  let trace = Message.trace "%s" in
  Reportable.snap_with_updates ~trace Progress.the_reportable_errors
;;

let error_pipe =
  Rpc.State_rpc.implement Rpc_intf.Error_pipe.rpc (fun state () ->
    return (Ok (error_pipe_handler state ())))

let getenv =
  Rpc.Rpc.implement Rpc_intf.Getenv.rpc (fun _state q -> return (Var.Getenv.run q))

let setenv =
  Rpc.Rpc.implement Rpc_intf.Setenv.rpc (fun _state q -> return (Var.Setenv.run q))

let env_info =
  Rpc.Rpc.implement Rpc_intf.Env_info.rpc (fun _state () -> return (Var.all_registered ()))

let progress_stream =
  let progress_report_span = sec (0.3) in
  Rpc.Pipe_rpc.implement Rpc_intf.Progress_stream.rpc
    (fun (state : State.t) () ->
       make_periodic_pipe_writer progress_report_span ~f:(fun () ->
         Progress.snap state.progress
       )
    )

let dump_tenacious_graph =
  Rpc.Rpc.implement Rpc_intf.Dump_tenacious_graph.rpc
    (fun _state () -> return (Tenacious_lib.Graph.Dump.collect ()))

let all_rpcs =
  Versioned_rpc.Menu.add
    [
      progress_stream;
      error_pipe;
      dump_tenacious_graph;
      getenv;
      setenv;
      env_info;
    ]

let versions =
  List.map all_rpcs ~f:Rpc.Implementation.description
  |> List.map ~f:(fun { name; version } -> name, version)
  |> String.Map.of_alist_multi
  |> Map.map ~f:Int.Set.of_list

let really_go ~root_dir progress =
  let implementations =
    Rpc.Implementations.create
      ~on_unknown_rpc:`Close_connection
      ~implementations:all_rpcs
  in
  match implementations with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok implementations ->
    let start_server () =
      Tcp.Server.create Tcp.Where_to_listen.of_port_chosen_by_os ~on_handler_error:`Ignore
        (fun _addr reader writer ->
           Rpc.Connection.server_with_close reader writer ~implementations
             ~heartbeat_config
             ~connection_state:(fun (_ : Rpc.Connection.t) -> { progress })
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
