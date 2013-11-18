
open Core.Std
open Async.Std

let make_periodic_pipe_writer span ~aborted ~f =
  let (r,w) = Pipe.create () in
  don't_wait_for (
    let rec loop () =
      let value = f() in
      choose [
        choice aborted (fun () -> `aborted);
        choice (
          Pipe.write w value >>= fun () ->
          Clock.after span
        ) (fun () -> `again)
      ]
      >>= function
      | `aborted -> return ()
      | `again -> loop ()
    in
    loop ()
  );
  return (Ok r)

let really_go ~root_dir progress =
  let progress_report_span = sec (0.3) in
  let progress_stream =
    Rpc.Pipe_rpc.implement Rpc_intf.progress_stream
      (fun () () ~aborted ->
        make_periodic_pipe_writer progress_report_span ~aborted ~f:(fun () ->
          Mon.snap progress
        )
      )
  in
  let implementations =
    Rpc.Implementations.create ~on_unknown_rpc:`Ignore ~implementations: [
      progress_stream;
    ]
  in
  match implementations with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok implementations ->
    let start_server () =
      Tcp.Server.create Tcp.on_port_chosen_by_os ~on_handler_error:`Ignore
        (fun _addr reader writer ->
          Rpc.Connection.server_with_close reader writer ~implementations
            ~connection_state:()
            ~on_handshake_error:`Ignore)
    in
    start_server () >>= fun inet ->
    let port = Tcp.Server.listening_on inet in
    (* write the server lock file - will shutdown it not possible *)
    Server_lock.lock_running_server ~root_dir ~port

let go config ~root_dir progress =
  if Config.no_server config (* jem will interpret port=0 to mean no server is running *)
  then Server_lock.lock_running_server ~root_dir ~port:0
  else really_go ~root_dir progress



