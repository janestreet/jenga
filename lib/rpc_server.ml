
open Core.Std
open Async.Std

let make_periodic_pipe_writer span ~aborted ~f =
  let r =
    Pipe.init (fun w ->
      let rec loop () =
        let value = f () in
        choose [
          choice aborted (fun () -> `aborted);
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

let really_go ~root_dir progress =
  let progress_report_span = sec (0.3) in
  let progress_stream =
    Rpc.Pipe_rpc.implement Rpc_intf.Progress_stream.rpc
      (fun (_ : Progress.Update.State.t) () ~aborted ->
         make_periodic_pipe_writer progress_report_span ~aborted ~f:(fun () ->
           Progress.snap progress
         )
      )
  in
  let update_stream =
    Rpc.Pipe_rpc.implement Rpc_intf.Update_stream.rpc
      (fun state () ~aborted ->
         make_periodic_pipe_writer progress_report_span ~aborted ~f:(fun () ->
           Progress.updates progress state
         )
      )
  in
  let implementations =
    Rpc.Implementations.create ~on_unknown_rpc:`Close_connection ~implementations: [
      progress_stream;
      update_stream;
    ]
  in
  match implementations with
  | Error (`Duplicate_implementations _descrs) -> assert false
  | Ok implementations ->
    let start_server () =
      Tcp.Server.create Tcp.on_port_chosen_by_os ~on_handler_error:`Ignore
        (fun _addr reader writer ->
           Rpc.Connection.server_with_close reader writer ~implementations
             ~connection_state:(fun (_ : Rpc.Connection.t) ->
               Progress.Update.State.create ())
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



