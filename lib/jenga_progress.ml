
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std


(* copied from run.ml *)
let jenga_root_basename =
  match Core.Std.Sys.getenv "JENGA_ROOT" with
  | None -> "JengaRoot.ml"
  | Some x -> x

let discover_root() =
  match Repo_root.discover ~marker:jenga_root_basename with
  | `cant_find_root ->
    failwithf "Cant find '%s' in start-dir or any ancestor dir"
      jenga_root_basename ()
  | `ok root -> root



let retry_span = sec 1.0

let run () =

  let root_dir = discover_root() in

  let message_line fmt =
    ksprintf (fun s ->
      Printf.printf "%s: %s\r%!" root_dir s
    ) fmt
  in

  let last_fraction = ref None in

  let string_of_fraction (top,bot) = sprintf "%d / %d" top bot in

  let suck_progress_pipe conn =
    Rpc.Pipe_rpc.dispatch_exn Rpc_intf.progress_stream conn () >>= fun (reader,_id) ->
    Pipe.iter_without_pushback reader ~f:(fun fraction ->
      last_fraction := Some fraction;
      message_line "progress: %s" (string_of_fraction fraction)
    )
  in

  let poll_for_connection ~retry =
    Server_lock.server_location ~root_dir >>= function

    | `server_not_running ->
      message_line "jenga not running%s"
        (match !last_fraction with
        | None -> ""
        | Some fraction ->
          sprintf " (last progress: %s)" (string_of_fraction fraction)
        );
      retry()

    | `hostname_and_port (hostname,port) ->
      let server_name = sprintf "%s:%d" hostname port in
      let where_to_connect = Tcp.to_host_and_port hostname port in
      try_with (fun () ->
        Tcp.with_connection where_to_connect (fun _ reader writer ->
          Rpc.Connection.create reader writer ~connection_state:() >>= function
          | Error exn ->
            (* when does this ever come?.. *)
            message_line "with_rpc_connection, error: %s" (Exn.to_string exn);
            return ()
          | Ok conn ->
            (*message_line "connected to jenga server: %s " server_name;*)
            suck_progress_pipe conn
        )
      )
      >>= function

      | Ok () ->
        message_line "lost connection with jenga server: %s" server_name;
        retry()

      | Error _ ->
        message_line "failed to connect to jenga server: %s" server_name;
        retry()

  in
  let rec retry () =
    Clock.after retry_span >>= fun () ->
    poll_for_connection ~retry
  in
  poll_for_connection ~retry


let main () =
  Deferred.unit >>> (fun () ->
    run () >>> (fun n ->
      Shutdown.shutdown n
    )
  );
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())
