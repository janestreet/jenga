open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

let retry_span = sec 0.5

let terminal_type =
  match Core.Std.Sys.getenv "TERM" with
  | None -> ""
  | Some x -> x

let dont_emit_kill_line = String.(terminal_type = "dumb")

let message =
  if dont_emit_kill_line
  then fun fmt -> ksprintf (fun s -> Core.Std.Printf.printf "%s\r%!" s) fmt
  else fun fmt -> ksprintf (fun s -> Core.Std.Printf.printf "\027[2K%s\r%!" s) fmt

module Connection_error = struct
  type t =
    | Not_running
    | No_server_mode
    | Rpc_failed of string * exn
    | Tcp_failed of string * exn

  let report = function
    | Not_running -> message "jenga not running"
    | No_server_mode -> message "jenga running in -no-server mode"
    | Rpc_failed(server_name, exn) ->
      message !"with_rpc_connection: %s\n%{Exn}" server_name exn
    | Tcp_failed(server_name, exn) ->
      message !"failed to connect with: %s\n%{Exn}" server_name exn

  let may_retry = function
    | Not_running | No_server_mode -> true
    | Rpc_failed _ | Tcp_failed _ -> false

  let exit_code err =
    if may_retry err then 1 else 2
end

let with_connection ~root_dir ~f =
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
        Rpc.Connection.with_client ~host ~port f
        >>| function
        | Ok _ as ok -> ok
        | Error exn -> Error (Connection_error.Rpc_failed(server_name, exn))
      )
      >>| function
      | Ok res -> res
      | Error exn -> Error (Connection_error.Tcp_failed(server_name, exn))

let run_once ~root_dir style =
  with_connection ~root_dir ~f:(fun conn ->
    Rpc.Pipe_rpc.dispatch_exn Rpc_intf.Progress_stream.rpc conn ()
    >>= fun (reader,_id) ->
    Pipe.read reader >>| function
    | `Eof -> failwith "read: end of file"
    | `Ok snap ->
      printf "%s\n" (Progress.Snap.to_string snap style);
  ) >>= function
  | Ok () -> return 0
  | Error err ->
    Connection_error.report err;
    printf "\n";
    return (Connection_error.exit_code err)

let run exit_on_finish ~root_dir style =
  let last_snap = ref None in
  let string_of_last_snap () =
    match !last_snap with
    | None -> "no progress seen!"
    | Some snap -> Progress.Snap.to_string snap style
  in
  let rec loop ~wait =
    begin
      if wait then Clock.after retry_span
      else Deferred.unit
    end >>= fun () ->
    with_connection ~root_dir ~f:(fun conn ->
      let stop = ref false in
      let fresh = ref false in
      don't_wait_for (
        let rec loop q =
          Clock.after (sec 0.5) >>= fun () ->
          if !stop then return ()
          else
          if !fresh then (fresh := false; loop 1)
          else (
            let qmes = String.concat (List.init q ~f:(fun _ -> "?")) in
            message "%s %s" (string_of_last_snap ()) qmes;
            loop (q+1);
          )
        in loop 1
      );
      Rpc.Pipe_rpc.dispatch_exn Rpc_intf.Progress_stream.rpc conn ()
      >>= fun (reader,_id) ->
      Pipe.iter reader ~f:(fun snap ->
        last_snap := Some snap;
        fresh := true;
        let message_string = (Progress.Snap.to_string snap style) in
        message "%s" message_string;
        if exit_on_finish then
          match Progress.Snap.finished snap with
          | None -> Deferred.unit
          | Some res ->
            printf "\n";
            exit (match res with
                  | `Success -> 0
                  | `Failure -> 3)
        else Deferred.unit
      ) >>= fun () ->
      stop := true;
      return ()
    ) >>= function
    | Ok () -> loop ~wait:true
    | Error err ->
      Connection_error.report err;
      if Connection_error.may_retry err && not exit_on_finish
      then loop ~wait:true
      else begin
        printf "\n";
        return (Connection_error.exit_code err)
      end
  in
  loop ~wait:false


let error fmt = ksprintf (fun s -> Core.Std.Printf.eprintf "%s\n%!" s) fmt

let command =
  Command.basic
    Command.Spec.(
      empty
      +> flag "exit-on-finish" no_arg
           ~doc:" exit with appropriate code when the compile is finished"
      +> flag "snapshot" no_arg
           ~doc:" display only a single snapshot of jenga's state"
      +> flag "progress-fraction" no_arg
           ~doc:" display only the built/total fraction"
    )
    ~summary:"monitor jenga running in the current repo."
    ~readme:Progress.readme
    (fun exit_on_finish snapshot progress_fraction () ->
       match Special_paths.discover_root () with
       | Error e ->
         error "%s" (Error.to_string_hum e)
       | Ok root_dir ->
         Path.Repo.set_root root_dir;
         let style = if progress_fraction then `fraction else `jem_style in
         Misc.in_async ~f:(fun () ->
           if snapshot then run_once ~root_dir style
           else run exit_on_finish ~root_dir style
         )
    )
