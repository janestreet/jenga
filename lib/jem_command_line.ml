
(* jem - jenga monitor *)

open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let retry_span = sec 0.5

let terminal_type =
  match Core.Std.Sys.getenv "TERM" with
  | None -> ""
  | Some x -> x

let dont_emit_kill_line = String.(terminal_type = "dumb")

let message =
  if dont_emit_kill_line
  then fun fmt -> ksprintf (fun s -> Printf.printf "%s\r%!" s) fmt
  else fun fmt -> ksprintf (fun s -> Printf.printf "\027[2K%s\r%!" s) fmt

let run ~root_dir =

  let string_of_snap snap =
    Progress.Snap.to_string snap `jem_style
  in

  let last_snap = ref None in
  let string_of_last_snap () =
    match !last_snap with
    | None -> "no progress seen!"
    | Some snap -> string_of_snap snap
  in

  let suck_snap_pipe conn =
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
    Rpc.Pipe_rpc.dispatch_exn Rpc_intf.progress_stream conn () >>= fun (reader,_id) ->
    Pipe.iter_without_pushback reader ~f:(fun snap ->
      last_snap := Some snap;
      fresh := true;
      message "%s" (string_of_snap snap);
    ) >>= fun () ->
    stop := true;
    return ()
  in

  let poll_for_connection ~retry =
    Server_lock.server_location ~root_dir >>= function
    | `server_not_running ->
      message "%s (not running)" (string_of_last_snap ()); (*root_dir*)
      retry()
    | `info info ->
      let host = Server_lock.Info.host info in
      let port = Server_lock.Info.port info in
      let host = if String.(host = Unix.gethostname()) then "localhost" else host in
      match port with
      | 0 ->
        message "%s (jenga running in -no-server mode)" (string_of_last_snap ());
        retry();

      | _ ->
      let server_name = sprintf "%s:%d" host port in
      let where_to_connect = Tcp.to_host_and_port host port in
      try_with (fun () ->
        Tcp.with_connection where_to_connect (fun _ reader writer ->
          Rpc.Connection.create reader writer ~connection_state:() >>= function
          | Error exn ->
            (* when does this ever come?.. *)
            message "with_rpc_connection: %s\n%s" server_name (Exn.to_string exn);
            return false
          | Ok conn ->
            suck_snap_pipe conn >>= fun () ->
            (*message "lost connection with: %s" server_name;*)
            return true
        )
      )
      >>= function
      | Ok ok -> if ok then retry() else return 1
      | Error exn ->
        message "failed to connect with: %s\n%s" server_name (Exn.to_string exn);
        return 2
  in
  let rec retry () =
    Clock.after retry_span >>= fun () ->
    poll_for_connection ~retry
  in
  poll_for_connection ~retry

let error fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let main () =
  Command.run (
    Command.basic (Command.Spec.empty)
      ~summary:"Jenga monitor - monitor jenga running in the current repo."
      ~readme:Progress.readme
      (fun () ->
        match Path.Root.discover() with | `cant_find_root ->
          error "Cant find '%s' or '%s' in start-dir or any ancestor dir"
            Misc.jenga_conf_basename
            Misc.jenga_root_basename
        | `ok ->
          let root_dir = Path.Rel.to_absolute_string Path.Rel.the_root in
          Misc.in_async ~f:(fun () ->
            run ~root_dir
          )
      )
  )
