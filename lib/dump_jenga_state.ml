
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let message fmt = ksprintf (fun s -> Printf.printf "%s%!" s) fmt

let run ~root_dir =

  message "1";

  let send_dump_request conn =

    Rpc.Rpc.dispatch_exn Rpc_intf.dump_progress_state conn () >>= fun () ->
    message "6";
    return ()
  in

  Server_lock.server_location ~root_dir >>= function
  | `server_not_running ->
    message "jenga (not running)";
    return 1

  | `info info ->

    message "2";

    let host = Server_lock.Info.host info in
    let port = Server_lock.Info.port info in
    let server_name = sprintf "%s:%d" host port in
    let where_to_connect = Tcp.to_host_and_port host port in

    try_with (fun () ->
      Tcp.with_connection where_to_connect (fun _ reader writer ->

        message "3";

        Rpc.Connection.create reader writer ~connection_state:() >>= function
        | Error exn ->
          (* when does this ever come?.. *)
          message "with_rpc_connection: %s\n%s" server_name (Exn.to_string exn);
          return false
        | Ok conn ->
          message "4";
          send_dump_request conn >>= fun () ->
          message "5";
          return true
      )
    )

    >>= function
    | Ok ok ->
      if ok then (
        message "OK true"; return 1
      ) else (
        message "OK false"; return 1
      )

    | Error exn ->
      message "failed to connect with: %s\n%s" server_name (Exn.to_string exn);
      return 2


module Spec = Command.Spec
let (+>) = Spec.(+>)
let (++) = Spec.(++)

let error fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let command_line () =
  Command.run (
    Command.basic Spec.empty
      ~summary:"dump_jenga_state of the jenga running in the current repo."
      (fun () ->

        match Path.Root.discover() with | `cant_find_root ->
          error "Cant find '%s' in start-dir or any ancestor dir"
            Misc.jenga_root_basename
        | `ok ->
          let root_dir = Path.to_absolute_string Path.the_root in
          Misc.in_async ~f:(fun () ->
            run ~root_dir
          )
      )
  )
