
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

type config  = {
  full : bool;
}

let message fmt = ksprintf (fun s -> Printf.printf "%s\r%!" s) fmt

let run config =

  let root_dir = discover_root() in

  let last_progress = ref None in

  let string_of_progress progress =
    let (top,bot) = Build.Progress.Counts.fraction progress in
    let fraction_string = sprintf "%d / %d" top bot in
    if config.full
    then sprintf "%s (%s)" (Build.Progress.Counts.to_string progress) fraction_string
    else fraction_string
  in

  let suck_progress_pipe conn =
    Rpc.Pipe_rpc.dispatch_exn Rpc_intf.progress_stream conn () >>= fun (reader,_id) ->
    Pipe.iter_without_pushback reader ~f:(fun progress ->
      last_progress := Some progress;
      message "progress: %s" (string_of_progress progress)
    )
  in

  let poll_for_connection ~retry =
    Server_lock.server_location ~root_dir >>= function
    | `server_not_running ->
      message "%s : not running%s"
        root_dir
        (match !last_progress with
        | None -> ""
        | Some progress ->
          sprintf " (last: %s)" (string_of_progress progress)
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
            message "with_rpc_connection: %s\n%s" server_name (Exn.to_string exn);
            return false
          | Ok conn ->
            suck_progress_pipe conn >>= fun () ->
            message "lost connection with: %s" server_name;
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


let main config =
  Deferred.unit >>> (fun () ->
    run config >>> (fun n ->
      Shutdown.shutdown n
    )
  );
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())


module Spec = Command.Spec
let (+>) = Spec.(+>)

let full =
  Spec.step (fun m x -> m ~full:x)
  +> Spec.flag "full" Spec.no_arg
    ~doc:" Full progress breakdown"

let command_line () =
  Command.run (
    Command.basic (full)
      ~summary:"See progress of jenga running in the current repo."
      (fun ~full () -> main {full})
  )
