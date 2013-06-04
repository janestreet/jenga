
(* jem - jenga monitor *)

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

let retry_span = sec 0.5

type config  = {
  brief : bool;
}

let message fmt = ksprintf (fun s -> Printf.printf "\027[2K%s\r%!" s) fmt

let run config =

  let root_dir = discover_root() in

  let last_progress = ref None in

  let string_of_progress progress =
    let (top,bot) = Build.Progress.Counts.fraction progress in
    let fraction_string = sprintf "%d / %d" top bot in
    if config.brief
    then fraction_string
    else sprintf "%s (%s)" (Build.Progress.Counts.to_string progress) fraction_string
  in

  let string_of_last_progress () =
    match !last_progress with
    | None -> "no progress seen!"
    | Some progress -> string_of_progress progress
  in

  let suck_progress_pipe conn =
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
            message "%s %s" (string_of_last_progress ()) qmes;
            loop (q+1);
          )
      in loop 1
    );
    Rpc.Pipe_rpc.dispatch_exn Rpc_intf.progress_stream conn () >>= fun (reader,_id) ->
    Pipe.iter_without_pushback reader ~f:(fun progress ->
      last_progress := Some progress;
      fresh := true;
      message "%s" (string_of_progress progress);
    ) >>= fun () ->
    stop := true;
    return ()
  in

  let poll_for_connection ~retry =
    Server_lock.server_location ~root_dir >>= function
    | `server_not_running ->
      message "%s (not running)" (string_of_last_progress ()); (*root_dir*)
      retry()
    | `info info ->
      let host = Server_lock.Info.host info in
      let port = Server_lock.Info.port info in
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

let brief =
  Spec.step (fun m x -> m ~brief:x)
  +> Spec.flag "brief" Spec.no_arg
    ~doc:" only display the omake-style progress fraction (done / total)"

let command_line () =
  Command.run (
    Command.basic (brief)
      ~summary:
"Jenga monitor - monitor jenga running in the current repo."
      ~readme:(fun () -> String.concat ~sep:"\n" (
        List.concat [
["Jem connects to a jenga instance running in the current repo,
(or waits until one is started), and displays a stream of progress
reports indicating the status of each target being considered.

The number of targets in each of the following categorys is shown:
"];
           Build.Progress.readme;

["
Additionally, an omake-style progress fraction (done / total),
where (done = built + source), is displayed."];
          ]))
      (fun ~brief () -> main {brief})
  )
