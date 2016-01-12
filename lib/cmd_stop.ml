open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

module Spec = Command.Spec

let command =
  Command.async_or_error
    Command.Spec.(
      empty
      +> flag "-signal" (optional_with_default "sigterm" string)
           ~doc:"signal (default is sigterm)")
    ~summary:"stop the jenga running in the current repo"
    (fun signal () ->
       let signal = Signal.of_string signal in
       match Special_paths.discover_root () with
       | Error _ as e -> return e
       | Ok root_dir ->
         Path.Repo.set_root root_dir;
         Server_lock.server_location ~root_dir >>| function
         | `server_not_running -> Or_error.error_string "no jenga running"
         | `info info ->
           let server_pid = Server_lock.Info.pid info in
           Ok (Signal.send_exn signal (`Pid server_pid))
    )
