open Core
open! Int.Replace_polymorphic_compare
open Async

let retry_span = sec 0.5

let terminal_type =
  match Core.Sys.getenv "TERM" with
  | None -> ""
  | Some x -> x

let dont_emit_kill_line = String.(terminal_type = "dumb")

let message =
  if dont_emit_kill_line
  then fun fmt -> ksprintf (fun s -> Core.Printf.printf "%s\r%!" s) fmt
  else fun fmt -> ksprintf (fun s -> Core.Printf.printf "\027[2K%s\r%!" s) fmt

let run_once ~root_dir style =
  Jenga_client.with_menu_connection_with_detailed_error ~root_dir ~f:(fun cwm ->
    Rpc_intf.Progress_stream.dispatch_multi cwm () >>| ok_exn
    >>= function
    | Error x -> never_returns x
    | Ok (reader,_id) ->
      let reader = Pipe.map reader ~f:ok_exn in
      Pipe.read reader >>| function
      | `Eof -> failwith "read: end of file"
      | `Ok snap -> printf "%s\n" (Progress.Snap.to_string snap style)
  ) >>= function
  | Ok () -> return 0
  | Error err ->
    message "%s" (Jenga_client.Connection_error.to_string err);
    printf "\n";
    return (Jenga_client.Connection_error.exit_code err)

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
    Jenga_client.with_menu_connection_with_detailed_error ~root_dir ~f:(fun cwm ->
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
      Rpc_intf.Progress_stream.dispatch_multi cwm () >>| ok_exn
      >>= function
      | Error x -> never_returns x
      | Ok (reader,_id) ->
        let reader = Pipe.map reader ~f:ok_exn in
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
      message "%s" (Jenga_client.Connection_error.to_string err);
      if Jenga_client.Connection_error.may_retry err && not exit_on_finish
      then loop ~wait:true
      else begin
        printf "\n";
        return (Jenga_client.Connection_error.exit_code err)
      end
  in
  loop ~wait:false

open Command.Let_syntax
let return = Async.return

let command =
  Command.async_or_error
    ~summary:"monitor jenga running in the current repo."
    ~readme:Progress.readme
    [%map_open
      let exit_on_finish =
        flag "exit-on-finish" no_arg
          ~doc:" exit with appropriate code when the compile is finished"
      and snapshot =
        flag "snapshot" no_arg
          ~doc:" display only a single snapshot of jenga's state"
      and progress_fraction =
        flag "progress-fraction" no_arg
          ~doc:" display only the built/total fraction"
      in fun () ->
       match Special_paths.discover_root () with
       | Error e -> return (Error e)
       | Ok root_dir ->
         Path.Repo.set_root root_dir;
         let style = if progress_fraction then `fraction else `monitor_style in
         (if snapshot then run_once ~root_dir style
          else run exit_on_finish ~root_dir style)
         >>= Shutdown.exit
    ]
