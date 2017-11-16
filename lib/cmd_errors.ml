open Core
open Async
open! Int.Replace_polymorphic_compare

let puts fmt = ksprintf (fun s -> printf !"%s\n%!" s) fmt

(* This output isn't very pretty. But [jenga errors] is mostly just for dev. *)
let show_error_state n (xs:Reportable.t) ~sexp =
  match sexp with
  | true ->
    printf !"%{sexp#hum:int * Reportable.t}\n" (n, xs)
  | false ->
    puts "----------------------------------------------------------------------";
    puts "show_error_state(%d), #errors=%d" n (Reportable.length xs);
    let i = ref 0 in
    Reportable.iter xs ~f:(fun x ->
      incr i;
      puts !"(%d) Error=#%{sexp:Reportable.Error.Id.t} Goal=%s" (!i)
        (Reportable.Error.id x)
        (Reportable.Error.goal_string x)
      ;
      match Reportable.Error.contents x with
      | `Internal lines -> List.iter lines ~f:(fun line -> puts "Internal: %s" line)
      | `Command_failed js ->
        let module JS = Job_summary in
        let lines =
          List.map (JS.to_stdout_lines js) ~f:(fun x -> ("stdout",x)) @
          List.map (JS.to_stderr_lines js) ~f:(fun x -> ("stderr",x))
        in
        puts "%s" (JS.build_message js);
        puts "%s" (JS.command_message js);
        List.iter lines ~f:(fun (tag,line) -> puts "%s> %s" tag line);
        puts "exit: %s" (JS.status_message js)
    )

let show_error_pipe_changes = fun
  ~(initial:Reportable.Snap.t)
  ~(updates:Reportable.Update.t Pipe.Reader.t)
  ~sexp
  ()
  ->
    let errs = Reportable.of_snap ~name:"error-watcher" initial in
    show_error_state 0 errs ~sexp;
    puts "(watching for error updates)";
    let n = ref 0 in
    Pipe.iter updates ~f:(fun update ->
      incr n;
      Reportable.update errs update;
      show_error_state (!n) errs ~sexp;
      Deferred.unit
    ) >>| fun () ->
    puts "(no longer watching for error updates)"

let watch_error_changes ~root_dir ~sexp () : [`retry|`giveup of int] Deferred.t =
  Jenga_client.with_menu_connection_with_detailed_error ~root_dir ~f:(fun cwm ->
    begin
      Rpc_intf.Error_pipe.dispatch_multi cwm ()
      >>| ok_exn
      >>| function
      | Ok x -> x
      | Error nr -> never_returns nr
    end
    >>= fun (initial,updates,_metadata) ->
    let updates = Pipe.map updates ~f:ok_exn in
    puts "(connected to jenga)";
    show_error_pipe_changes ~initial ~updates ~sexp ()
  ) >>| function
  | Ok () ->
    puts "(lost connection with jenga)";
    `retry
  | Error err ->
    puts "%s" (Jenga_client.Connection_error.to_string err);
    if Jenga_client.Connection_error.may_retry err
    then `retry
    else `giveup (Jenga_client.Connection_error.exit_code err)

let run_watch_errors ~sexp =
  return (Special_paths.discover_root ()) >>=? fun root_dir ->
  let retry_span = sec 0.5 in
  let rec loop () =
    watch_error_changes ~root_dir ~sexp () >>= function
    | `retry -> Clock.after retry_span >>= loop
    | `giveup code -> return code
  in
  loop () >>= fun i ->
  return (Or_error.errorf "Got some exit code: %d" i)

open Command.Let_syntax

let command =
  Command.async_or_error
    ~summary:"connect to running jenga, and watch the error pipe"
    [%map_open
      let sexp =
        flag "-sexp" no_arg
          ~doc:" display the errors as sexp instead of human readable text"
      in fun () -> run_watch_errors ~sexp
    ]
