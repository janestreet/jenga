
open Core
open! Int.Replace_polymorphic_compare

let terminal_type =
  match Core.Sys.getenv "TERM" with
  | None -> ""
  | Some x -> x

let config_param : Config.t Command.Param.t =
  let open Command.Let_syntax in
  let%map_open
    j_number =
    let formula,default =
      match System.num_cpus_if_known with
      | None -> "", 1
      | Some cpus ->
        " = max (#cpus-1) 1", Int.max (cpus-1) 1
    in
    flag "j" (optional_with_default default int)
      ~doc:(sprintf "<jobs> parallel jobs, def: %d%s" default formula)

  and f_number =
    let formula,default =
      match System.num_cpus_if_known with
      | None -> "", 1
      | Some cpus ->
        " = (#cpus-1)/4 + 1",  (cpus - 1) / 4 + 1
    in
    flag "f" (optional_with_default default int)
      ~doc:(sprintf "<forkers> forker processes, def: %d%s" default formula)

  and tenacious_concurrency =
    flag "-internal-concurrency" (optional int)
      ~doc:(sprintf "<n> Mainly for debug.")

  and d_number =
    let formula,default = (* same defaults as for -j *)
      match System.num_cpus_if_known with
      | None -> "", 1
      | Some cpus ->
        " = max (#cpus-1) 1", Int.max (cpus-1) 1
    in
    flag "max-par-digest" (optional_with_default default int)
      ~doc:(sprintf "<digests> parallel digests, def: %d%s" default formula)

  and poll_forever =
    flag "poll-forever" ~aliases:["P"] no_arg
      ~doc:" poll filesystem for changes (keep polling forever)"

  and stop_on_first_error =
    flag "stop-on-first-error" no_arg
      ~doc:" stop when first error encounterd"

  and verbose =
    flag "verbose" ~aliases:["--verbose"] no_arg
      ~doc:" Show full command string, and stdout/stderr from every command run"

  and show_actions_run =
    flag "show-actions-run" no_arg
      ~doc:" Show actions being run; and the reason why"

  and show_actions_run_verbose =
    flag "show-actions-run-verbose" no_arg
      ~doc:" Be more verbose about the reason actions are run"

  and show_checked =
    flag "show-checked" no_arg
      ~doc:" Show actions which are checked, but not run"

  and show_considering =
    flag "show-considering" no_arg
      ~doc:" Mainly for debug. Shows when deps are considered/re-considered (rather verbose)"

  and show_error_dependency_paths =
    flag "show-error-dependency-paths" no_arg
      ~doc:" show dependency paths from root goal to each error (like exception stack traces)"

  and show_memory_allocations =
    flag "show-memory-allocations" no_arg
      ~doc:" Show information about memory allocation at the end of the build"

  and show_reconsidering =
    flag "show-reconsidering" no_arg
      ~doc:" Mainly for debug. Show when deps are re-considered"

  and show_reflecting =
    flag "show-reflecting" no_arg
      ~doc:" Mainly for debug. Shows when deps are being reflected"

  and show_trace_messages =
    flag "trace" no_arg
      ~doc:" switch on some additional trace messages"

  and prefix_time =
    flag "time" no_arg
      ~doc:" prefix all messages with the time"

  and report_long_cycle_times =
    flag "report-long-cycle-times" (optional int)
      ~doc:"<ms> (for development) pass to Scheduler.report_long_cycle_times"

  and progress =
    flag "progress" ~aliases:["--progress"] no_arg
      ~doc:" Show periodic progress report"

  and path_to_jenga_conf =
    flag "-path-to-jenga-conf" (optional string)
      ~doc:(sprintf " Specify path to <jenga.conf>; The repo_root is taken to be CWD.")

  and brief_error_summary =
    flag "brief-error-summary" no_arg
      ~doc:" Don't repeat stdout/stderr from failing commands in error summary"

  and no_server =
    flag "no-server" no_arg
      ~doc:" Don't start jenga server (queries to the server won't work)"

  and minor_heap_size =
    let default = 50 in
    flag "minor-heap-size" (optional_with_default default int)
      ~doc:(sprintf "<Mb> (default = %d Mb)" default)

  and major_heap_increment =
    let default = 200 in
    flag "major-heap-increment" (optional_with_default default int)
      ~doc:(sprintf "<Mb> (default = %d Mb)" default)

  and space_overhead =
    let default = 100 in
    flag "space-overhead" (optional_with_default default int)
      ~doc:(sprintf "<percent> (default = %d)" default)

  and no_notifiers =
    flag "no-notifiers" no_arg
      ~doc:" Disable filesystem notifiers (inotify); polling wont work"

  and no_fs_triggers =
    flag "no-fs-triggers" no_arg
      ~doc:(" For testing, only valid without notifiers. Makes jenga more strict by \
              failing instead of potentially recovering when the file system changes.")

  and sandbox_actions =
    flag "-sandbox-actions" no_arg
      ~doc:" Check dependencies are right by running actions in a part of the filesystem \
            where only the declared dependencies are available"

  and anon_goals =
    anon (sequence ("GOAL" %: string))

  in
  {
    Config.
    j_number;
    f_number;
    d_number;
    tenacious_concurrency;
    poll_forever;
    stop_on_first_error;
    verbose;
    show_memory_allocations;
    show_actions_run = show_actions_run || show_actions_run_verbose;
    show_actions_run_verbose;
    show_checked;
    show_considering;
    show_reflecting;
    show_reconsidering;
    show_trace_messages;
    show_error_dependency_paths;
    prefix_time;
    report_long_cycle_times =
      Option.map report_long_cycle_times ~f:(fun ms -> Time.Span.create ~ms ());
    progress;
    dont_emit_kill_line = String.(terminal_type = "dumb");
    path_to_jenga_conf;
    brief_error_summary;
    no_server;
    no_notifiers;
    no_fs_triggers;
    sandbox_actions =
      if sandbox_actions then Db.Sandbox_kind.Hardlink
      else Db.Sandbox_kind.No_sandbox;
    goals = anon_goals;
    gc = {
      Config.Gc.
      minor_heap_size;
      major_heap_increment;
      space_overhead
    }
  }
;;

let command ~toplevel ~run () =
  Command.basic
    ~summary:("build specified targets" ^
              if toplevel then "" else " (default subcommand)")
    ~readme:(fun () ->
      let rest =
        if toplevel then ["To see other jenga commands, try jenga help."] else []
      in
      String.concat ~sep:"\n" ("By default building the .DEFAULT target." :: rest)
    )
    (Command.Param.map config_param ~f:(fun config () -> run config))
