
open Core.Std
open! Int.Replace_polymorphic_compare

let terminal_type =
  match Core.Std.Sys.getenv "TERM" with
  | None -> ""
  | Some x -> x

module Param = Command.Param
let (%:) = Param.(%:)

let j_number =
  let formula,default =
    match System.num_cpus_if_known with
    | None -> "", 1
    | Some cpus ->
      " = max (#cpus-1) 1", Int.max (cpus-1) 1
  in
  Param.flag "j" (Param.optional_with_default default Param.int)
    ~doc:(sprintf "<jobs> parallel jobs, def: %d%s" default formula)

let f_number =
  let formula,default =
    match System.num_cpus_if_known with
    | None -> "", 1
    | Some cpus ->
      " = (#cpus-1)/4 + 1",  (cpus - 1) / 4 + 1
  in
  Param.flag "f" (Param.optional_with_default default Param.int)
    ~doc:(sprintf "<forkers> forker processes, def: %d%s" default formula)

let d_number =
  let formula,default = (* same defaults as for -j *)
    match System.num_cpus_if_known with
    | None -> "", 1
    | Some cpus ->
      " = max (#cpus-1) 1", Int.max (cpus-1) 1
  in
  Param.flag "max-par-digest" (Param.optional_with_default default Param.int)
    ~doc:(sprintf "<digests> parallel digests, def: %d%s" default formula)

let poll_forever =
  Param.flag "poll-forever" ~aliases:["P"] Param.no_arg
    ~doc:" poll filesystem for changes (keep polling forever)"

let stop_on_first_error =
  Param.flag "stop-on-first-error" ~aliases:["Q"] Param.no_arg
    ~doc:" stop when first error encounterd"

let verbose =
  Param.flag "verbose" ~aliases:["--verbose"] Param.no_arg
    ~doc:" Show full command string, and stdout/stderr from every command run"

let show_actions_run =
  Param.flag "show-actions-run" ~aliases:["act";"rr"] Param.no_arg
    ~doc:" Show actions being run; and the reason why"

let show_actions_run_verbose =
  Param.flag "show-actions-run-verbose" ~aliases:["act-verbose";"rr-verbose"] Param.no_arg
    ~doc:" Be more verbose about the reason actions are run"

let show_buildable_discovery =
  Param.flag "show-buildable-discovery" ~aliases:["buildable"] Param.no_arg
    ~doc:" Mainly for debug. Shows discovery of buildable targets in a directory"

let show_checked =
  Param.flag "show-checked" ~aliases:["nr"] Param.no_arg
    ~doc:" Show actions which are checked, but not run"

let show_considering =
  Param.flag "show-considering" ~aliases:["con"] Param.no_arg
    ~doc:" Mainly for debug. Shows when deps are considered/re-considered (rather verbose)"

let show_error_dependency_paths =
  Param.flag "show-error-dependency-paths" Param.no_arg
    ~doc:" show dependency paths from root goal to each error (like exception stack traces)"

let show_memory_allocations =
  Param.flag "show-memory-allocations" Param.no_arg
    ~doc:" Show information about memory allocation at the end of the build"

let show_reconsidering =
  Param.flag "show-reconsidering" ~aliases:["recon"] Param.no_arg
    ~doc:" Mainly for debug. Show when deps are re-considered"

let show_reflecting =
  Param.flag "show-reflecting" ~aliases:["reflect"] Param.no_arg
    ~doc:" Mainly for debug. Shows when deps are being reflected"

let show_trace_messages =
  Param.flag "trace" Param.no_arg
    ~doc:" switch on some additional trace messages"

let prefix_time =
  Param.flag "time" Param.no_arg
    ~doc:" prefix all messages with the time"

let report_long_cycle_times =
  Param.flag "report-long-cycle-times" ~aliases:["long"] (Param.optional Param.int)
    ~doc:"<ms> (for development) pass to Scheduler.report_long_cycle_times"

let omake_server =
  Param.flag "w" ~aliases:["-omake-server"] Param.no_arg
    ~doc:" Omake compatability; declare omake-server is caller"

let output_postpone =
  Param.flag "--output-postpone" Param.no_arg
    ~doc:" Omake compatability; ignored"

let progress =
  Param.flag "progress" ~aliases:["--progress"] Param.no_arg
    ~doc:" Show periodic progress report (omake style)"

let path_to_jenga_conf =
  Param.flag "-path-to-jenga-conf" (Param.optional Param.string)
    ~doc:(sprintf " Specify path to <jenga.conf>; The repo_root is taken to be CWD.")

let brief_error_summary =
  Param.flag "brief-error-summary" Param.no_arg
    ~doc:" Don't repeat stdout/stderr from failing commands in error summary"

let no_server =
  Param.flag "no-server" Param.no_arg
    ~doc:" Don't start jenga server (queries to the server won't work)"

let minor_heap_size =
  let default = 50 in
  Param.flag "minor-heap-size" (Param.optional_with_default default Param.int)
    ~doc:(sprintf "<Mb> (default = %d Mb)" default)

let major_heap_increment =
  let default = 200 in
  Param.flag "major-heap-increment" (Param.optional_with_default default Param.int)
    ~doc:(sprintf "<Mb> (default = %d Mb)" default)

let space_overhead =
  let default = 100 in
  Param.flag "space-overhead" (Param.optional_with_default default Param.int)
    ~doc:(sprintf "<percent> (default = %d)" default)

let no_notifiers =
  Param.flag "no-notifiers" ~aliases:["nono"] Param.no_arg
    ~doc:" Disable filesystem notifiers (inotify); polling wont work"

let no_fs_triggers =
  Param.flag "no-fs-triggers" Param.no_arg
    ~doc:(" For testing, only valid without notifiers. Makes jenga more strict by \
            failing instead of potentially recovering when the file system changes.")

let buildable_targets_fixpoint_max =
  let default = 5 in
  Param.flag "buildable-targets-fixpoint-max" (Param.optional_with_default default Param.int)
    ~doc:(sprintf "<iters> (default = %d); 0 means no limit" default)

let sandbox_actions =
  Param.flag "-sandbox-actions" Param.no_arg
    ~doc:" Check dependencies are right by running actions in a part of the filesystem \
          where only the declared dependencies are available"

let anon_demands =
  Param.anon (Param.sequence ("DEMAND" %: Param.string))

let create_config
      ~j_number
      ~f_number
      ~d_number
      ~poll_forever
      ~stop_on_first_error
      ~verbose
      ~show_actions_run
      ~show_actions_run_verbose
      ~show_buildable_discovery
      ~show_checked
      ~show_considering
      ~show_error_dependency_paths
      ~show_memory_allocations
      ~show_reconsidering
      ~show_reflecting
      ~show_trace_messages
      ~prefix_time
      ~report_long_cycle_times
      ~omake_server
      ~output_postpone:_
      ~progress
      ~path_to_jenga_conf
      ~brief_error_summary
      ~no_server
      ~minor_heap_size
      ~major_heap_increment
      ~space_overhead
      ~no_notifiers
      ~no_fs_triggers
      ~buildable_targets_fixpoint_max
      ~sandbox_actions
      ~anon_demands
  =
  {
    Config.
    j_number;
    f_number;
    d_number;
    poll_forever;
    stop_on_first_error;
    verbose;
    show_memory_allocations;
    show_actions_run = show_actions_run || show_actions_run_verbose;
    show_actions_run_verbose;
    show_checked;
    show_considering;
    show_buildable_discovery;
    show_reflecting;
    show_reconsidering;
    show_trace_messages;
    show_error_dependency_paths;
    prefix_time;
    report_long_cycle_times =
      Option.map report_long_cycle_times ~f:(fun ms -> Time.Span.create ~ms ());

    progress =
      if progress then
        if omake_server
        then Some `omake_style
        else Some `jem_style
      else None;

    dont_emit_kill_line = String.(terminal_type = "dumb");

    path_to_jenga_conf;
    brief_error_summary;
    no_server;
    no_notifiers;
    no_fs_triggers;
    buildable_targets_fixpoint_max;
    sandbox_actions =
      if sandbox_actions then Db.Sandbox_kind.Hardlink
      else Db.Sandbox_kind.No_sandbox;
    demands = anon_demands;
    gc = {
      Config.Gc.
      minor_heap_size;
      major_heap_increment;
      space_overhead
    }
  }

let config_param : Config.t Command.Param.t =
  let open Command.Let_syntax in
  let%map_open
      j_number                        = j_number
  and f_number                        = f_number
  and d_number                        = d_number
  and poll_forever                    = poll_forever
  and stop_on_first_error             = stop_on_first_error
  and verbose                         = verbose
  and show_actions_run                = show_actions_run
  and show_actions_run_verbose        = show_actions_run_verbose
  and show_buildable_discovery        = show_buildable_discovery
  and show_checked                    = show_checked
  and show_considering                = show_considering
  and show_error_dependency_paths     = show_error_dependency_paths
  and show_memory_allocations         = show_memory_allocations
  and show_reconsidering              = show_reconsidering
  and show_reflecting                 = show_reflecting
  and show_trace_messages             = show_trace_messages
  and prefix_time                     = prefix_time
  and report_long_cycle_times         = report_long_cycle_times
  and omake_server                    = omake_server
  and output_postpone:_               = output_postpone
  and progress                        = progress
  and path_to_jenga_conf              = path_to_jenga_conf
  and brief_error_summary             = brief_error_summary
  and no_server                       = no_server
  and minor_heap_size                 = minor_heap_size
  and major_heap_increment            = major_heap_increment
  and space_overhead                  = space_overhead
  and no_notifiers                    = no_notifiers
  and no_fs_triggers                  = no_fs_triggers
  and buildable_targets_fixpoint_max  = buildable_targets_fixpoint_max
  and sandbox_actions                 = sandbox_actions
  and anon_demands                    = anon_demands
  in
  create_config
    ~j_number
    ~f_number
    ~d_number
    ~poll_forever
    ~stop_on_first_error
    ~verbose
    ~show_actions_run
    ~show_actions_run_verbose
    ~show_buildable_discovery
    ~show_checked
    ~show_considering
    ~show_error_dependency_paths
    ~show_memory_allocations
    ~show_reconsidering
    ~show_reflecting
    ~show_trace_messages
    ~prefix_time
    ~report_long_cycle_times
    ~omake_server
    ~output_postpone
    ~progress
    ~path_to_jenga_conf
    ~brief_error_summary
    ~no_server
    ~minor_heap_size
    ~major_heap_increment
    ~space_overhead
    ~no_notifiers
    ~no_fs_triggers
    ~buildable_targets_fixpoint_max
    ~sandbox_actions
    ~anon_demands

let command ~toplevel ~run () =
  Command.basic'
    ~summary:("build specified targets" ^
              if toplevel then "" else " (default subcommand)")
    ~readme:(fun () ->
      let rest =
        if toplevel then ["To see other jenga commands, try jenga help."] else []
      in
      String.concat ~sep:"\n" ("By default building the .DEFAULT target." :: rest)
    )
    (Command.Param.map config_param ~f:(fun config () -> run config))
