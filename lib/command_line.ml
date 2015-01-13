
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

let terminal_type =
  match Core.Std.Sys.getenv "TERM" with
  | None -> ""
  | Some x -> x

module Spec = Command.Spec
let (+>) = Spec.(+>)
let (++) = Spec.(++)
let (%:) = Spec.(%:)

let j_number =
  let formula,default =
    match System.num_cpus_if_known with
    | None -> "", 1
    | Some cpus ->
      " = max (#cpus-1) 1", Int.max (cpus-1) 1
  in
  Spec.step (fun m x -> m ~j_number:x)
  +> Spec.flag "j" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<jobs> parallel jobs, def: %d%s" default formula)

let f_number =
  let formula,default =
    match System.num_cpus_if_known with
    | None -> "", 1
    | Some cpus ->
      " = (#cpus-1)/4 + 1",  (cpus - 1) / 4 + 1
  in
  Spec.step (fun m x -> m ~f_number:x)
  +> Spec.flag "f" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<forkers> forker processes, def: %d%s" default formula)

let d_number =
  let formula,default = (* same defaults as for -j *)
    match System.num_cpus_if_known with
    | None -> "", 1
    | Some cpus ->
      " = max (#cpus-1) 1", Int.max (cpus-1) 1
  in
  Spec.step (fun m x -> m ~d_number:x)
  +> Spec.flag "max-par-digest" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<digests> parallel digests, def: %d%s" default formula)

let poll_forever =
  Spec.step (fun m x -> m ~poll_forever:x)
  +> Spec.flag "poll-forever" ~aliases:["P"] Spec.no_arg
    ~doc:" poll filesystem for changes (keep polling forever)"

let stop_on_first_error =
  Spec.step (fun m x -> m ~stop_on_first_error:x)
  +> Spec.flag "stop-on-first-error" ~aliases:["Q"] Spec.no_arg
    ~doc:" stop when first error encounterd"

let verbose =
  Spec.step (fun m x -> m ~verbose:x)
  +> Spec.flag "verbose" ~aliases:["--verbose"] Spec.no_arg
    ~doc:" Show full command string, and stdout/stderr from every command run"

let show_actions_run =
  Spec.step (fun m x -> m ~show_actions_run:x)
  +> Spec.flag "show-actions-run" ~aliases:["act";"rr"] Spec.no_arg
    ~doc:" Show actions being run; and the reason why"

let show_actions_run_verbose =
  Spec.step (fun m x -> m ~show_actions_run_verbose:x)
  +> Spec.flag "show-actions-run-verbose" ~aliases:["act-verbose";"rr-verbose"] Spec.no_arg
    ~doc:" Be more verbose about the reason actions are run"

let show_checked =
  Spec.step (fun m x -> m ~show_checked:x)
  +> Spec.flag "show-checked" ~aliases:["nr"] Spec.no_arg
    ~doc:" Show actions which are checked, but not run"

let show_buildable_discovery =
  Spec.step (fun m x -> m ~show_buildable_discovery:x)
  +> Spec.flag "show-buildable-discovery" ~aliases:["buildable"] Spec.no_arg
    ~doc:" Mainly for debug. Shows discovery of buildable targets in a directory"

let show_reflecting =
  Spec.step (fun m x -> m ~show_reflecting:x)
  +> Spec.flag "show-reflecting" ~aliases:["reflect"] Spec.no_arg
    ~doc:" Mainly for debug. Shows when deps are being reflected"

let show_considering =
  Spec.step (fun m x -> m ~show_considering:x)
  +> Spec.flag "show-considering" ~aliases:["con"] Spec.no_arg
    ~doc:" Mainly for debug. Shows when deps are considered/re-considered (rather verbose)"

let show_reconsidering =
  Spec.step (fun m x -> m ~show_reconsidering:x)
  +> Spec.flag "show-reconsidering" ~aliases:["recon"] Spec.no_arg
    ~doc:" Mainly for debug. Show when deps are re-considered"

let show_error_dependency_paths =
  Spec.step (fun m x -> m ~show_error_dependency_paths:x)
  +> Spec.flag "show-error-dependency-paths" Spec.no_arg
    ~doc:" show dependency paths from root goal to each error (like exception stack traces)"

let show_trace_messages =
  Spec.step (fun m x -> m ~show_trace_messages:x)
  +> Spec.flag "trace" Spec.no_arg
    ~doc:" switch on some additional trace messages"

let debug_discovered_graph =
  Spec.step (fun m x -> m ~debug_discovered_graph:x)
  +> Spec.flag "debug-discovered-graph" ~aliases:["dg"] Spec.no_arg
    ~doc:" debug when edges are added/removed in the discovered-graph"

let prefix_time =
  Spec.step (fun m x -> m ~prefix_time:x)
  +> Spec.flag "time" Spec.no_arg
    ~doc:" prefix all messages with the time (since the build started)"

let delay_for_dev =
  Spec.step (fun m x -> m ~delay_for_dev:x)
  +> Spec.flag "delay" (Spec.optional Spec.int)
    ~doc:"<seconds> (for development) delay for N seconds before every external job"

let report_long_cycle_times =
  Spec.step (fun m x -> m ~report_long_cycle_times:x)
  +> Spec.flag "report-long-cycle-times" ~aliases:["long"] (Spec.optional Spec.int)
    ~doc:"<ms> (for development) pass to Scheduler.report_long_cycle_times"

let omake_server =
  Spec.step (fun m x -> m ~omake_server:x)
  +> Spec.flag "w" ~aliases:["-omake-server"] Spec.no_arg
    ~doc:" Omake compatability; declare omake-server is caller"

let output_postpone =
  Spec.step (fun m x -> m ~output_postpone:x)
  +> Spec.flag "--output-postpone" Spec.no_arg
    ~doc:" Omake compatability; ignored"

let progress =
  Spec.step (fun m x -> m ~progress:x)
  +> Spec.flag "progress" ~aliases:["--progress"] Spec.no_arg
    ~doc:" Show periodic progress report (omake style)"

let path_to_jenga_conf =
  Spec.step (fun m x -> m ~path_to_jenga_conf:x)
  +> Spec.flag "-path-to-jenga-conf" (Spec.optional Spec.string)
    ~doc:(sprintf " Specify path to <jenga.conf>; The repo_root is taken to be CWD.")

let brief_error_summary =
  Spec.step (fun m x -> m ~brief_error_summary:x)
  +> Spec.flag "brief-error-summary" Spec.no_arg
    ~doc:" Don't repeat stdout/stderr from failing commands in error summary"

let no_server =
  Spec.step (fun m x -> m ~no_server:x)
  +> Spec.flag "no-server" Spec.no_arg
    ~doc:" Don't start jenga server (jem wont work)"

let minor_heap_size =
  let default = 50 in
  Spec.step (fun m x -> m ~minor_heap_size:x)
  +> Spec.flag "minor-heap-size" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<Mb> (default = %d Mb)" default)

let major_heap_increment =
  let default = 200 in
  Spec.step (fun m x -> m ~major_heap_increment:x)
  +> Spec.flag "major-heap-increment" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<Mb> (default = %d Mb)" default)

let space_overhead =
  let default = 100 in
  Spec.step (fun m x -> m ~space_overhead:x)
  +> Spec.flag "space-overhead" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<percent> (default = %d)" default)

let no_notifiers =
  Spec.step (fun m x -> m ~no_notifiers:x)
  +> Spec.flag "no-notifiers" ~aliases:["nono"] Spec.no_arg
    ~doc:" Disable filesystem notifiers (inotify); polling wont work"

let buildable_targets_fixpoint_max =
  let default = 5 in
  Spec.step (fun m x -> m ~buildable_targets_fixpoint_max:x)
  +> Spec.flag "buildable-targets-fixpoint-max" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<iters> (default = %d); 0 means no limit" default)

let cat_api =
  Spec.step (fun m x -> m ~cat_api:x)
  +> Spec.flag "cat-api" ~aliases:["api"] Spec.no_arg
    ~doc:" Print the API supported by this version of jenga, then exit."

let anon_demands =
  Spec.step (fun m x -> m ~anon_demands:x)
  +> Spec.anon (Spec.sequence ("DEMAND" %: Spec.string))

let print_api_and_exit () =
  Printf.printf "%s\n%!" Cat_api.string;
  exit 0

let go_command =
  Command.basic (
    j_number
    ++ f_number
    ++ d_number
    ++ poll_forever
    ++ stop_on_first_error
    ++ verbose
    ++ show_actions_run
    ++ show_actions_run_verbose
    ++ show_checked
    ++ show_buildable_discovery
    ++ show_reflecting
    ++ show_considering
    ++ show_reconsidering
    ++ show_trace_messages
    ++ show_error_dependency_paths
    ++ debug_discovered_graph
    ++ prefix_time
    ++ delay_for_dev
    ++ report_long_cycle_times
    ++ omake_server
    ++ output_postpone
    ++ progress
    ++ path_to_jenga_conf
    ++ brief_error_summary
    ++ no_server
    ++ minor_heap_size
    ++ major_heap_increment
    ++ space_overhead
    ++ no_notifiers
    ++ buildable_targets_fixpoint_max
    ++ cat_api
    ++ anon_demands
  )
    ~summary:"Run Jenga in the current directory."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "By default building the .DEFAULT target.";
    ])
    (fun
      ~j_number
      ~f_number
      ~d_number
      ~poll_forever
      ~stop_on_first_error
      ~verbose
      ~show_actions_run
      ~show_actions_run_verbose
      ~show_checked
      ~show_buildable_discovery
      ~show_reflecting
      ~show_considering
      ~show_reconsidering
      ~show_trace_messages
      ~show_error_dependency_paths
      ~debug_discovered_graph
      ~prefix_time
      ~delay_for_dev
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
      ~buildable_targets_fixpoint_max
      ~cat_api
      ~anon_demands
      () ->
        if cat_api then print_api_and_exit() else
        let config = {
          Config.
          j_number;
          f_number;
          d_number;
          poll_forever;
          stop_on_first_error;
          verbose;
          show_actions_run = show_actions_run || show_actions_run_verbose;
          show_actions_run_verbose;
          show_checked;
          show_considering;
          show_buildable_discovery;
          show_reflecting;
          show_reconsidering;
          show_trace_messages;
          show_error_dependency_paths;
          debug_discovered_graph;
          prefix_time;
          delay_for_dev = Option.map delay_for_dev ~f:(fun x -> sec (float x));
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
          buildable_targets_fixpoint_max;
          demands = anon_demands;
        }
        in
        let words_per_mb = 1024 * 1024 / 8 in
        Gc.tune
          ~minor_heap_size: (words_per_mb * minor_heap_size)
          ~major_heap_increment: (words_per_mb * major_heap_increment)
          ~space_overhead: space_overhead
         ();
        Run.main config
    )

let main () =
  Command.run go_command
