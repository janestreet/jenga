
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Spec = Command.Spec
let (+>) = Spec.(+>)
let (++) = Spec.(++)
let (%:) = Spec.(%:)

let cpus =
  let err fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt in
  let path = "/proc/cpuinfo" in
  let lines =
    try
      let module IC = Core.Std.In_channel in
      IC.with_file path ~f:IC.input_lines
    with
    | _ -> err "cant read: %s" path; []
  in
  match List.filter lines ~f:(String.is_prefix ~prefix:"processor")  with
  | [] -> err "no processor lines seen in: %s" path; 1
  | lines -> List.length lines

let j_formula = "max (#cpus-1) 1"
let j_default = Int.max (cpus-1) 1

let f_formula = "(#cpus-1)/4 + 1"
let f_default = (cpus - 1) / 4 + 1

(* user *)
let j_number =
  Spec.step (fun m x -> m ~j_number:x)
  +> Spec.flag "j" (Spec.optional_with_default j_default Spec.int)
    ~doc:(sprintf "<jobs> maximum num jobs, def: %d = %s)"
            j_default j_formula)

let f_number =
  Spec.step (fun m x -> m ~f_number:x)
  +> Spec.flag "f" (Spec.optional_with_default f_default Spec.int)
    ~doc:(sprintf "<forkers> num forker procs, def: %d = %s"
            f_default f_formula)

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

let show_considering =
  Spec.step (fun m x -> m ~show_considering:x)
  +> Spec.flag "show-considering" ~aliases:["con"] Spec.no_arg
    ~doc:" Mainly for debug. Shows when deps are considered/re-considered (rather verbose)"

let show_reconsidering =
  Spec.step (fun m x -> m ~show_reconsidering:x)
  +> Spec.flag "show-reconsidering" ~aliases:["recon"] Spec.no_arg
    ~doc:" Mainly for debug. Show when deps are re-considered"


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

(* compatability with omake - oamke-mode passes this *)
let wflag =
  Spec.step (fun m x -> m ~wflag:x)
  +> Spec.flag "w" Spec.no_arg
    ~doc:" Ignored (omake compatability)"

let output_postpone =
  Spec.step (fun m x -> m ~output_postpone:x)
  +> Spec.flag "--output-postpone" Spec.no_arg
    ~doc:" Ignored (omake compatability)"

let progress =
  Spec.step (fun m x -> m ~progress:x)
  +> Spec.flag "progress" ~aliases:["--progress"] Spec.no_arg
    ~doc:" Show periodic progress report"

let external_jenga_root =
  Spec.step (fun m x -> m ~external_jenga_root:x)
  +> Spec.flag "external-jengaroot" ~aliases:["x"] (Spec.optional Spec.string)
    ~doc:(sprintf " Specify path to %s; The repo_root is taken to be CWD."
            Misc.jenga_root_basename)

let anon_demands =
  Spec.step (fun m demands -> m ~demands)
  +> Spec.anon (Spec.sequence ("DEMAND" %: Spec.string))

let full_error_summary =
  Spec.step (fun m x -> m ~full_error_summary:x)
  +> Spec.flag "full-error-summary" Spec.no_arg
    ~doc:" Repeat stdout/stderr from failing commands in error summary"

let no_server =
  Spec.step (fun m x -> m ~no_server:x)
  +> Spec.flag "no-server" Spec.no_arg
    ~doc:" Don't start jenga server (jem wont work)"


let go_command =
  Command.basic (
    j_number
    ++ f_number
    ++ poll_forever
    ++ stop_on_first_error
    ++ verbose
    ++ show_actions_run
    ++ show_actions_run_verbose
    ++ show_checked
    ++ show_considering
    ++ show_reconsidering
    ++ show_trace_messages
    ++ debug_discovered_graph
    ++ prefix_time
    ++ delay_for_dev
    ++ report_long_cycle_times
    ++ wflag
    ++ output_postpone
    ++ progress
    ++ external_jenga_root
    ++ full_error_summary
    ++ no_server
    ++ anon_demands

  )
    ~summary:"Run Jenga in the current directory."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "By default building the .DEFAULT target.";
    ])
    (fun
      ~j_number
      ~f_number
      ~poll_forever
      ~stop_on_first_error
      ~verbose
      ~show_actions_run
      ~show_actions_run_verbose
      ~show_checked
      ~show_considering
      ~show_reconsidering
      ~show_trace_messages
      ~debug_discovered_graph
      ~prefix_time
      ~delay_for_dev
      ~report_long_cycle_times
      ~wflag:_
      ~output_postpone:_
      ~progress
      ~external_jenga_root
      ~full_error_summary
      ~no_server
      ~demands
      () ->
        let config = {
          Config.
          j_number;
          f_number;
          poll_forever;
          stop_on_first_error;
          verbose;
          show_actions_run = show_actions_run || show_actions_run_verbose;
          show_actions_run_verbose;
          show_checked;
          show_considering;
          show_reconsidering;
          show_trace_messages;
          debug_discovered_graph;
          prefix_time;
          delay_for_dev = Option.map delay_for_dev ~f:(fun x -> sec (float x));
          report_long_cycle_times =
            Option.map report_long_cycle_times ~f:(fun ms -> Time.Span.create ~ms ());
          progress;
          external_jenga_root;
          full_error_summary;
          no_server;
          demands;
        }
        in
        Run.main config
    )

let main () =
  Command.run go_command

