
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Spec = Command.Spec
let (+>) = Spec.(+>)
let (++) = Spec.(++)
let (%:) = Spec.(%:)

(* user *)
let j_number =
  let default = 1 in
  Spec.step (fun m x -> m ~j_number:x)
  +> Spec.flag "j" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<jobs> maximum number of parallel jobs (default: %d)" default)

let poll_forever =
  Spec.step (fun m x -> m ~poll_forever:x)
  +> Spec.flag "poll-forever" ~aliases:["P"] Spec.no_arg
    ~doc:" poll filesystem for changes (keep polling forever)"

let verbose =
  Spec.step (fun m x -> m ~verbose:x)
  +> Spec.flag "verbose" ~aliases:["--verbose"] Spec.no_arg
    ~doc:" Verbose output"

let show_run_reason =
  Spec.step (fun m x -> m ~show_run_reason:x)
  +> Spec.flag "show-run-reason" ~aliases:["rr"] Spec.no_arg
    ~doc:" Show actions being run, and why"

let show_checked =
  Spec.step (fun m x -> m ~show_checked:x)
  +> Spec.flag "show-checked" ~aliases:["nr"] Spec.no_arg
    ~doc:" Show actions which are checked, but not run"

let show_considering =
  Spec.step (fun m x -> m ~show_considering:x)
  +> Spec.flag "show-considering" ~aliases:["con"] Spec.no_arg
    ~doc:" Mainly for debug. Shows every dep as it is being considered"

let quiet =
  Spec.step (fun m x -> m ~quiet:x)
  +> Spec.flag "quiet" Spec.no_arg
    ~doc:" Dont show any commands run & their output, even when they fail"

(* dev *)
let debug =
  Spec.step (fun m x -> m ~debug:x)
  +> Spec.flag "debug" ~aliases:["D"] Spec.no_arg
    ~doc:" show debug"

let sequential_deps =
  Spec.step (fun m x -> m ~sequential_deps:x)
  +> Spec.flag "sequential-deps" Spec.no_arg
    ~doc:" (for development) build multiple dependencies sequentially"

let show_sensitized =
  Spec.step (fun m x -> m ~show_sensitized:x)
  +> Spec.flag "show-sensitized" ~aliases:["S"]  Spec.no_arg
    ~doc:" (for development) show sensitized file when polling"

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

let show_working_on =
  Spec.step (fun m x -> m ~show_working_on:x)
  +> Spec.flag "show-working-on" ~aliases:["working"] Spec.no_arg
    ~doc:" Show periodic report on the targets being worked upon"

let continuous_graph_dump =
  Spec.step (fun m x -> m ~continuous_graph_dump:x)
  +> Spec.flag "continuous-graph-dump" ~aliases:["graph"] Spec.no_arg
    ~doc:" Continuous dump build graph to .jenga.graph for dev/demo"

let external_jenga_root =
  Spec.step (fun m x -> m ~external_jenga_root:x)
  +> Spec.flag "external-jenga-root" ~aliases:["x"] (Spec.optional Spec.string)
    ~doc:" Specify path to JengaRoot.ml; The repo_root is taken to be CWD."

let anon_demands =
  Spec.step (fun m demands -> m ~demands)
  +> Spec.anon (Spec.sequence ("DEMAND" %: Spec.string))


let go_command =
  Command.basic (
    j_number
    ++ poll_forever
    ++ verbose
    ++ show_run_reason
    ++ show_checked
    ++ show_considering
    ++ quiet
    ++ debug
    ++ sequential_deps
    ++ show_sensitized
    ++ delay_for_dev
    ++ report_long_cycle_times
    ++ wflag
    ++ output_postpone
    ++ progress
    ++ show_working_on
    ++ continuous_graph_dump
    ++ external_jenga_root
    ++ anon_demands
  )
    ~summary:"Run Jenga in the current directory."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "By default building the .DEFAULT target.";
    ])
    (fun ~j_number ~poll_forever
      ~verbose ~show_run_reason ~show_checked ~show_considering
      ~quiet ~debug ~sequential_deps ~show_sensitized
      ~delay_for_dev ~report_long_cycle_times
      ~wflag:_ ~output_postpone:_
      ~progress ~show_working_on ~continuous_graph_dump
      ~external_jenga_root
      ~demands
      () ->
        let config = {
          Config.
          j_number;
          poll_forever;
          verbose;
          show_run_reason;
          show_checked;
          show_considering;
          quiet;
          debug;
          sequential_deps;
          show_sensitized;
          delay_for_dev = Option.map delay_for_dev ~f:(fun x -> sec (float x));
          report_long_cycle_times =
            Option.map report_long_cycle_times ~f:(fun ms -> Time.Span.create ~ms ());
          progress;
          show_working_on;
          continuous_graph_dump;
          external_jenga_root;
          demands;
        }
        in
        Run.main config
    )

let main () =
  Command.run go_command

