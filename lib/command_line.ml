
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

let f_number =
  let default = 1 in
  Spec.step (fun m x -> m ~f_number:x)
  +> Spec.flag "f" (Spec.optional_with_default default Spec.int)
    ~doc:(sprintf "<forkers> #additional procs for forking (default: %d)" default)

let poll_forever =
  Spec.step (fun m x -> m ~poll_forever:x)
  +> Spec.flag "poll-forever" ~aliases:["P"] Spec.no_arg
    ~doc:" poll filesystem for changes (keep polling forever)"

let verbose =
  Spec.step (fun m x -> m ~verbose:x)
  +> Spec.flag "verbose" ~aliases:["--verbose"] Spec.no_arg
    ~doc:" Verbose output"

let show_actions_run =
  Spec.step (fun m x -> m ~show_actions_run:x)
  +> Spec.flag "show-actions-run" ~aliases:["act"] Spec.no_arg
    ~doc:" Show actions being run, and why"

let show_generators_run =
  Spec.step (fun m x -> m ~show_generators_run:x)
  +> Spec.flag "show-generators-run" ~aliases:["gen"] Spec.no_arg
    ~doc:" Show generators being run, and why"

let show_scanners_run =
  Spec.step (fun m x -> m ~show_scanners_run:x)
  +> Spec.flag "show-scanners-run" ~aliases:["scan"] Spec.no_arg
    ~doc:" Show scanners being run, and why"

let show_run_reason =
  Spec.step (fun m x -> m ~show_run_reason:x)
  +> Spec.flag "show-run-reason" ~aliases:["rr"] Spec.no_arg
    ~doc:" Show actions/scanners/generators being run, and why"

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

let quiet =
  Spec.step (fun m x -> m ~quiet:x)
  +> Spec.flag "quiet" Spec.no_arg
    ~doc:" Dont show any commands run & their output, even when they fail"

(* dev *)
let debug =
  Spec.step (fun m x -> m ~debug:x)
  +> Spec.flag "debug" ~aliases:["D"] Spec.no_arg
    ~doc:" show debug"

let time =
  Spec.step (fun m x -> m ~time:x)
  +> Spec.flag "time" Spec.no_arg
    ~doc:" prefix all messages with the time (since the build started)"

let report_mem =
  Spec.step (fun m x -> m ~report_mem:x)
  +> Spec.flag "report-mem" Spec.no_arg
    ~doc:" report heap mem usage when polling; causes one major GC to be forced"

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

let continuous_graph_dump =
  Spec.step (fun m x -> m ~continuous_graph_dump:x)
  +> Spec.flag "continuous-graph-dump" ~aliases:["graph"] Spec.no_arg
    ~doc:" Continuous dump build graph to .jenga.graph for dev/demo"

let external_jenga_root =
  Spec.step (fun m x -> m ~external_jenga_root:x)
  +> Spec.flag "external-jengaroot" ~aliases:["x"] (Spec.optional Spec.string)
    ~doc:(sprintf " Specify path to %s; The repo_root is taken to be CWD."
            Misc.jenga_root_basename)

let anon_demands =
  Spec.step (fun m demands -> m ~demands)
  +> Spec.anon (Spec.sequence ("DEMAND" %: Spec.string))


let go_command =
  Command.basic (
    j_number
    ++ f_number
    ++ poll_forever
    ++ verbose
    ++ show_actions_run
    ++ show_scanners_run
    ++ show_generators_run
    ++ show_run_reason
    ++ show_checked
    ++ show_considering
    ++ show_reconsidering
    ++ quiet
    ++ debug
    ++ time
    ++ report_mem
    ++ sequential_deps
    ++ show_sensitized
    ++ delay_for_dev
    ++ report_long_cycle_times
    ++ wflag
    ++ output_postpone
    ++ progress
    ++ continuous_graph_dump
    ++ external_jenga_root
    ++ anon_demands
  )
    ~summary:"Run Jenga in the current directory."
    ~readme:(fun () -> String.concat ~sep:"\n" [
      "By default building the .DEFAULT target.";
    ])
    (fun ~j_number ~f_number ~poll_forever
      ~verbose
      ~show_actions_run
      ~show_scanners_run
      ~show_generators_run
      ~show_run_reason
      ~show_checked ~show_considering ~show_reconsidering
      ~quiet ~debug ~time ~report_mem
      ~sequential_deps ~show_sensitized
      ~delay_for_dev ~report_long_cycle_times
      ~wflag:_ ~output_postpone:_
      ~progress ~continuous_graph_dump
      ~external_jenga_root
      ~demands
      () ->
        let config = {
          Config.
          j_number;
          f_number;
          poll_forever;
          verbose;
          show_actions_run      = show_run_reason || show_actions_run;
          show_generators_run   = show_run_reason || show_generators_run;
          show_scanners_run     = show_run_reason || show_scanners_run;
          show_checked;
          show_considering;
          show_reconsidering;
          quiet;
          debug;
          time;
          report_mem;
          sequential_deps;
          show_sensitized;
          delay_for_dev = Option.map delay_for_dev ~f:(fun x -> sec (float x));
          report_long_cycle_times =
            Option.map report_long_cycle_times ~f:(fun ms -> Time.Span.create ~ms ());
          progress;
          continuous_graph_dump;
          external_jenga_root;
          demands;
        }
        in
        Run.main config
    )

let main () =
  Command.run go_command

