
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_ let _ = _squelch_unused_module_warning_

type t = {
  j_number : int;
  f_number : int;
  poll_forever : bool;
  verbose : bool;
  run_reason_verbose : bool;
  show_actions_run : bool;
  show_generators_run : bool;
  show_scanners_run : bool;
  show_checked : bool;
  show_considering : bool;
  show_reconsidering : bool;
  quiet : bool;
  debug : bool;
  time : bool;
  report_mem : bool;
  sequential_deps : bool;
  show_sensitized : bool;
  delay_for_dev : Time.Span.t option;
  report_long_cycle_times : Time.Span.t option;
  progress : bool;
  external_jenga_root : string option;
  demands : string list;
} with fields
