
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_ let _ = _squelch_unused_module_warning_

type t = {
  j_number : int;
  f_number : int;
  stop_on_first_error : bool;
  poll_forever : bool;
  verbose : bool;
  show_actions_run : bool;
  show_actions_run_verbose : bool;
  show_checked : bool;
  show_considering : bool;
  show_reconsidering : bool;
  show_trace_messages : bool;
  debug_discovered_graph : bool;
  prefix_time : bool;
  delay_for_dev : Time.Span.t option;
  report_long_cycle_times : Time.Span.t option;
  progress : bool;
  external_jenga_root : string option;
  full_error_summary : bool;
  no_server : bool;
  demands : string list;
} with fields
