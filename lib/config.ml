
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

type t = {
  j_number : int;
  f_number : int;
  d_number : int;
  stop_on_first_error : bool;
  poll_forever : bool;
  verbose : bool;
  show_actions_run : bool;
  show_actions_run_verbose : bool;
  show_checked : bool;
  show_buildable_discovery : bool;
  show_reflecting : bool;
  show_considering : bool;
  show_reconsidering : bool;
  show_trace_messages : bool;
  show_error_dependency_paths : bool;
  debug_discovered_graph : bool;
  prefix_time : bool;
  delay_for_dev : Time.Span.t option;
  report_long_cycle_times : Time.Span.t option;
  progress : [`omake_style | `jem_style] option;
  dont_emit_kill_line : bool;
  path_to_jenga_conf : string option;
  brief_error_summary : bool;
  no_server : bool;
  demands : string list;
  no_notifiers : bool;
  buildable_targets_fixpoint_max : int;
} with fields
