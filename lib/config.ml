
open Core.Std
open! Int.Replace_polymorphic_compare

module Gc = struct
  type t = {
    minor_heap_size : int;
    major_heap_increment : int;
    space_overhead : int
  }
end

type t = {
  j_number : int;
  f_number : int;
  d_number : int;
  stop_on_first_error : bool;
  poll_forever : bool;
  verbose : bool;
  show_memory_allocations : bool;
  show_actions_run : bool;
  show_actions_run_verbose : bool;
  show_checked : bool;
  show_buildable_discovery : bool;
  show_reflecting : bool;
  show_considering : bool;
  show_reconsidering : bool;
  show_trace_messages : bool;
  show_error_dependency_paths : bool;
  prefix_time : bool;
  report_long_cycle_times : Time.Span.t option;
  progress : [`omake_style | `jem_style] option;
  dont_emit_kill_line : bool;
  path_to_jenga_conf : string option;
  brief_error_summary : bool;
  no_server : bool;
  demands : string list;
  no_notifiers : bool;
  no_fs_triggers : bool;
  sandbox_actions : bool;
  deprecated_camlp4 : bool;
  buildable_targets_fixpoint_max : int;
  gc : Gc.t
} [@@deriving fields]
