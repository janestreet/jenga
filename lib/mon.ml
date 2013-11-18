
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

type t = {
  progress : Progress.Snapped.t;
  effort : Effort.Snapped.t;
} with bin_io, fields

let snap progress = {
  progress = Progress.snap progress;
  effort = Build.snap_all_effort();
}
