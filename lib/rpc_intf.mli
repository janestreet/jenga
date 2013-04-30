
open Core.Std
open Async.Std

val progress_stream : (unit,Build.Progress.Counts.t,unit) Rpc.Pipe_rpc.t
