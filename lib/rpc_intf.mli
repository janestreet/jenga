
open Core.Std
open Async.Std

val progress_stream : (unit,Mon.Progress.t,unit) Rpc.Pipe_rpc.t
