
open Core.Std
open Async.Std

val progress_stream : (unit,Progress.Snap.t,unit) Rpc.Pipe_rpc.t
