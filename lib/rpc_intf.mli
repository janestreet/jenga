
open Core.Std
open Async.Std

val progress_stream : (unit,Progress.Snap.t,unit) Rpc.Pipe_rpc.t
val update_stream : (unit,Progress.Updates.t,unit) Rpc.Pipe_rpc.t
