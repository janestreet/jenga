
open Core.Std
open Async.Std

let progress_stream = Rpc.Pipe_rpc.create
  ~name:"progress-stream"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Progress.Snap.bin_t
  ~bin_error:Unit.bin_t
