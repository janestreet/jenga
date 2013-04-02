
open Core.Std
open Async.Std

module Fraction = struct
  type t = int * int with bin_io
end

let progress_stream = Rpc.Pipe_rpc.create
  ~name:"progress-stream"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Fraction.bin_t
  ~bin_error:Unit.bin_t
