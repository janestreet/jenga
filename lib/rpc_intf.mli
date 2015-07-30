open Core.Std
open Async.Std

module Progress_stream : sig
  type query = unit with bin_io
  type response = Progress.Snap.t with bin_io
  type error = Nothing.t with bin_io
  val rpc : (query, response, error) Rpc.Pipe_rpc.t
end

module Update_stream : sig
  type query = unit with bin_io
  type response = Progress.Updates.t with bin_io
  type error = Nothing.t with bin_io
  val rpc : (query, response, error) Rpc.Pipe_rpc.t
end
