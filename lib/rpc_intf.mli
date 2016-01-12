open Core.Std
open Async.Std

module Progress_stream : sig
  type query = unit [@@deriving bin_io]
  type response = Progress.Snap.t [@@deriving bin_io]
  type error = Nothing.t [@@deriving bin_io]
  val rpc : (query, response, error) Rpc.Pipe_rpc.t
end

module Update_stream : sig
  type query = unit [@@deriving bin_io]
  type response = Progress.Updates.t [@@deriving bin_io]
  type error = Nothing.t [@@deriving bin_io]
  val rpc : (query, response, error) Rpc.Pipe_rpc.t
end
