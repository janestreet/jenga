
open Core.Std
open Async.Std

module Progress_stream = struct
  type query = unit [@@deriving bin_io]
  type response = Progress.Snap.t [@@deriving bin_io]
  type error = Nothing.t [@@deriving bin_io]
  let rpc =
    Rpc.Pipe_rpc.create ~name:"progress-stream" ~version:0
      ~bin_query ~bin_response ~bin_error ()
  ;;
end

module Update_stream = struct
  type query = unit [@@deriving bin_io]
  type response = Progress.Updates.t [@@deriving bin_io]
  type error = Nothing.t [@@deriving bin_io]
  let rpc =
    Rpc.Pipe_rpc.create ~name:"update-stream" ~version:0
      ~bin_query ~bin_response ~bin_error ()
  ;;
end
