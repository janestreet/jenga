
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

module Getenv = struct
  type query = Var.Getenv.query [@@deriving bin_io]
  type response = Var.Getenv.response [@@deriving bin_io]
  let rpc = Rpc.Rpc.create ~name:"getenv" ~version:0 ~bin_query ~bin_response
end

module Setenv = struct
  type query = Var.Setenv.query [@@deriving bin_io]
  type response = Var.Setenv.response [@@deriving bin_io]
  let rpc = Rpc.Rpc.create ~name:"setenv/unsetenv" ~version:0 ~bin_query ~bin_response
end

module Env_info = struct
  type query = unit [@@deriving bin_io]
  type response = Var.Info.t list [@@deriving bin_io]
  let rpc = Rpc.Rpc.create ~name:"env-info" ~version:0 ~bin_query ~bin_response
end

module Dump_tenacious_graph = struct
  type query = unit [@@deriving bin_io]
  type response = Tenacious_lib.Graph.Dump.t [@@deriving bin_io]
  let rpc =
    Rpc.Rpc.create ~name:"dump-tenacious-graph" ~version:0
      ~bin_query ~bin_response
end
