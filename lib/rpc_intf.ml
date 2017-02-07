(** All the rpcs exposed by the jenga server.

    The rpcs in here are used by the jenga client and build-manager. When called from
    jenga, versioning doesn't matter as the same executable is used on both sides. When
    called from build-manager, we expect build-manager to be ahead, and it should be
    rolled before jengas that introduce new versions. *)

open Core
open Async
open! Int.Replace_polymorphic_compare

module Progress_stream = struct
  module V1 = struct
    let client_pushes_back = false
    let version = 1

    type query = unit [@@deriving bin_io]
    type response = Progress.Snap.Stable.V1.t [@@deriving bin_io]
    type error = Nothing.t [@@deriving bin_io]

    let query_of_model = Fn.id
    let model_of_response = Fn.id
    let model_of_error = Fn.id
  end

  include Versioned_rpc.Caller_converts.Pipe_rpc.Make (struct
      let name = "progress-stream"
      include V1
    end)

  include Register(V1)
end

module Error_pipe = struct (** Snapshot of errors + updates *)
  module V1 = struct
    let client_pushes_back = false
    let version = 1

    type query = unit [@@deriving bin_io]
    type state = Reportable.Stable.V1.Snap.t [@@deriving bin_io]
    type update = Reportable.Stable.V1.Update.t [@@deriving bin_io]
    type error = Nothing.t [@@deriving bin_io]

    let query_of_model = Fn.id
    let model_of_state = Reportable.Stable.V1.Snap.upgrade
    let model_of_error = Fn.id
    let model_of_update = Reportable.Stable.V1.Update.upgrade
  end

  module V2 = struct
    let client_pushes_back = false
    let version = 2

    type query = unit [@@deriving bin_io]
    type state = Reportable.Stable.V2.Snap.t [@@deriving bin_io]
    type update = Reportable.Stable.V2.Update.t [@@deriving bin_io]
    type error = Nothing.t [@@deriving bin_io]

    let query_of_model = Fn.id
    let model_of_state = Fn.id
    let model_of_error = Fn.id
    let model_of_update = Fn.id
  end

  include Versioned_rpc.Caller_converts.State_rpc.Make (struct
      let name = "error-pipe"
      include V2
    end)

  include Register(V1)
  include Register(V2)
end

module Getenv = struct
  module V1 = struct
    let version = 1

    type query = Var.Getenv.Stable.V1.query [@@deriving bin_io]
    type response = Var.Getenv.Stable.V1.response [@@deriving bin_io]

    let query_of_model = Fn.id
    let model_of_response = Fn.id
  end

  include Versioned_rpc.Caller_converts.Rpc.Make (struct
      let name = "getenv"
      include V1
    end)

  include Register(V1)
end

module Setenv = struct
  module V1 = struct
    let version = 1

    type query = Var.Setenv.Stable.V1.query [@@deriving bin_io]
    type response = Var.Setenv.Stable.V1.response [@@deriving bin_io]

    let query_of_model = Fn.id
    let model_of_response = Fn.id
  end

  include Versioned_rpc.Caller_converts.Rpc.Make (struct
      let name = "setenv/unsetenv"
      include V1
    end)

  include Register(V1)
end

module Env_info = struct
  module V1 = struct
    let version = 1

    type query = unit [@@deriving bin_io]
    type response = Var.Info.Stable.V1.t list [@@deriving bin_io]

    let query_of_model = Fn.id
    let model_of_response = Fn.id
  end

  include Versioned_rpc.Caller_converts.Rpc.Make (struct
      let name = "env-info"
      include V1
    end)

  include Register(V1)
end

module Dump_tenacious_graph = struct
  module V1 = struct
    let version = 1

    type query = unit [@@deriving bin_io]
    type response = Tenacious_lib.Graph.Dump.Stable.V1.t [@@deriving bin_io]

    let query_of_model = Fn.id
    let model_of_response = Fn.id
  end

  include Versioned_rpc.Caller_converts.Rpc.Make (struct
      let name = "dump-tenacious"
      include V1
    end)

  include Register(V1)
end
