(** This module allows the jenga server to incrementally send updates about
    build errors in a typed way to clients (see the errors rpc). *)

open Core
open Async

(** A [Reportable.t] is an updatable bag of reportable errors, with clients.
    When a client attaches it receives a snapshot and a pipe of updates. *)
type t
[@@deriving sexp_of]

(** [Reportable.Error.t] is a reportable error *)
module Error : sig

  type t

  (** [Reportable.Error.Id.t] is a reportable error handle; used as a key for removal *)
  module Id : sig
    type t [@@deriving sexp_of]
  end

  (** Create a new [Reportable.Error.t] from a [Goal.t] and [Reason.t] *)
  val create : Goal.t -> Reason.t -> t

  (** [id t] return the unique handle of the error *)
  val id : t -> Id.t

  (** [contents t] is the contents of [t] *)
  val contents : t -> [`Command_failed of Job_summary.t | `Internal of string list]

  (** [goal_string t] is the goal_string of [t] *)
  val goal_string : t -> string

  (** [directory t] is the directory of [t] *)
  val directory : t -> Path.Rel.t

end

(** [create t] returns a new [Reportable.t] to which errors can be [add]ed and [remove]d
    [name] is shown by "jenga -trace" when clients register/unregister *)
val create : name:string -> t

(** [add t e] adds [e] to [t]. If [e] is already in [t] then it raises an exception. *)
val add : t -> Error.t -> unit

(** [remove t e] removes [e] from [t]. *)
val remove : t -> Error.Id.t -> unit

(** [length t] is the number of errors in [t]. *)
val length : t -> int

(** [iter t ~f] calls [f] on each error in [t]. *)
val iter : t -> f:(Error.t -> unit) -> unit

(** [to_list t] is the list of errors in [t]. *)
val to_list : t -> Error.t list

module Snap : sig
  type t
end

module Update : sig
  type t
end

module Stable : sig

  module V1 : sig

    (** [Snap.t] facilitates transport over RPC *)
    module Snap : sig
      type t [@@deriving bin_io]
      val upgrade : t -> Snap.t
    end

    (** [Update.t] facilitates transport over RPC *)
    module Update : sig
      type t [@@deriving bin_io]
      val upgrade : t -> Update.t
    end
  end

  module V2 : sig

    (** [Snap.t] facilitates transport over RPC *)
    module Snap : sig
      type t = Snap.t [@@deriving bin_io]
    end

    (** [Update.t] facilitates transport over RPC *)
    module Update : sig
      type t = Update.t [@@deriving bin_io]
    end
  end

end

(** Drop all clients attached to the [Reportable.t].  Useful for testing the reconnection
    logic of clients. *)
val drop_clients : t -> unit

(** [snap_with_updates ~trace t] attaches a new client, returning the snapshot & update
    pipe. The client is detached when the caller closes the returned pipe-reader.

    The intended use of [snap_with_updates] is the [Error_pipe] RPC; constructing a
    `mirror' [Reportable.t] on the client-side of the RPC using [of_snap] and [update]. *)
val snap_with_updates :
  trace: (string -> unit) ->
  t ->
  Snap.t * Update.t Pipe.Reader.t

(** [of_snap ~name snap] returns a new [Reportable.t] initialized from a snapshot. *)
val of_snap : name:string -> Snap.t -> t

(** [update t update] applies an update to [Reportable.t]. *)
val update : t -> Update.t -> unit
