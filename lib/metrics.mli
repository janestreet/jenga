(** This module is used to track performance.
    [Counter] are used to keep track of events such as "ran an action", and [Memory] keeps
    track of allocation rates, and global properties like the amount of live memory.
    jenga writes the collected information to .jenga/metrics everytime jenga is done
    building (and then quits, or waits for filesystem changes).
    [../benchmarking/bench.exe] can be used to build and gather these metrics files,
    and compare various versions of jenga or the jenga rules against one another. *)

open! Core

module Unit : sig
  type t =
    | Byte
    | Second
    | Dimensionless
  [@@deriving compare, sexp_of]
end

type t = (float * Unit.t) String.Map.t [@@deriving sexp_of]
type metrics = t

val disjoint_union_exn : t -> t -> t

module Counter : sig
  type t
  val create : string -> t
  val incr : t -> unit
  val get : t -> int
end

module Counters : sig
  type t
  val create : Counter.t list -> t
  val reset_to_zero : t -> unit

  module Snap : sig
    type t
    val to_string : ?sep:string -> t -> string
    val to_metrics : t -> metrics
    module Stable : sig
      module V1 : sig
        type nonrec t = t [@@deriving bin_io]
      end
    end
  end

  val snap : t -> Snap.t
end

module Memory : sig
  type t =
    { top_heap : Byte_units.t
    ; minor : Byte_units.t
    ; major : Byte_units.t
    ; promoted : Byte_units.t
    ; major_collections : int
    ; heap : Byte_units.t
    } [@@deriving sexp]
  val create_diff_from_previous_create : unit -> t
  val to_metrics : t -> metrics
end

module System_resources : sig
  type t
  val create_diff_from_previous_create : unit -> t
  val to_metrics : t -> metrics
end

module Disk_format : sig
  open Async
  type nonrec t =
    { build_info : Sexp.t
    ; version_util : string list
    ; metrics : t
    }
  [@@deriving sexp]

  val append : metrics -> unit Deferred.t
end
