
open Core.Std
open Async.Std

val persist_saves_done : Effort.Counter.t
val actions_run : Effort.Counter.t
val considerations_run : Effort.Counter.t

module Need : sig
  type t
  val goal : Goal.t -> t
  val jengaroot : t
  include Hashable_binable with type t := t
  include Comparable_binable with type t := t
  val to_string : t -> string
end

module Status : sig
  type t =
  | Todo
  | Built
  | Error of Reason.t list (* empty list means failure in deps *)
end

type t
val create : Config.t -> t

val enqueue_job : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

val set_status : t -> Need.t -> Status.t -> unit
val message_errors : Config.t -> t -> unit

val mask_unreachable : t -> is_reachable_error:(Need.t -> bool) -> unit
val unmask_everything : t -> unit

module Snap : sig

  type t with bin_io

  val no_errors : t -> bool (*bad=0*)
  val built : t -> int
  val fraction : t -> (int*int) (* built/total *)

  val to_string : t -> [`omake_style | `jem_style ] -> string
  val to_effort_string : t -> string
  val finished: t -> bool
  val error_code: t -> int
end

val snap : t -> Snap.t
val reset_effort : unit -> unit

val readme : unit -> string

