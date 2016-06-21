
open! Core.Std
open! Async.Std

val lstat_counter : Metrics.Counter.t
val digest_counter : Metrics.Counter.t
val ls_counter : Metrics.Counter.t
val mkdir_counter : Metrics.Counter.t

val saves_done : Metrics.Counter.t

val actions_run : Metrics.Counter.t
val saves_run : Metrics.Counter.t
val considerations_run : Metrics.Counter.t

module Status : sig
  type t =
    | Todo
    | Built
    | Error of Reason.t list (* empty list means failure in deps *)
end

type t
val create : Config.t -> t

val enqueue_job : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

val set_status : t -> Goal.t -> Status.t option -> unit

val mask_unreachable : t -> is_reachable_error:(Goal.t -> bool) -> unit

module Snap : sig

  type t [@@deriving bin_io]

  val no_errors : t -> bool (*bad=0*)
  val built : t -> int
  val fraction : t -> (int*int) (* built/total *)

  val to_string : t -> [< `omake_style | `jem_style | `fraction ] -> string
  val to_effort_string : t -> string
  val finished: t -> [ `Success | `Failure ] option
  val to_metrics : t -> Metrics.t
end

val snap : t -> Snap.t
val reset_metrics : unit -> unit

val readme : unit -> string
