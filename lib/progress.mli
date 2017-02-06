(** This module keeps tracks of different things for progress reporting:
    - the amount of work performed, in the form on the counters below
    - for every goal, whether it needs to be built/is built/is in error (along with the
      error)
    This information is what is displayed periodically in the output of
    [jenga --progress], and the second kind of information is available in a typed
    way to rpc clients through the [Reportable.t].
*)

open! Core
open! Async

val lstat_counter : Metrics.Counter.t
val digest_counter : Metrics.Counter.t
val ls_counter : Metrics.Counter.t
val mkdir_counter : Metrics.Counter.t

val saves_done : Metrics.Counter.t

val actions_run : Metrics.Counter.t
val saves_run : Metrics.Counter.t
val considerations_run : Metrics.Counter.t

(** THE bag of reportable errors; reported to clients over the error-pipe. *)
val the_reportable_errors : Reportable.t

type t
val create : Config.t -> t

val enqueue_job : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

val set_status_todo  : t -> Goal.t -> unit
val set_status_built : t -> Goal.t -> unit
val set_status_error : t -> Goal.t -> Reason.t list -> unit
val clear_status     : t -> Goal.t -> unit

val mask_unreachable : t -> is_reachable_error:(Goal.t -> bool) -> unit

module Snap : sig
  type t

  val no_errors : t -> bool (*bad=0*)
  val built : t -> int
  val fraction : t -> (int*int) (* built/total *)
  (** [todo] and [to_act_string] are provided for display by build-manager *)
  val todo : t -> int
  val to_act_string : t -> string
  val to_string : t -> [< `monitor_style | `fraction ] -> string
  val to_effort_string : t -> string
  val finished: t -> [ `Success | `Failure ] option
  val to_metrics : t -> Metrics.t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io]
    end
  end
end

val snap : t -> Snap.t
val reset_metrics : unit -> unit

val readme : unit -> string
