
open Core.Std
open Async.Std

module Glass : sig
  type t
  val create : unit -> t
  val is_broken : t -> bool
  val break : t -> unit
end

module Heart : sig
  type t
  val of_glass : Glass.t -> t
  val unbreakable : t
  val when_broken : t -> unit Deferred.t
end

type 'a t

val return      : 'a -> 'a t
val map         : 'a t -> f:('a -> 'b) -> 'b t
val bind        : 'a t -> ('a -> 'b t) -> 'b t
val all         : 'a t list -> 'a list t
val all_unit    : unit t list -> unit t
val reify       : 'a t -> 'a t
val lift        : (unit -> ('a * Heart.t) Deferred.t) -> 'a t
val exec        : 'a t -> ('a * Heart.t) Deferred.t
val cutoff      : equal:('a -> 'a -> bool) -> 'a t -> 'a t

val both        : 'a t -> 'b t -> ('a * 'b) t

(* removed [with_semantics] from interface; replaced with... *)

val before_redo : 'a t -> f:(unit -> unit) -> 'a t
val uncancellable : 'a t -> 'a t

val with_acquire_release :
  'a t
  -> acquire:(unit -> unit Deferred.t)
  -> release:(unit -> unit)
  -> 'a t

val desensitize : 'a t -> ('a * Heart.t) t
