open Core.Std
open Async.Std

(* Type to represent when some previously computed result is no longer valid.
   When this happens the heart becomes broken.
   User code can set up chains of breakage between hearts, using [combine].
*)

type t
type canceller = (unit -> unit) option

module Glass : sig
  type t
  val create : desc:string -> t
  val desc : t -> string
  val is_broken : t -> bool
  val break : t -> unit
end

val of_glass : Glass.t -> t

val combine2 : t -> t -> t
val combine : t list -> t

val upon : t -> f:(unit -> unit) -> canceller
val trigger : canceller -> unit

val unbreakable : t
val is_broken : t -> bool
val when_broken : t -> unit Deferred.t

val to_sensitivity_list : t -> string list
