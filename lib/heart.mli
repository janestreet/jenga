
open Core.Std
open Async.Std

(* Type to represent when some previously computed result is no longer valid.
   When this happens the heart becomes broken.
   User code can set up chains of breakage between hearts, using [combine].
*)

type t

module Desc : sig
  type t with sexp_of
  val create : string -> t
  val to_string : t -> string
end

module Glass : sig
  type t
  val create : Desc.t -> t
  val desc : t -> Desc.t
  val is_broken : t -> bool
  val break : t -> unit
end

val of_glass : Glass.t -> t
val unbreakable : t
val combine2 : t -> t -> t
val combine : t list -> t

val is_broken : t -> bool
val when_broken : t -> unit Deferred.t

val to_sensitivity_list : t -> Desc.t list
val to_broken_list : t -> Desc.t list
