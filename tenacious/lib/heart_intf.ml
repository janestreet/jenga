
open! Core.Std
open Async.Std

(* Type to represent when some previously computed result is no longer valid.
   When this happens the heart becomes broken.
   User code can set up chains of breakage between hearts, using [combine]. *)

module type S = sig

  val version : string

  module Heart : sig
    type t
  end

  module Glass : sig
    type t
    val break : t -> unit
    val create : unit -> t
    val is_broken : t -> bool
  end

  module Weak_glass : sig
    type t
    (**
       [let g,h = Weak_glass.create] is analogous to
       [let g = Glass.create () and h = watch g] except it also lets you see when it's
       [unwatched]. It is a guarantee that after [unwatched g] becomes determined,
       [break g] will no longer have any effect (because the user has lost all references
       to all hearts derived from [h]).
    *)
    val create : unit -> t * Heart.t
    val unwatched : t -> unit Deferred.t
    (** Breaks the heart returned by [create] *)
    val break : t -> unit
  end

  module Watching : sig
    type t
    val stop : t -> unit
    val count : unit -> int (* for debug *)
  end

  type t = Heart.t

  val watch : Glass.t -> t
  val combine : t list -> t
  val combine2 : t -> t -> t
  val unbreakable : t
  val is_broken : t -> bool
  (** This watches the heart until it's broken and fills the deferred when it is.
      Note that it is a memory leak to call this on a heart that never breaks. *)
  val when_broken : t -> unit Deferred.t
  (** tells you about what happens first: heart broken ([None]) or
      the given deferred determined with [x] ([Some x]) *)
  val or_broken : t -> 'a Deferred.t -> 'a option Deferred.t
  (**
     [upon_broken_or_determined] : A version of [or_broken] that gives you the result
     synchronously during the heart breakage propagation.
     Prefer [or_broken] when you can as it's easier to reason about.

     [upon_broken_or_determined heart deferred ~broken ~determined]
     - calls [broken] or [determined] depending on what happens first:
     (a) [heart] breaks -> call [broken]
     (b) [deferred] becomes determined -> call [determined]
     It is never the case that both functions are called *)
  val upon_broken_or_determined :
    t -> 'a Deferred.t -> broken:(unit -> unit) -> determined:('a -> unit) -> unit
  (**
     [upon] : A primitive mostly equivalent to [or_broken].

     Watches the heart and calls [f] when it's broken.
     [f] will not be called after you stop watching.
     Returns [None] in case the heart is already broken or unbreakable.

     Note that [f] is called during heart breakage propagation so it can see some
     inconsistent heart states.

     Consider using [when_broken]/[or_broken] instead if they work for you.
  *)
  val upon : t -> f:(unit -> unit) -> Watching.t option

end
