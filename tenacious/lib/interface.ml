
(*
  [Tenacious.t] -- A type for "Tenacious computations".

  A value [tenacious] of type ['a Tenacious.t] is a recipe for cancellable computation
  instances, returning certified values of type ['a].

  When a [tenacious] is sampled, a fresh "computation instance" begins running. The result
  of (an uncancelled) computation instance is a value [v] paired with certificate of
  validity [h]. The certificate is invalidated if [h] breaks.

  A computation instance has an associated [cancel] signal, supplied when the instance was
  created. If an instance is cancelled, is will start no further leaf computations.

  [Tenacious.exec t] samples a tenacious [t], and awaits the certified result [(v,h)] of
  the computation instance. The certificate [h] may already be invalidated when the result
  is determined.

  [Tenacious.embed f] constructs a [leaf] tenacious from a thunked deferred computation.
  A new deferred computation is created (by applying [f] to a [cancel] signal) each time
  [leaf] is sampled. The deferred computation may not return [None] unless [leaf] is
  cancelled.

  [Tenacious.bind t1 f] construct a [sequenced] tenacious, such that [f] is applied to a
  value [v1] returned by a computation instance obtained from sampling [t1]. The tenacious
  [t2 = f v1] is then sampled, with a cancel-signal incorporating the certificate [h1]
  associated with [v1], such that [t2] is cancelled if ever [v1] becomes invalidated.  If
  [h1] is invalid at the time [t2] has finished, [sequenced] is re-sampled.

  [Tenacious.all children] constructs a [parent] tenacious which samples its [children]
  concurrently, and waits for them all to finish. Once a child has finished, its
  certificate is monitored for validity; if the certificate becomes invalid, and some
  siblings are still running, the invalidated child is re-sampled immediately.

  [Tenacious.reify inner] constructs an [outer] tenacious which behaves like [inner]
  except a single computation instance obtained from sampling [inner] is shared between
  every sampling of [outer]. It is guaranteed only a single computation instance is ever
  running at the same time.
*)

open! Core.Std
open Async.Std

module type S = sig

  val version : string

  module Heart : Heart_interface.S

  type 'a t

  val exec        : 'a t -> ('a * Heart.t) Deferred.t
  val embed       : (cancel:Heart.t -> ('a * Heart.t) option Deferred.t) -> 'a t
  val return      : 'a -> 'a t
  val bind        : 'a t -> ('a -> 'b t) -> 'b t
  val map         : 'a t -> f:('a -> 'b) -> 'b t
  val all         : 'a t list -> 'a list t
  val all_unit    : unit t list -> unit t
  val reify       : 'a t -> 'a t

  val before_redo : 'a t -> f:(unit -> unit) -> 'a t
  val uncancellable : 'a t -> 'a t
  val desensitize : 'a t -> ('a * Heart.t) t

  (* [lift] is specialization/simplification of [embed] *)
  val lift : (unit -> ('a * Heart.t) Deferred.t) -> 'a t

  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t

end
