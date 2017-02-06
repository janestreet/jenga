(**
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

  [Tenacious.memoize inner] constructs an [outer] tenacious which behaves like [inner]
  except a single computation instance obtained from sampling [inner] is shared between
  every sampling of [outer]. It is guaranteed only a single computation instance is ever
  running at the same time.
*)

open! Core
open Async

module type S = sig

  val version : string

  (** To be called to set the amount of concurrency allowed before any call to [exec].*)
  val init : concurrency:int -> unit

  module Heart : Heart_intf.S

  include Monad.S
  type 'a tenacious = 'a t
  val all_unit    : unit t list -> unit t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** note that [exec] adds a new root in the observable tenacious graph,
      even if you call it from a function given to [embed] or [lift]. *)
  val exec        : 'a t -> name:(String.t Lazy.t) -> ('a * Heart.t) Deferred.t
  val embed       : (cancel:Heart.t -> ('a * Heart.t) option Deferred.t) -> 'a t
  val memoize       : name:(String.t Lazy.t) -> 'a t -> 'a t

  (** This is most useful in combination with memoize:
      [memoize (bracket ~running ~finished ~cancelled x)].
      Without [memoize], you can get multiple concurrent [running..canceled] and
      [running..finished] blocks even when your tenacious doesn't breaks its heart.
  *)
  val bracket
    :  'a t
    -> running:(int -> unit) (** Number of times this tenacious has run, starting at
                                 0 for the first execution. *)
    -> finished:('a -> unit)
    -> cancelled:(unit -> unit)
    -> 'a t
  val uncancellable : 'a t -> 'a t
  val desensitize : 'a t -> ('a * Heart.t) t

  (** [lift] is specialization/simplification of [embed] *)
  val lift : (unit -> ('a * Heart.t) Deferred.t) -> 'a t

  (** cutoff is dangerous:
      it will delay heart breakage for the time it takes to re-compute the value so
      evaluating a [memoize (cutoff x)] might give you stale values even when [memoize x]
      wouldn't.

      We have had a solution in the form of [val protecting_cutoffs : 'a t -> 'a t]
      that would only return a value after waiting for all cutoffs to finish.
      We removed the function right after [f322aafe57c1] because it was unused
      and was preventing an optimization for [Heart.or_broken].
  *)
  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t

  (**
     [race] non-deterministically chooses the first computation to succeed,
     cancels the other.

      Note that as it's non-deterministic it should be used with care. Ideally the final
      result should not depend on which result gets produced first.
  *)
  val race : 'a t -> 'a t -> 'a t

  (** [Stream] provides two things:
      - sharing of the computations needed by various tenacious computations (same as
        memoize)
      - sharing of the [Heart]s representing the validity of the prefixes of the stream
      That second part is important, as it cannot be done by simply calling [memoize]. *)
  module Stream : sig
    type 'a t
    val unfold : 's -> ('s -> ('a * 's tenacious)) -> name:string Lazy.t -> 'a t
    type ('a, 'res) query =
      | Return of 'res
      | Continue of ('a -> ('a, 'res) query)
    val query : 'a t -> ('a, 'res) query -> 'res tenacious
  end

  module Result : sig
    type nonrec ('a, 'e) t = ('a, 'e) Result.t t
    include Monad.S2 with type ('a, 'e) t := ('a, 'e) t
    val fail : 'e -> (_, 'e) t
    val map_error : ('a, 'e1) t -> f:('e1 -> 'e2) -> ('a, 'e2) t
  end

  (** non-deterministically choose the faster one to fail.
      if neither fails, returns both.
      The [race] caveats apply.
  *)
  val race_error :
    ('a, 'e) Result.t ->
    ('b, 'e) Result.t ->
    f:('a -> 'b -> 'c) ->
    ('c, 'e) Result.t

  val race_errors :
    ('a, 'e) Result.t list ->
    ('a list, 'e) Result.t

  (** A mutable variable whose state can be watched as a Tenacious computation.
      ['a Var.t] is conceptually a ['a Ref.t] with
      [Var.{create,get,set,replace}] corresponding to
      [Ref.{create,(!),(:=),replace}].
      The important difference is the [watch] function, that lets you construct tenacious
      computations that depend on the value of the variable.
  *)
  module Var : sig
    type 'a t
    type 'a ten
    val create : 'a -> 'a t

    (** a tenacious computation whose value tracks the variable value *)
    val watch : 'a t -> 'a ten

    (** Set new value for the variable.
        Any hearts derived from [watch] are broken immediately. *)
    val set : 'a t -> 'a -> unit

    (** Get the current value of the variable without watching it.
        Should be avoided in Tenacious computations.
        Use [watch] instead. *)
    val get : 'a t -> 'a

    val replace : 'a t -> f:('a -> 'a) -> unit
  end
  with type 'a ten := 'a t
end

module type For_tests = sig
  type 'a t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val all : 'a t list -> 'a list t
  val race_error :
    ('a, 'e) Result.t t ->
    ('b, 'e) Result.t t ->
    f:('a -> 'b -> 'c) ->
    ('c, 'e) Result.t t
  val all_via_race_errors : 'a t list -> 'a list t
end
