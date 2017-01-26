
(** A [Ring] is a bag structure; similar to [Core.Bag].

   The differences being:
   - An [elem] can be [detach]ed without explicit reference to the [Ring].
   - It is safe to call [detach] from a finalizer. *)

type 'a t
type 'a elem

(** [ring = create ()]
   Returns a new empty ring *)
val create : unit -> 'a t

(** [keep_alive ring v] makes [ring] keep [v] alive in the gc sense. Only one value can
    be kept alive per ring. *)
val keep_alive : 'a t -> 'b -> unit

(** [elem = add ring x]

   Adds a value [x] to an existing [ring], returning [elem], which is a handle by which
   the value [x] can be removed from the ring *)
val add : 'a t -> 'a -> 'a elem

(** [detach elem]

   Detaches [elem] from the [ring] to which it was [add]ed. It is allowed for [detach] to
   be called repeatedly on the same [elem]; subsequent calls have no effect. *)
val detach : 'a elem -> unit

(** [iter ring ~f]

   [f] is called in turn on each value [x] which has been [add]ed to [ring] (in the order
   the [x] were added) for those [x] whose associated [elem] has not been [detach]ed.

   It is allowed for elements to be detached during an iteration - since calls to [detach]
   from a finalizer may preempt a running [iter]. It is undefined whether [f] is called on
   the value [x] which is preemptively detached. *)
val iter : 'a t -> f:('a -> unit) -> unit

(** [add_calling t x ~f]

   Behaves like [add t x] except [f] is called part way through the operation.  This is
   not intended to be useful! The purpose is to allow tests which check the behaviour of
   calling [detach] part way through an [add] operation. *)
val add_calling : 'a t -> 'a -> f:(unit -> unit) -> 'a elem

val size : 'a t -> int
