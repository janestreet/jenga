
(* [Weak_ref.t] - Individual (not arrays of) weak references. *)

type 'a t
val create : 'a -> 'a t
val get : 'a t -> 'a option
