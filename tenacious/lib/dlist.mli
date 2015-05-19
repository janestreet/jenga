(** difference lists:
    a representation for lists supporting constant-time append *)

type 'a t
val empty : 'a t
val singleton : 'a -> 'a t
val (@) : 'a t -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val concat : 'a t list -> 'a t
