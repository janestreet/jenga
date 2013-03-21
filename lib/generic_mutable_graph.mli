
open Core.Std

type 'a t
type 'a n

val create : unit -> 'a t
val add_node : 'a t -> 'a -> 'a n
val equal_node : 'a n -> 'a n -> bool
val id_string : 'a n -> string
val value : 'a n -> 'a
val graph : 'a n -> 'a t
val add_edge : src:'a n -> dest:'a n -> unit
val step : 'a n -> 'a n list
val inverse_step : 'a n -> 'a n list
val remove_all_incoming_edges_from_node : 'a n -> unit

val try_find_path_to_reach_node_from :
  start:'a n -> goal:'a n -> 'a n list option

val hashable_node : 'a n Hashtbl.Hashable.t
