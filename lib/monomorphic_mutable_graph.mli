
open Core.Std

type t

module Node : sig
  type t
  include Hashable with type t := t
  val equal : t -> t -> bool
end

val create : unit -> t
val create_node : t -> Node.t
val equal_node : Node.t -> Node.t -> bool
val id_string : Node.t -> string
val graph : Node.t -> t
val add_edge : src:Node.t -> dest:Node.t -> unit
val step : Node.t -> Node.t list
val inverse_step : Node.t -> Node.t list
val remove_all_incoming_edges_from_node : Node.t -> unit
