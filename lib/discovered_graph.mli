
open Core.Std

(* The graph `discovered' from a build description.
   Represented explicitly to allow cycle checking.
*)

module Item : sig

  type t =
  | Root
  | Goal of Goal.t
  | Reflect of Path.t
  | Dep_scheme of int
  | Artifacts of Path.t
  | Buildable of Path.t

  val to_string : t -> string

end

type t (* item-graph + roots *)

module Node : sig
  type t
  include Hashable with type t := t
  val equal : t -> t -> bool
end

val roots : t -> Node.t list
val dependencies : Node.t -> Node.t list
val dependants : Node.t -> Node.t list
val id_string : Node.t -> string
val lookup_item : t -> Node.t -> Item.t

val create : Config.t -> t
val create_root : t -> Node.t
val disregard_roots : t -> unit
val create_dependency : t -> Node.t -> Item.t -> Node.t
val link_dependants_no_cycle_check : t -> Node.t -> additional:Node.t -> unit
val remove_all_dependencies : t -> Node.t -> unit

val iter_reachable : t -> f:(Node.t -> unit) -> unit
