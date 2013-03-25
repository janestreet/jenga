
open Core.Std

(* The graph `discovered' from a build description.
   Represented explicitly to allow cycle checking.
*)

module Item : sig

  type t =
  | Root
  | Dep of Description.Dep.t
  | Target_rule of Description.Target_rule.t
  | Gen_key of Description.Gen_key.t

end

type t (* item-graph + roots *)

module Node : sig
  type t
  include Hashable with type t := t
end

val roots : t -> Node.t list
val dependencies : Node.t -> Node.t list
val dependants : Node.t -> Node.t list
val id_string : Node.t -> string
val lookup_item : t -> Node.t -> Item.t

val create : unit -> t
val create_root : t -> Node.t
val disregard_roots : t -> unit
val create_dependency : t -> Node.t -> Item.t -> Node.t
val link_dependants : t -> Node.t -> additional:Node.t -> [ `ok | `cycle of Item.t list ]
val remove_all_dependencies : Node.t -> unit
