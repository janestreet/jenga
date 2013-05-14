
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module MG = Monomorphic_mutable_graph

open Description

module Item = struct

  type t =
  | Root
  | Scanner of Description.Scanner.t
  | Dep of Dep.t
  | Target_rule of Target_rule.t
  | Gen_key of Gen_key.t

end

module T = struct

  module Node = MG.Node

  type t = graph and graph = {
    (* An edge (src,dest) in this graph inidates: src depends on dest.
       - dest is a dependency of src
       - src is a dependant of dest
    *)
    mg : MG.t;
    mutable roots : Node.t list;
    data : (Node.t, Item.t) Hashtbl.t;
  }

  let roots t = t.roots
  let dependants n = MG.step n
  let dependencies n = MG.inverse_step n
  let id_string n = MG.id_string n
  let lookup_item t n = Hashtbl.find_exn t.data n

  let create () = {
    mg = MG.create ();
    roots = [];
    data = Node.Table.create ();
  }

  let add_node graph value =
    let node = MG.create_node graph.mg in
    Hashtbl.add_exn graph.data ~key:node ~data:value;
    node

  let create_root graph =
    let node = add_node graph (Item.Root) in
    graph.roots <- node :: graph.roots;
    node

  let disregard_roots t = t.roots <- []

  let create_dependency graph old_node value =
    let new_node = add_node graph value in
    MG.add_edge ~src:new_node ~dest:old_node;
    new_node

  let link_dependants_no_cycle_check n1 ~additional:n2 =
    MG.add_edge ~src:n1 ~dest:n2

  let remove_all_dependencies n =
    MG.remove_all_incoming_edges_from_node n

end

include T
