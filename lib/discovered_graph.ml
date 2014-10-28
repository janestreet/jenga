
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module MG = Monomorphic_mutable_graph

module Item = struct

  type t =
  | Root
  | Goal of Goal.t
  | Reflect of Path.t
  | Dep_scheme of int
  | Artifacts of Path.t
  | Buildable of Path.t

  let to_string = function
    | Root -> "ROOT"
    | Goal goal -> sprintf "GOAL: %s" (Goal.to_string goal)
    | Reflect path -> sprintf "REFLECT: %s" (Path.to_string path)
    | Dep_scheme dep_u -> sprintf "DEP-SCHEME: %d" dep_u
    | Artifacts dir -> sprintf "ARTIFACTS: %s" (Path.to_string dir)
    | Buildable dir -> sprintf "BUILDABLE: %s" (Path.to_string dir)

end

module T = struct

  module Node = MG.Node

  type t = graph and graph = {
    (* Edges in this graph have the following meaning...
       SRC --> DEST
       -- SRC is (one of) the dependencies of DEST, or
       -- DEST is (one of) the DEPENDANTS of DEST
       i.e
       these edges are *reverse* dependency edges
       the arrow means: here are my dependants - "the things which depend on me"
    *)
    debug : bool;
    mg : MG.t;
    mutable roots : Node.t list;
    data : (Node.t, Item.t) Hashtbl.t;
  }

  let roots t = t.roots
  let dependants n = MG.step n
  let dependencies n = MG.inverse_step n
  let id_string n = MG.id_string n
  let lookup_item t n = Hashtbl.find_exn t.data n

  let create config = {
    debug = Config.debug_discovered_graph config;
    mg = MG.create ();
    roots = [];
    data = Node.Table.create ();
  }

  let add_node graph value =
    let node = MG.create_node graph.mg in
    if graph.debug then (
      Message.message "%s = NODE (%s)" (id_string node) (Item.to_string value);
    );
    Hashtbl.add_exn graph.data ~key:node ~data:value;
    node

  let add_edge graph ~src ~dest =
    if graph.debug then (
      Message.message "DEP : %s --> %s" (id_string dest) (id_string src);
    );
    MG.add_edge ~src ~dest

  let create_root graph =
    let node = add_node graph (Item.Root) in
    graph.roots <- node :: graph.roots;
    node

  let disregard_roots t = t.roots <- []

  let create_dependency graph old_node value =
    let new_node = add_node graph value in
    add_edge graph ~src:new_node ~dest:old_node;
    new_node

  let link_dependants_no_cycle_check graph n1 ~additional:n2 =
    add_edge graph ~src:n1 ~dest:n2

  let remove_all_dependencies graph n =
    if graph.debug then (
      Message.message "remove-all-dependencies: %s" (id_string n);
    );
    MG.remove_all_incoming_edges_from_node n

end

include T

let iter_reachable t ~f =
  let visiting_or_visited = Node.Hash_set.create () in
  let rec walk node =
    let stop = Hash_set.mem visiting_or_visited node in
    if stop then () else (
      Hash_set.add visiting_or_visited node;
      f node;
      List.iter (dependencies node) ~f:walk
    )
  in
  List.iter (roots t) ~f:walk
