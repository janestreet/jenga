
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Bag : sig (* Document Bag interface required *)
  module Elt : sig
    type 'a t
  end
  type 'a t
  val create : unit -> 'a t
  val add : 'a t -> 'a -> 'a Elt.t
  val remove : 'a t -> 'a Elt.t -> unit
  val to_list :'a t -> 'a list
end = struct
  include Core.Std.Bag
end

type graph = {
  gid : int;
}
type t = graph

let equal_graph g1 g2 = Int.(=) g1.gid g2.gid

module Node = struct

  module T = struct
    type t = {
      graph : graph;
      u : int;
      outgoing : t Bag.t;
      mutable incoming : (t * t Bag.Elt.t) list;
    }
    let hash t = t.u
    let compare t1 t2 = Int.compare t1.u t2.u
    let sexp_of_t t = Int.sexp_of_t t.u
    let t_of_sexp _ = assert false
  end
  include T
  include Hashable.Make(T)

end

open Node.T

let create =
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun () -> {
    gid = genU();
  }

let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u)

let create_node =
  fun graph -> {
    graph;
    u = genU();
    outgoing = Bag.create();
    incoming = [];
  }

let graph n = n.graph

let equal_node n1 n2 =
  assert (equal_graph (graph n1) (graph n2));
  Int.(=) n1.u n2.u

let id_string n = sprintf "n%d" n.u

let add_edge ~src ~dest =
  assert (equal_graph (graph src) (graph dest));
  let h = Bag.add src.outgoing dest in
  dest.incoming <- (src,h) :: dest.incoming

let step n = Bag.to_list n.outgoing

let inverse_step n = List.map ~f:fst n.incoming

let remove_all_incoming_edges_from_node dest =
  List.iter dest.incoming ~f:(fun (src,h) ->
    Bag.remove src.outgoing h;
  );
  dest.incoming <- []

  (* Can find a path from one node to another.  Do this to detect cycles.
  *)

let try_find_path_to_reach_node_from ~start ~goal =
  let visiting_or_visited = Node.Hash_set.create () in
  let rec search path node =
    if equal_node node goal
    then Some (goal::path)
    else
      if Hash_set.mem visiting_or_visited node
      then None
      else (
        Hash_set.add visiting_or_visited node;
        let path = node :: path in
        let rec loop = function
          | [] -> None
          | node::nodes ->
            match (search path node) with
            | None -> loop nodes
            | Some result -> Some result
        in loop (step node)
      )
  in search [] start
