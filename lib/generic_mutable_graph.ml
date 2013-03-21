
open Core.Std

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

module T : sig

  type 'a t
  type 'a n

  val create : unit -> 'a t
  val add_node : 'a t -> 'a -> 'a n
  val equal_node : 'a n -> 'a n -> bool
  val id_string : 'a n -> string
  val value : 'a n -> 'a
  val graph : 'a n -> 'a t (* every node is part of a specific graph *)
  val add_edge : src:'a n -> dest:'a n -> unit (* graph implicit *)
  val step : 'a n -> 'a n list (* going forwards on edges *)
  val inverse_step : 'a n -> 'a n list (* backwards *)
  val remove_all_incoming_edges_from_node : 'a n -> unit

  val hashable_node : 'a n Hashtbl.Hashable.t

end = struct

  type 'a t = {
    gid : int;
  }

  let equal_graph g1 g2 = Int.(=) g1.gid g2.gid

  type 'a n = {
    graph : 'a t;
    value : 'a;
    u : int;
    outgoing : 'a n Bag.t;
    mutable incoming : ('a n * 'a n Bag.Elt.t) list;
  }

  let create =
    let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
    fun () -> {
      gid = genU();
    }

  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u)
  let add_node =
    fun graph value -> {
      graph;
      value;
      u = genU();
      outgoing = Bag.create();
      incoming = [];
    }

  let value n = n.value

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

  let hash_node n = n.u
  let compare_node n1 n2 = Int.compare n1.u n2.u
  let sexp_of_node n = Int.sexp_of_t n.u

  let hashable_node =
    { Hashtbl.Hashable.
      hash          = hash_node;
      compare       = compare_node;
      sexp_of_t     = sexp_of_node;
    }

end

module Graph_1 = struct

  include T

  (* Can find a path from one node to another.  Do this to detect cycles.
  *)

  let try_find_path_to_reach_node_from ~start ~goal =
    let visiting_or_visited = Hash_set.create ~hashable:T.hashable_node() in
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

end

include Graph_1
