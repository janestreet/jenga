open Core
open Async
open Int.Replace_polymorphic_compare

module Id = Unique_id.Int63 ()

module Node = struct
  type t = {
    id : Id.t;
    started_at : Time.t;
    blocked_on : t Bag.t;
    name : String.t Lazy.t;
  } [@@deriving fields]

  let compare t1 t2 =
    Id.compare t1.id t2.id

  let (<>) x y = compare x y <> 0

  let create name =
    { id = Id.create ()
    ; blocked_on = Bag.create ()
    ; started_at = Time.now ()
    ; name
    }

  let to_string { id; started_at = _; blocked_on = _; name } =
    sprintf !"[%{Id}] (%s)" id (force name)
end

let is_reachable : src:Node.t -> dest:Node.t -> bool  =
  let rec go checked ~src ~dest =
    if Id.(=) (Node.id src) (Node.id dest)
    then true
    else if Hash_set.mem checked (Node.id src)
    then false
    else
      begin
        Hash_set.add checked (Node.id src);
        Bag.exists (Node.blocked_on src)
          ~f:(fun x -> go checked ~src:x ~dest);
      end
  in
  fun ~src ~dest ->
    go (Id.Hash_set.create ()) ~src ~dest

let _useful_for_debugging = is_reachable

let look_for_a_cycle (thread : Node.t)
  : ([ `Prefix of string list ] * [ `Cycle of string list ]) option =
  let checked = Id.Hash_set.create () in
  (* Although this is not tail-recursive, the maximum observed depth when building [lib]
     was 188 so it should be fine. *)
  let rec go ancestors_set (thread : Node.t) =
    let id = Node.id thread in
    if Hash_set.mem checked id
    then None
    else if Id.Set.mem ancestors_set id
    then Some ([], thread)
    else
      begin
        let res =
          Bag.find_map (Node.blocked_on thread)
            ~f:(go (Set.add ancestors_set id))
        in
        Hash_set.add checked id;
        Option.map res
          ~f:(fun (cyclic_path, cycle_start) -> (thread :: cyclic_path), cycle_start)
      end
  in
  Option.map (go (Id.Set.empty) thread)
    ~f:(fun (cyclic_path, cycle_start) ->
      let (prefix, cycle) =
        List.split_while ~f:(Node.(<>) cycle_start) cyclic_path
      in
      let f = List.map ~f:Node.to_string in
      (`Prefix (f prefix), `Cycle (f cycle))
    )

let root = Node.create (lazy "root")

module Dump = struct

  module Stable = struct
    open Core.Core_stable

    module V1 = struct
      type t = {
        id : Int63.V1.t;
        name : string;
        age : Time.Span.V2.t;
        children : children;
      }
      and children =
        | See_above
        | Here of t list
      [@@deriving bin_io, sexp]
    end

    let%expect_test _ =
      print_endline [%bin_digest: V1.t];
      [%expect {| 8dad49afe85669cef2905bec97ec3347 |} ]
  end

  include Stable.V1

  let collect node =
    let already_printed = Id.Hash_set.create () in
    let now = Time.now () in
    let rec go node =
      let omit_children =
        Hash_set.mem already_printed (Node.id node)
      in
      Hash_set.add already_printed (Node.id node);
      { id = (Node.id node :> Int63.t);
        name = Lazy.force (Node.name node);
        age = Time.diff now (Node.started_at node);
        children = if omit_children
          then See_above
          else
            Here
              begin
                let nodes =
                  List.sort ~compare:Node.compare
                    (Bag.to_list (Node.blocked_on node))
                in
                List.map nodes ~f:go
              end
      }
    in
    go node

  let collect () = collect root

end

module Global = struct

  let look_for_a_cycle () = look_for_a_cycle root

end

let edge_until (node : Node.t) ~(blocked_on : Node.t) deferred =
  let lock = Bag.add (Node.blocked_on node) blocked_on in
  deferred >>| fun res ->
  Bag.remove (Node.blocked_on node) lock;
  res

let root_until ~node deferred = edge_until root ~blocked_on:node deferred

let look_for_a_cycle = Global.look_for_a_cycle
