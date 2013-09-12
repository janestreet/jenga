open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

type state =
  | Unbreakable
  | Broken
  | Fresh
  | Fragile of (unit -> unit) ref (* This is by far the most common case *)
  | Fragiles of (unit -> unit) Bag.t

(* mutable node set to Void to avoid keeping reference to graph of broken nodes *)
type node = Void | Base of string | Node of h list
and h = {
  u : int;
  mutable node : node;
  mutable state : state;
}

let create_u =
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun ~node -> {
    u = genU();
    node;
    state = Fresh;
  }

let seal h =
  (match h.node with Node _ -> h.node <- Void | _ -> ());
  h.state <- Broken

let break h =
  match h.state with
  | Unbreakable -> assert false
  | Broken -> ()
  | Fresh ->
    seal h
  | Fragile f ->
    seal h; !f ()
  | Fragiles bag ->
    seal h;
    let triggers = Bag.to_list bag in
    Bag.clear bag;
    List.iter ~f:(fun f -> f ()) triggers

let register g ~f =
  let ignore' = ignore in
  let fragile g f =
    let one = ref f in
    g.state <- Fragile one;
    Some (fun () -> one := ignore')
  in
  let fragiles bag f =
    let elt = Bag.add bag f in
    Some (fun () -> if not (Bag.is_empty bag) then Bag.remove bag elt)
  in
  match g.state with
  | Unbreakable -> None
  | Broken -> f (); None
  | Fresh -> fragile g f
  | Fragile one when phys_equal !one ignore' ->
    fragile g f
  | Fragile one ->
    let bag = Bag.of_list [(fun () -> !one ())] in
    g.state <- Fragiles bag;
    fragiles bag f
  | Fragiles bag ->
    fragiles bag f

let link ~child ~parent = register child ~f:(fun () -> break parent)

let is_broken h =
  match h.state with
  | Broken -> true
  | (Unbreakable | Fresh | Fragile _ | Fragiles _) -> false

module Glass = struct
  type t = h
  let create ~desc = create_u ~node:(Base desc)
  let is_broken = is_broken
  let break = break
  let desc h = match h.node with | Base desc -> desc | (Void | Node _) -> assert false
end

type t = h

let of_glass g = g

let broken =
  let g = Glass.create ~desc:"Broken" in
  break g; g
let unbreakable =
  let g = Glass.create ~desc:"Unbreakable" in
  g.state <- Unbreakable; g

let combine2 h1 h2 = match h1, h2 with
  | {state=Broken;_},_ | _, {state=Broken;_} -> broken
  | {state=Unbreakable;_},h | h,{state=Unbreakable;_} -> h
  | h1,h2 ->
    let g = create_u ~node:(Node [h1;h2]) in
    ignore (link ~child:h1 ~parent:g);
    ignore (link ~child:h2 ~parent:g);
    g

let combine hearts =
  if List.exists ~f:is_broken hearts then broken
  else if List.for_all hearts ~f:(phys_equal unbreakable) then unbreakable
  else
    let g = create_u ~node:(Node hearts) in
    List.iter hearts ~f:(fun child -> ignore (link ~child ~parent:g));
    g

let upon h ~f = register h ~f

type canceller = (unit -> unit) option
let trigger = function
  | None -> ()
  | Some f -> f ()

let when_broken h =
  if is_broken h
  then Deferred.unit
  else Deferred.create (fun ivar -> ignore (register h ~f:(Ivar.fill ivar)))

let collect pred h =
  let visited = Int.Hash_set.create() in
  let rec walk acc h =
    if Hash_set.mem visited h.u then acc else (
      Hash_set.add visited h.u;
      match h.node with
      | Base who -> if pred h then who :: acc else acc
      | Node hs -> List.fold_left ~init:acc ~f:walk hs
      | Void -> acc
    )
  in
  walk [] h

let to_sensitivity_list = collect (fun _ -> true)
