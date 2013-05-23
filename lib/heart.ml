
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

type state = Broken | Fragile of (unit -> unit) list

type node = Base of string | Node of h * h
and h = {
  u : int ;
  node : node;
  mutable state : state;
}

let create_u =
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun ~node -> {
    u = genU();
    node;
    state = Fragile [];
  }

let break h =
  match h.state with
  | Broken -> ()
  | Fragile triggers ->
    h.state <- Broken;
    List.iter triggers ~f:(fun trigger -> trigger())

let link ~child ~parent =
  match child.state with
  | Broken -> break parent
  | Fragile triggers ->
    child.state <- Fragile ((fun () -> break parent) :: triggers)

let node_is_broken h =
  match h.state with
  | Broken -> true
  | Fragile _ -> false

module Glass = struct
  type t = h
  let create ~desc = create_u ~node:(Base desc)
  let is_broken = node_is_broken
  let break = break
  let desc h = match h.node with | Base desc -> desc | Node _ -> assert false
end

type t = Unbreakable | Breakable of h

let of_glass g = Breakable g

let unbreakable = Unbreakable

let combine2 t1 t2 =
  match t1,t2 with
  | Unbreakable,Unbreakable -> Unbreakable
  | Unbreakable,t -> t
  | t,Unbreakable -> t
  | Breakable h1, Breakable h2 ->
    let h = create_u ~node:(Node (h1,h2)) in
    link ~child:h1 ~parent:h;
    link ~child:h2 ~parent:h;
    Breakable h

let combine ts = List.fold ts ~init:unbreakable ~f:combine2

let is_broken = function
  | Unbreakable -> false
  | Breakable h -> node_is_broken h

let when_broken = function
  | Unbreakable -> Deferred.never()
  | Breakable h ->
    match h.state with
    | Broken -> Deferred.unit
    | Fragile triggers ->
      Deferred.create (
        fun ivar ->
          h.state <-
            Fragile ((fun () -> Ivar.fill ivar ()) :: triggers)
      )

let collect pred t =
  match t with
  | Unbreakable -> []
  | Breakable h ->
    let visited = Int.Hash_set.create() in
    let rec walk acc h =
      if Hash_set.mem visited h.u then acc else (
        Hash_set.add visited h.u;
        match h.node with
        | Base who -> if pred h then who :: acc else acc
        | Node (h1,h2) -> walk (walk acc h1) h2
      )
    in
    walk [] h

let to_sensitivity_list = collect (fun _ -> true)


let shutdown_glass = Glass.create ~desc:"shutdown_glass"
let shutdown () = Glass.break shutdown_glass
let is_shutdown = of_glass shutdown_glass

