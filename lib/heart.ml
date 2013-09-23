open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Ring : sig
  type 'a t
  val root : 'a -> 'a t
  val extend : 'a t -> 'a -> 'a t
  val detach : 'a t -> unit
  val to_list : 'a t -> 'a list
  val value : 'a t -> 'a
end = struct
  type 'a t = {mutable prev: 'a t; value: 'a; mutable next: 'a t}

  let root value =
    let rec t = {prev = t; next = t; value} in
    t

  let extend t value =
    let t' = {prev = t.prev; value; next = t} in
    t.prev.next <- t';
    t.prev <- t';
    t'

  let detach t =
    t.next.prev <- t.prev;
    t.prev.next <- t.next;
    t.next <- t;
    t.prev <- t

  let to_list =
    let rec aux t t' acc =
      if phys_equal t t'
      then t.value :: acc
      else aux t t'.prev (t'.value :: acc)
    in
    fun t -> aux t t.prev []

  let value t = t.value
end
 
type t = {
  u : int;
  desc : string;
  mutable state : state;
}
and state =
  | Unbreakable
  | Fresh
  | Fragile of (t Ring.t * t Ring.t Weak.t * (unit -> unit) Ring.t)
  | Broken

type canceller = (unit -> unit) Ring.t

let create_u =
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun ~desc -> 
    {
      u = genU();
      desc;
      state = Fresh;
    }

let no_deps = Weak.create ~len:0

let fragilize t =
  match t.state with 
  | Broken | Unbreakable | Fragile _ -> t
  | Fresh ->
    t.state <- Fragile (Ring.root t, no_deps, Ring.root ignore);
    t

let rec break t =
  match t.state with
  | Broken -> ()
  | Unbreakable -> assert false
  | Fresh ->
    t.state <- Broken;
  | Fragile (deps, rdeps, callbacks) ->
    let callbacks = Ring.to_list callbacks in
    let deps = Ring.to_list deps in
    t.state <- Broken;
    for i = 0 to Weak.length rdeps - 1 do
      match Weak.get rdeps i with
      | None -> ()
      | Some h -> Ring.detach (Heap_block.value h)
    done;
    List.iter deps ~f:break;
    List.iter callbacks ~f:(fun f -> f ())

let register t ~f =
  match (fragilize t).state with
  | Broken      -> f (); None
  | Unbreakable -> None
  | Fresh       -> assert false
  | Fragile (_deps, _rdeps, callbacks) ->
    Some (Ring.extend callbacks f)

let is_broken t =
  match t.state with
  | Broken -> true
  | Unbreakable | Fresh | Fragile _ -> false

let broken =
  let g = create_u ~desc:"Broken" in
  break g; g

let unbreakable =
  let g = create_u ~desc:"Unbreakable" in
  g.state <- Unbreakable; g

let create_with_deps ~is_glass ~desc deps =
  let breakable = function {state = Unbreakable; _} -> false | _ -> true in
  match List.rev_filter ~f:breakable deps with
  | [h] when not is_glass -> h
  | []  when not is_glass -> unbreakable
  | [] -> create_u ~desc
  | deps when List.exists ~f:is_broken deps -> broken
  | deps ->
    let t = create_u ~desc in
    let len = List.length deps in
    let weak = Weak.create ~len in
    t.state <- Fragile (Ring.root t, weak, Ring.root ignore);
    List.iteri deps ~f:(fun i dep ->
      match (fragilize dep).state with
      | Fragile (root,_rdeps,_callbacks) ->
        let dep = Ring.extend root t in
        Weak.set weak i (Heap_block.create dep)
      | Unbreakable | Broken | Fresh -> assert false
    );
    t

module Glass = struct
  type heart = t
  type nonrec t = t
  let create = create_u
  let create_with_deps deps ~desc = create_with_deps ~is_glass:true ~desc deps

  let is_broken = is_broken
  let break = break
  let desc h = h.desc
end

let combine2 h1 h2 = create_with_deps ~is_glass:false ~desc:"" [h1;h2]
let combine hs = create_with_deps ~is_glass:false ~desc:"" hs

let of_glass g = g

let upon h ~f = register h ~f

let cancel = function
  | None -> ()
  | Some r -> Ring.detach r

let when_broken h =
  if is_broken h
  then Deferred.unit
  else Deferred.create (fun ivar -> ignore (register h ~f:(Ivar.fill ivar)))

let collect pred h =
  let visited = Int.Hash_set.create() in
  let rec walk acc h =
    if Hash_set.mem visited h.u
    then acc
    else
      let acc = if pred h then h.desc :: acc else acc in
      Hash_set.add visited h.u;
      match h.state with
      | Fragile (_deps,rdeps,_callbacks) ->
        let acc = ref acc in
        for i = 0 to Weak.length rdeps - 1 do
          match Weak.get rdeps i with
          | None -> ()
          | Some h -> acc := walk !acc (Ring.value (Heap_block.value h))
        done;
        !acc
      | Unbreakable | Broken | Fresh -> acc
  in
  walk [] h

let to_sensitivity_list = collect (fun _ -> true)
