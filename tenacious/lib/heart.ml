open Core
open Async
open! Int.Replace_polymorphic_compare

let version = "finalized-hearts+watching+weak-glass"

(**
   The type representing a heart that is not yet broken.

   If the heart [client] wants to watch a heart [dependency], we add
   [client] to [dependency.clients] so we can find all the clients of a given heart when
   we want to break it.

   Note that [clients] holding weak references makes it not sufficient to keep
   notification chains in memory. To ensure [dependency] never gets garbage collected
   before [client] does:
   - there is a [Ring.elem] in [client.registration] that is an element of
     [dependency.clients]
   - a [Ring.elem] keeps its [Ring.t] alive, so [client] keeps [dependency.clients] alive
   - we make [dependency.clients] keeps [dependency] alive (using [Ring.keep_alive])

   We also keep the list of [registrations] to unsubscribe from [dependency] when [client]
   doesn't need it anymore. (gets broken and/or GCed)

   [trigger] is there to call the user callbacks when hearts are broken.
   Here we need to prevent premature GC of hearts again.
   We do that by using the global [Watching.ring] and let user control the lifetimes.
*)
type fragile = {
  registrations : breakable Weak_ref.t Ring.elem Array.t;
  clients : breakable Weak_ref.t Ring.t;
  trigger : unit Ivar.t;
}

and state = Fragile of fragile | Broken

and breakable = { mutable state : state }

module Watching = struct

  type t = breakable Ring.elem

  let ring : breakable Ring.t = Ring.create ()

  let start b : t = Ring.add ring b

  let stop (t : t) = Ring.detach t

end

module Breakable = struct

  type t = breakable

  let create ~state = { state }

  let broken = create ~state:Broken

  let is_broken t =
    match t.state with
    | Broken -> true
    | Fragile _ -> false

  let get_unbroken_exn t =
    match t.state with
    | Broken -> assert false
    | Fragile x -> x

  let combine0 () =
    let registrations = [||] in
    let clients = Ring.create () in
    let trigger = Ivar.create () in
    let fragile = { registrations; clients; trigger } in
    let t = { state = Fragile fragile } in
    Ring.keep_alive clients t;
    t

  let combine ts =
    if Array.exists ts ~f:is_broken
    then broken
    else
      let t = create ~state:Broken in
      let w = Weak_ref.create t in
      let registrations =
        Array.map ts ~f:(fun child ->
          Ring.add (get_unbroken_exn child).clients w)
      in
      let clients = Ring.create () in
      let trigger = Ivar.create () in
      let fragile = { registrations; clients; trigger; } in
      Core.Gc.Expert.add_finalizer_exn fragile (fun fragile ->
        Array.iter fragile.registrations ~f:Ring.detach;
      );
      t.state <- Fragile fragile;
      Ring.keep_alive clients t;
      t

  let rec break t =
    match t.state with
    | Broken -> ()
    | Fragile fragile ->
      t.state <- Broken;
      Ivar.fill fragile.trigger ();
      Ring.iter fragile.clients ~f:(fun w ->
        match Weak_ref.get w with
        | None -> ()
        | Some client -> break client
      )

  let or_broken t d1 =
    match t.state with
    | Broken -> Deferred.return None
    | Fragile fragile ->
      let watch = Watching.start t in
      Deferred.choose [
        Deferred.choice d1
          (fun x -> Watching.stop watch; Some x);
        Deferred.choice (Ivar.read fragile.trigger)
          (fun () -> Watching.stop watch; None);
      ]

  let when_broken t = match t.state with
    | Broken -> Deferred.return ()
    | Fragile fragile ->
      let watch = Watching.start t in
      Ivar.read fragile.trigger >>| fun () ->
      Watching.stop watch

end

module Glass = struct

  include Breakable
  let create () = combine0 ()

end

module Heart = struct

  type t = Breakable.t option (* [None] means unbreakable *)

  let unbreakable = None

  let is_unbreakable = function
    | None -> true
    | Some _ -> false

  let watch b = Some b

  let combine2 t1 t2 = match t1,t2 with
    | None, t -> t
    | t, None -> t
    | Some b1, Some b2 -> Some (Breakable.combine [|b1;b2|])

  let combine ts =
    match (List.filter_opt ts) with
    | [] -> unbreakable
    | [b] -> Some b
    | _::_::_ as bs -> Some (Breakable.combine (Array.of_list bs))

  let is_broken = function
    | None -> false
    | Some b -> Breakable.is_broken b

  let when_broken = function
    | None -> Deferred.never()
    | Some b -> Breakable.when_broken b

  let or_broken t d1 = match t with
    | None -> (d1 >>| Option.some)
    | Some b2 -> Breakable.or_broken b2 d1

end

module Weak_glass = struct

  type t = Glass.t Weak_ref.t * unit Deferred.t

  let create () =
    let glass = Glass.create () in
    let heart = Heart.watch glass in
    let unwatched = Ivar.create () in
    Async.Gc.add_finalizer_exn glass (fun _ -> Ivar.fill unwatched ());
    (Weak_ref.create glass, Ivar.read unwatched), heart

  let break (t, _) =
    match Weak_ref.get t with
    | None -> ()
    | Some glass -> Glass.break glass

  let unwatched = snd

end

include Heart
