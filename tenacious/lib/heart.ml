
open Core.Std
open Async.Std

let version = "finalized-hearts+watching+weak-glass"

(**
   The type representing a heart that is not yet broken.

   If the heart [client] wants to watch a heart [dependency], we add
   [client] to [dependency.clients] so we can find all the clients of a given heart when
   we want to break it.

   Note that [clients] holding weak references makes it not sufficient to keep
   notification chains in memory. To ensure [dependency] never gets garbage collected
   before [client] does, we add [dependency] to [client.strong_refs].

   We also keep the list of [registrations] to unsubscribe from [dependency] when [client]
   doesn't need it anymore. (gets broken and/or GCed)

   [triggers] is there to call the user callbacks when hearts are broken.
   Here we need to prevent premature GC of hearts again.
   We do that by using the global [Watching.ring] and let user control the lifetimes.
*)
type fragile = {
  strong_refs : breakable Array.t; (* to prevent GC *)
  registrations : breakable Weak_ref.t Ring.elem Array.t;
  clients : breakable Weak_ref.t Ring.t;
  triggers : trigger Ring.t;
}

and state = Fragile of fragile | Broken

and breakable = { mutable state : state }

and trigger = {
  strong_elem : breakable Ring.elem;
  func : (unit -> unit);
}

module Watching = struct

  type t = {
    trigger : trigger;
    elem : trigger Ring.elem;
  }

  let ring : breakable Ring.t = Ring.create ()

  let stop t =
    Ring.detach t.trigger.strong_elem;
    Ring.detach t.elem

  let count () = Ring.size ring

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
    { state =
        Fragile
          { strong_refs = Array.empty ()
          ; registrations = Array.empty ()
          ; clients = Ring.create ()
          ; triggers = Ring.create ()
          }
    }

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
      let fragile = {
        strong_refs = ts;
        registrations;
        clients = Ring.create ();
        triggers = Ring.create ();
      } in
      Core.Std.Gc.Expert.add_finalizer_exn fragile (fun fragile ->
        Array.iter fragile.registrations ~f:Ring.detach;
      );
      t.state <- Fragile fragile;
      t

  let rec break t =
    match t.state with
    | Broken -> ()
    | Fragile fragile ->
      t.state <- Broken;
      Ring.iter fragile.triggers ~f:(fun trigger ->
        Ring.detach trigger.strong_elem;
        trigger.func()
      );
      Ring.iter fragile.clients ~f:(fun w ->
        match Weak_ref.get w with
        | None -> ()
        | Some client -> break client
      )

  let upon t ~f:func =
    match t.state with
    | Broken -> func(); None (* [f] called immediately before returning *)
    | Fragile fragile ->
      let strong_elem = Ring.add Watching.ring t in
      let trigger = { strong_elem; func } in
      let elem = Ring.add fragile.triggers trigger in
      let watching = { Watching. trigger; elem } in
      Some watching

end

module Glass = struct

  include Breakable
  let create () = combine0 ()

end

module Heart = struct

  type t = Breakable.t option (* [None] means unbreakable *)

  let unbreakable = None
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
    | Some b ->
      let ivar = Ivar.create () in
      let fill () = Ivar.fill ivar () in
      let (_:Watching.t option) = Breakable.upon b ~f:fill in
      Ivar.read ivar

  let or_broken t d1 = match t with
    | None -> (d1 >>| Option.some)
    | Some b2 ->
      let ivar = Ivar.create () in
      let fill () = Ivar.fill ivar () in
      match Breakable.upon b2 ~f:fill with
      | None -> Deferred.return None
      | Some w ->
        choose [
          choice d1 (fun x -> Watching.stop w; Some x);
          choice (Ivar.read ivar) (fun () -> None);
        ]

  let upon_broken_or_determined t d1 ~broken ~determined = match t with
    | None -> Deferred.upon d1 determined
    | Some b2 ->
      let broken_called = Ivar.create () in
      let call_broken () = Ivar.fill broken_called (); broken () in
      match Breakable.upon b2 ~f:call_broken with
      | None -> ()
      | Some w ->
        don't_wait_for (choose [
          choice d1 (fun x ->
            if Ivar.is_empty broken_called then
              (Watching.stop w; determined x));
          choice (Ivar.read broken_called) (fun () -> ());
        ])

  let upon t ~f =
    match t with
    | None -> None (* [f] not called, and never will be *)
    | Some b -> Breakable.upon b ~f
end

module Weak_glass = struct

  type t = Glass.t Weak_ref.t * unit Deferred.t

  let create () =
    let glass = Glass.create () in
    let heart = Heart.watch glass in
    let unwatched = Ivar.create () in
    Async.Std.Gc.add_finalizer_exn glass (fun _ -> Ivar.fill unwatched ());
    (Weak_ref.create glass, Ivar.read unwatched), heart

  let break (t, _) =
    match Weak_ref.get t with
    | None -> ()
    | Some glass -> Glass.break glass

  let unwatched = snd

end

include Heart
