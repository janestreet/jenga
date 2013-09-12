
open Core.Std
open Async.Std


module Shared_glass = struct
  type t = {mutable users: int; glass: Heart.Glass.t}
  let share glass = {users = 0; glass}
  let is_broken t = Heart.Glass.is_broken t.glass
  let use t =
    t.users <- t.users + 1

  let release t =
    t.users <- t.users - 1;
    if t.users = 0 then Heart.Glass.break t.glass
end

type 'a v = ('a * Heart.t) option

type 'a state = { mutable shared_glass : Shared_glass.t;
                  mutable value : 'a v Deferred.t }

type _ t =
  | Pure    : 'a -> 'a t
  | Lift    : (cancel:Heart.t -> 'a v Deferred.t) -> 'a t
  | Map     : 'a t * ('a -> 'b) -> 'b t
  | All     : 'a t list -> 'a list t
  | Reified : 'a t * 'a state -> 'a t
  | Bind    : 'a t * ('a -> 'b t) -> 'b t
  | Stable  : 'a t -> 'a t
  (*| Apply   : ('a -> 'b) t * 'a t -> 'b t
    | Defer   : 'a Deferred.t t -> 'a t
    | Lazy    : 'a t Lazy.t -> 'a t*)

let pure x    = Pure x
let lift f    = Lift f
let bind m f  = Bind (m,f)
let map m ~f  = match m with
  | Pure x -> Pure (f x)
  | Map (m,f') -> Map (m, fun x -> f (f' x))
  | _ -> Map (m,f)
let all l = All l
let stable = function
  | Stable _ as t -> t
  | t -> Stable t

(*
  let apply f a = Apply (f,a)
  let defer d   = Defer d
  let of_lazy t = Lazy t
*)
let rec sample : 'a. 'a t -> cancel:Heart.t -> 'a v Deferred.t =
  fun (type a) (t : a t) ~cancel -> match t with
  | _ when Heart.is_broken cancel -> Deferred.return None

  | Pure a -> Deferred.return (Some (a, Heart.unbreakable))
  | Lift f -> f ~cancel
  | Map (t,f) -> sample t ~cancel >>|
    begin function
    | None -> None
    | Some (a,h) -> Some (f a, h)
    end

  | Bind (m, f) ->
    begin sample ~cancel m >>= function
      | None -> Deferred.return None
      | Some (a,h1) ->
        sample ~cancel:(Heart.combine2 h1 cancel) (f a) >>| function
        | None -> None
        | Some (a,h2) -> Some (a,Heart.combine2 h1 h2)
    end

  | Reified (t,st) ->
    let subscribe {shared_glass = sg; _} =
      Shared_glass.use sg;
      Option.iter (Heart.upon cancel ~f:(fun () -> Shared_glass.release sg))
               ~f:(fun canceller -> st.value >>> (fun _ -> canceller ()))
    in
    begin match Deferred.peek st.value with
    | Some (Some (_d,h)) when not (Heart.is_broken h) ->
      st.value
    | None when not (Shared_glass.is_broken st.shared_glass) ->
      subscribe st;
      st.value
    | _ ->
      let g = Heart.Glass.create ~desc:"ten/reified" in
      st.shared_glass <- Shared_glass.share g;
      st.value <- sample t ~cancel:(Heart.of_glass g);
      subscribe st;
      st.value
    end

  | All ts ->
    Deferred.create
    begin fun ivar ->
      let final_glass = Heart.Glass.create ~desc:"final" in
      let canceller = Heart.upon cancel
        ~f:(fun () -> Heart.Glass.break final_glass) in
      let shared_glass = Shared_glass.share final_glass in
      let never = Deferred.never () in
      let sample_one t =
        let cell = ref never and canceller = ref None in
        let rec resample () =
          if Heart.Glass.is_broken final_glass then () else
          begin
            Shared_glass.use shared_glass;
            cell :=
            begin sample t ~cancel >>| function
              | None ->
                assert (Heart.is_broken cancel);
                None (* Normally not possible unless cancel broken!!*)
              | Some (_,h) as res ->
                Heart.trigger !canceller;
                canceller := Heart.upon h ~f:resample;
                Shared_glass.release shared_glass;
                res
            end
          end
        in
        resample ();
        cell, canceller
      in
      let results = List.map ts ~f:sample_one in
      ignore (Heart.upon (Heart.of_glass final_glass) ~f:
      begin fun () ->
        Heart.trigger canceller;
        let fetch (cell,canceller') =
          Heart.trigger !canceller';
          !cell
        in
        if Heart.is_broken cancel
        then Ivar.fill ivar None
        else
        Deferred.all (List.map ~f:fetch results) >>> fun results ->
        let rs, hs = List.unzip (List.filter_opt (results)) in
        Ivar.fill ivar (Some (rs, Heart.combine hs))
      end)
    end

  | Stable t ->
    let rec aux result = match result with
      | Some (_, h) when Heart.is_broken h ->
        sample t ~cancel >>= aux
      | None when not (Heart.is_broken cancel) ->
        sample t ~cancel >>= aux
      | _ -> return result
    in
    sample t ~cancel >>= aux

(*| Apply (tf,ta) ->
    let da = sample ta ~cancel in
    sample tf ~cancel >>=
    begin function
    | None -> Deferred.return None
    | Some (f,hf) -> da >>| function
      | None -> None
      | Some (a,ha) -> Some (f a, Heart.combine2 hf ha)
    end
  | Defer t -> sample ~cancel t >>=
    begin function
      | Some (d,h) -> d >>| (fun v -> Some (v,h))
      | None -> Deferred.return None
    end
  | Lazy (lazy t) -> sample t ~cancel*)

let reify d =
  match d with
  | Reified _ -> d
  | _ ->
    let glass = Heart.Glass.create ~desc:"ten/reify" in
    let shared_glass = Shared_glass.share glass in
    let value = sample d ~cancel:(Heart.of_glass glass) in
    Reified (d, {shared_glass; value})
