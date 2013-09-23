
open Core.Std
open Async.Std


module Shared_glass = struct
  type t = {mutable users: int; mutable glass: Heart.Glass.t}
  let share glass = {users = 0; glass}
  let is_broken t = Heart.Glass.is_broken t.glass
  let restart t g = t.glass <- g

  let use t =
    t.users <- t.users + 1

  let release t =
    assert (t.users > 0);
    t.users <- t.users - 1;
    if t.users = 0 then Heart.Glass.break t.glass
end

type 'a v = ('a * Heart.t) option

type 'a state = {
  shared_glass : Shared_glass.t;
  mutable value : 'a v Deferred.t;
}

type _ t =
  | Pure    : 'a -> 'a t
  | Lift    : (cancel:Heart.t -> 'a v Deferred.t) -> 'a t
  | Map     : 'a t * ('a -> 'b) -> 'b t
  | All     : 'a t list -> 'a list t
  | Reified : 'a t * 'a state -> 'a t
  | Bind    : 'a t * ('a -> 'b t) -> 'b t
  | Stable  : 'a t -> 'a t
  | With_ten : 'a t * (cancel:Heart.t -> (cancel:Heart.t -> ('a * Heart.t) option Deferred.t) -> ('b * Heart.t) option Deferred.t) -> 'b t

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
let with_ten t ~f = With_ten (t,f)

let or_cancel ~cancel ~shared_glass d =
  Shared_glass.use shared_glass;
  Deferred.create (fun ivar ->
    let fill_if_empty v =
      if Ivar.is_empty ivar then
        (Shared_glass.release shared_glass; Ivar.fill ivar v)
    in
    let on_cancel () = fill_if_empty None in
    let canceller = Heart.upon cancel ~f:on_cancel in
    d >>> fun r ->
      Heart.cancel canceller;
      fill_if_empty r
  )

let rec sample : 'a. 'a t -> cancel:Heart.t -> 'a v Deferred.t =
  fun (type a) (t : a t) ~cancel -> match t with
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
        sample ~cancel:(Heart.combine2 h1 cancel) (f a) >>= function
        | None when Heart.is_broken cancel ->
          Deferred.return None
        | None -> sample ~cancel t
        | Some (a,h2) -> 
          Deferred.return (Some (a,Heart.combine2 h1 h2))
    end

  | Reified (t,st) ->
    let shared_glass = st.shared_glass in
    let resample () =
      let glass = Heart.Glass.create ~desc:"Ten.Reified" in
      Shared_glass.restart shared_glass glass;
      st.value <- sample t ~cancel:(Heart.of_glass glass);
      st.value
    in
    begin match Deferred.peek st.value with
    (* Computation has finished, result is still valid *)
    | Some (Some (_d,h)) when not (Heart.is_broken h) -> st.value
    (* Computation has finished, result is invalid *)
    | Some _ -> or_cancel ~cancel ~shared_glass (resample ())
    (* Computation is running and was not cancelled *)
    | None when not (Shared_glass.is_broken st.shared_glass) ->
      or_cancel ~cancel ~shared_glass st.value
    (* Computation is running and was cancelled.
     * - wait for a potential result
     * - restart if needed, make sure not to restart multiple times *)
    | _ -> 
      or_cancel ~cancel ~shared_glass begin st.value >>= function
          (* We have a hesult *)
        | Some (_d,h) as result when not (Heart.is_broken h) ->
          return result
          (* No longer interested in the result *)
        | _ when Heart.is_broken cancel ->
          return None
          (* No result or invalid one *)
        | _ when Shared_glass.is_broken st.shared_glass ->
          resample ()
          (* Current result is invalid, but a new computation was started
           * and value got mutated. *)
        | _ -> st.value
      end
    end

  | All ts ->
    Deferred.create
    begin fun ivar ->
      let final_glass = Heart.Glass.create_with_deps [cancel] ~desc:"Ten.All" in
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
                Heart.cancel !canceller;
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
        let fetch (cell,canceller) =
          Heart.cancel !canceller;
          !cell
        in
        Deferred.all (List.map ~f:fetch results) >>> fun results ->
        try
          let extract = function
            | Some (r, h) when not (Heart.is_broken h) -> r, h
            | _ -> raise Not_found
          in
          let rs, hs = List.unzip (List.map ~f:extract results) in
          Ivar.fill ivar (Some (rs, Heart.combine hs))
        with Not_found -> Ivar.fill ivar None
      end)
    end

  | Stable t ->
    let rec aux = function
      | Some (_, h) when Heart.is_broken h ->
        sample t ~cancel >>= aux
      | None when not (Heart.is_broken cancel) ->
        sample t ~cancel >>= aux
      | result -> return result
    in
    sample t ~cancel >>= aux

  | With_ten (a,f) -> 
    let cb ~cancel = sample a ~cancel in
    let rec aux = function
      | None when not (Heart.is_broken cancel) -> f ~cancel cb >>= aux
      | result -> return result
    in
    (f ~cancel cb >>= aux : a v Deferred.t)

let reify d =
  match d with
  | Reified _ -> d
  | _ ->
    let glass = Heart.Glass.create ~desc:"Ten.Reify" in
    let shared_glass = Shared_glass.share glass in
    let value = sample d ~cancel:(Heart.of_glass glass) in
    Reified (d, {shared_glass; value})
