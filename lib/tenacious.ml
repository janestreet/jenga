
open Core.Std
open Async.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

(*
    The core semantics of Tenacious.t evaluation is handled by a function
    of the following type:

        val sample :
            'a Tenacious.t ->
            cancel:Heart.t -> ('a * Heart.t) option Deferred.t


    When a tenacious computation is [sample]ed, a result is obtained
    eventually (Deferred.t), that in the normal case is [Some(x,heart)],
    where [x] is the value of the result; and [heart] is a certificate of
    validity. If this heart is ever broken (triggered), then the result is
    to be regarded as invalidated; the tenacious should be re-evaluated.

    The caller may express his disinterest in a result previously [sample]ed
    by breaking the [cancel] heart. In this case (and only this case) may
    the result become determined by None, (but None is not guaranteed).

    The point of the tenacious monad is to manage the plumbing of the
    cancel & invalidation hearts & perform the re-evaluation of invalided
    computations.

    The following invariant was not correctly maintained:

        The deferred MUST always become determined, (even in the case that
        the computation is cancelled.)


    It's not immediately obvious why such an invariant is necessary: If
    the caller cancels, then why would he wait on the deferred?  It is
    necessary because a cancelled tenacious computation (which has not
    actually been stopped) may become wanted again. In this case the new
    caller would wait for the result of the cancelled-but-still-running
    computation to become determined.
*)

type 'a result = ('a * Heart.t) option Deferred.t
type 'a semantics = cancel:Heart.t -> 'a result

module type Inner_sig = sig

  type 'a t

  val sample : 'a t -> cancel:Heart.t -> 'a result
  val embed : (cancel:Heart.t -> 'a result) -> 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val all : 'a t list -> 'a list t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val reify : 'a t -> 'a t

  val with_semantics : 'a t -> f:('a semantics -> 'b semantics) -> 'b t

end

module Inner : Inner_sig = struct

  module Shared_glass = struct

    type t = {mutable users: int; mutable glass: Heart.Glass.t}

    let share glass = {users = 0; glass}
    let is_broken t = Heart.Glass.is_broken t.glass
    let restart t g = t.glass <- g

    let use t =
      t.users <- t.users + 1

    let release t =
      assert (Int.(t.users > 0));
      t.users <- t.users - 1;
      if Int.(t.users = 0) then Heart.Glass.break t.glass

  end

  type 'a state = {
    shared_glass : Shared_glass.t;
    mutable value : 'a result;
  }

  type _ t =
  | Return          : 'a -> 'a t
  | Embed           : (cancel:Heart.t -> 'a result) -> 'a t
  | Map             : 'a t * ('a -> 'b) -> 'b t
  | All             : 'a t list -> 'a list t
  | Reified         : 'a t * 'a state -> 'a t
  | Bind            : 'a t * ('a -> 'b t) -> 'b t
  | With_semantics  : 'a t * ('a semantics -> 'b semantics) -> 'b t

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

  let rec sample : 'a. 'a t -> cancel:Heart.t -> 'a result =
    fun (type a) (t : a t) ~cancel -> match t with

    | Return a -> Deferred.return (Some (a, Heart.unbreakable))

    | Embed f -> f ~cancel

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
          Deferred.return result
        (* No longer interested in the result *)
        | _ when Heart.is_broken cancel ->
          Deferred.return None
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
          let none = Deferred.return None in
          let sample_one t =
            let cell = ref none and canceller = ref None in
            let rec resample () =
              if Heart.Glass.is_broken final_glass then () else
                begin
                  Shared_glass.use shared_glass;
                  cell :=
                    begin sample t ~cancel >>| function
                    | None ->
                      (*assert (Heart.is_broken cancel);*)
                      (* asserting no longer valid, following change:

                         -      let never = Deferred.never () in
                         +      let none = Deferred.return None in
                      *)
                      None (* Normally not possible unless cancel broken!!*)

                    | Some (_,h) as result ->
                      Heart.cancel !canceller;
                      canceller := Heart.upon h ~f:resample;
                      Shared_glass.release shared_glass;
                      result
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

    | With_semantics (a,f) ->
      let cb ~cancel = sample a ~cancel in
      let rec aux = function
        | None when not (Heart.is_broken cancel) -> f cb ~cancel >>= aux
        | result -> Deferred.return result
      in
      f cb ~cancel >>= aux

  let return x  = Return x
  let embed f   = Embed f
  let bind m f  = Bind (m,f)
  let all l     = All l

  let map m ~f  = match m with
    | Return x -> Return (f x)
    | Map (m,f') -> Map (m, fun x -> f (f' x))
    | _ -> Map (m,f)

  let with_semantics t ~f = With_semantics (t, f)

  let reify d =
    match d with
    | Reified _ -> d
    | _ ->
      let glass = Heart.Glass.create ~desc:"Ten.Reify" in
      let shared_glass = Shared_glass.share glass in
      let value = sample d ~cancel:(Heart.of_glass glass) in
      Reified (d, {shared_glass; value})

end


include Inner

let all = function
  | [] -> return []
  | [x] -> map x ~f:(fun v -> [v])
  | xs -> all xs

let all_unit ts =
  map (all ts) ~f:(fun (_:unit list) -> ())

let exec t =
  sample t ~cancel:Heart.unbreakable >>| function
  | None -> assert false
  | Some v -> v

let lift f =
  embed (fun ~cancel:_ ->
    f () >>| Option.some
  )
