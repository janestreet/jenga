
open! Core.Std
open Async.Std

module Heart = Heart
module Glass = Heart.Glass
module Weak_glass = Heart.Weak_glass

let version = sprintf "new-tenacious; %s" Heart.version

type 'a result = ('a * Heart.t) option (* [None] when cancelled *)
type 'a comp = 'a result Deferred.t
type 'a semantics = cancel: Heart.t -> 'a comp

type 'a t =
| Embed : 'a semantics -> 'a t

let sample = function
  | Embed sem -> sem

let bind t1 f =
  Embed (fun ~cancel ->
    let rec again() =
      sample t1 ~cancel >>= function
      | None -> Deferred.return None
      | Some (v1,certificate1) ->
        let t2 = f v1 in
        sample t2 ~cancel:(Heart.combine2 cancel certificate1) >>= function
        | None ->
          (* [t2] was cancelled *)
          if (Heart.is_broken cancel) then (* so was [t] *) Deferred.return None
          else (
            (* [t] has not been cancelled; cancellation of [t2] must be because [v1] has
               been invalidated. Must sample again. *)
            assert (Heart.is_broken certificate1);
            again()
          )
        | Some (v2,certificate2) ->
          Deferred.return (Some (v2,Heart.combine2 certificate1 certificate2))
    in
    again()
  )

let return v =
  Embed (fun ~cancel:_ ->
    Deferred.return (Some (v,Heart.unbreakable))
  )

let map t1 ~f =
  Embed (fun ~cancel ->
    sample t1 ~cancel >>| function
    | None -> None
    | Some (v1,certificate) -> Some (f v1, certificate)
  )

let all = function
  | [] -> return []
  | [t] -> map t ~f:(fun v -> [v])
  | subts ->
    Embed (fun ~cancel ->
      let subt_cells = List.map subts ~f:(fun t -> t,ref None) in
      let ivar = Ivar.create () in
      let finished = Ivar.read ivar in
      let collect_results_and_finish =
        fun () ->
          (* This function must be called exactly once, but not until either:
             - [t] has been cancelled, or
             - we have collected a value from each [subt] *)
          Ivar.fill ivar (
            if Heart.is_broken cancel
            then None
            else
              let values,hearts =
                List.unzip (List.map subt_cells ~f:(fun (_,x) -> Option.value_exn (!x)))
              in
              Some (values, Heart.combine hearts)
          )
      in
      let count =
        (* the number of sub-tenacious sampled, but still running *)
        ref 0
      in
      List.iter subt_cells ~f:(fun (subt,r) ->
        let rec loop () =
          incr count;
          sample subt ~cancel >>> fun res ->
          decr count;
          let certificate_opt =
            match res with
            | None -> None
            | Some ((_,certificate) as res) ->
              (* [res] is stashed before checking if [subt] is the last to finish *)
              (r := Some res); Some certificate
          in
          if (!count = 0)
          then collect_results_and_finish()
          else
            match certificate_opt with
            | None ->
              (* This [subt] was cancelled (along with all the others).  We weren't the
                 last to finish, so we don't call [collect_results_and_finish] *)
              ()
            | Some certificate ->
              (* This [subt] has completed with a value. While our siblings are still
                 running, watch we dont become invalidated. If we do, sample again. *)
              Heart.or_broken certificate finished >>> function
              | Some _ -> () (* finished *)
              | None ->
                if not (Deferred.is_determined finished) then (
                  (* certificate invalidated; not finished *)
                  loop ()
                )
        in
        loop ()
      );
      assert (!count > 0); (* or else we never decrement to reach 0 and finish.
                              Ensured by special handling for [all []] *)
      finished
  )

let all_unit ts =
  map (all ts) ~f:(fun (_:unit list) -> ())

type 'a state =
| Wait of 'a comp
| Running of (cancel:Heart.t -> 'a comp)
| Ready of ('a * Heart.t)

(*
  When a computation instance has completed, the certified result is remembered for
  subsequent samplers.  Each subsequent sampler checks the certificate, and will begin a
  new shared computation if the certificate is invalid.

  Whilst the computation is running, subsequent samplers [join] the single running
  computation.  Clients may cancel, and new clients may join, at any time.  When a
  clients cancels it receives a `cancellation-occurred' response `[None]' immediately.
  If that client is only remaining client then the computation is cancelled.

  When a computation has been cancelled, the next sampling is responsible for starting a
  new computation instance, if necessary.  The computation instance will not start until
  the previous instance has occurred.  If the old computation, despite having been
  cancelled, returns a result with a valid certificate, that result can be used, and
  re-sampling is avoided.
*)

let reify inner_t =
  let state = ref (Wait (Deferred.return None)) in
  Embed (fun ~cancel ->
    let start ~last =
      let stop_glass =
        (* This is filled to cancel the [shared_computation] *)
        Glass.create ()
      in
      let stop_heart = Heart.watch stop_glass in
      let shared_computation =
        (* wait for the previous (cancelled) computation to finish *)
        last >>= fun res ->
        let last_still_valid =
          match res with
          | None -> false
          | Some (_,cert) -> not (Heart.is_broken cert)
        in
        (* if the last computation returns a result despite having been cancelled, we can
           use that result if the certificate is still valid, avoiding a new sampling *)
        if last_still_valid
        then last
        else sample inner_t ~cancel:stop_heart
      in
      let count = ref 0 in
      let join ~cancel:my_cancel =
        (* join a new client, with its own [my_cancel] to the running [shared_computation] *)
        incr count;
        Heart.or_broken my_cancel shared_computation >>= function
        | Some _ -> shared_computation
        | None -> (* this client has cancelled *)
          assert (!count >= 1);
          decr count;
          if (!count = 0) then (
            state := Wait shared_computation;
            Glass.break stop_glass
          );
          Deferred.return None
      in
      begin
        shared_computation >>> function
        | None -> () (* was stopped *)
        | Some (x,certificate) ->
          (* wasn't stopped, of stop requested *)
          if Heart.is_broken stop_heart then (
          (* Cancel of [shared_computation] requested, but it returned a result anyway.
             We cannot prevent this race. *)
          ) else (
            state := Ready (x,certificate)
          )
      end;
      state := Running join;
      join ~cancel
    in
    match !state with
    | Wait last -> start ~last
    | Running join -> join ~cancel
    | Ready ((_,certificate) as res) ->
      if Heart.is_broken certificate
      then start ~last:(Deferred.return None)
      else Deferred.return (Some res)
  )

let embed f = Embed f

let lift f =
  (* Make initial check of [cancel]; afterwards [cancel is ignored].
     Waits until [f ()] is determined before returning *)
  embed (fun ~cancel ->
    if Heart.is_broken cancel then Deferred.return None else
      f () >>| fun def -> Some def
  )

let with_semantics inner_t ~f =
  Embed (fun ~cancel ->
    f (fun ~cancel -> sample inner_t ~cancel) ~cancel
  )

let before_redo t ~f =
  let first_time = ref true in
  let hook () =
    if !first_time
    then first_time := false
    else f ()
  in
  let f sample ~cancel = hook (); sample ~cancel in
  with_semantics t ~f

let uncancellable t =
  with_semantics t ~f:(fun sample ~cancel:__ ->
    sample ~cancel:Heart.unbreakable
  )

let desensitize t =
  let f sample ~cancel =
    sample ~cancel >>| function
    | None -> None
    | Some (x, heart) ->
      Some ((x,heart), Heart.unbreakable)
  in
  with_semantics t ~f

let exec t =
  sample t ~cancel:Heart.unbreakable >>| function
  | None -> assert false
  | Some result -> result

let cutoff ~equal ten =
  embed (fun ~cancel:_ ->
    let my_weak_glass, my_heart = Weak_glass.create () in
    exec ten >>| fun (first_result,first_heart) ->
    let rec loop heart =
      Heart.or_broken heart (Weak_glass.unwatched my_weak_glass) >>>
      function
      | None ->
        exec ten >>> fun (replacement_result,heart) ->
        if (equal first_result replacement_result)
        then loop heart
        else Weak_glass.break my_weak_glass
      | Some () ->
        ()
    in
    loop first_heart;
    Some (first_result, my_heart)
  )
