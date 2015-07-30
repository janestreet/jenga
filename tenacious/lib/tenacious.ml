
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


let race a b =
  Embed (fun ~cancel ->
    let cancel_a = Heart.Glass.create () in
    let cancel_b = Heart.Glass.create () in
    let ad = sample ~cancel:(Heart.combine2 cancel (Heart.watch cancel_a)) a in
    let bd = sample ~cancel:(Heart.combine2 cancel (Heart.watch cancel_b)) b in
    Deferred.choose
      [ choice ad (fun x -> `a x)
      ; choice bd (fun x -> `b x)
      ] >>| function
    | `a res ->
      Glass.break cancel_b;
      res
    | `b res ->
      Glass.break cancel_a;
      res)

let map2 at bt ~f =
  all [
    (map at ~f:(fun a -> `a a));
    (map bt ~f:(fun b -> `b b));
  ] |>
  map ~f:(function
    | [`a a; `b b] -> f a b
    | _ -> assert false)

let both =
  map2 ~f:(fun x y -> x, y)

let race_error (type e) =
  let go cancel =
    let module M : sig
      val go :
        ('a, e) Result.t t
        -> ('b, e) Result.t t
        -> f:('a -> 'b -> 'c)
        -> ('c, e) Result.t comp

    end = struct

      (** a computation that might still be running with the ability to cancel and
          re-start *)
      type 'a computation =
        { tenacious : ('a, e) Result.t t
        ; deferred : ('a, e) Result.t comp
        ; cancel : Heart.Glass.t
        }

      let spawn (tenacious : ('a, e) Result.t t) : 'a computation =
        let g = Heart.Glass.create () in
        { tenacious
        ; deferred =
            sample tenacious
              ~cancel:(Heart.combine2 cancel (Heart.watch g))
        ; cancel = g
        }

      let rec
        one_done : (* [a] still running; [b] is done *)
        'a 'b 'c.
                'a computation
        -> ('b, e) Result.t t * ('b, e) Result.t result
        -> f_ab:('a -> 'b -> 'c)
        -> f_ba:('b -> 'a -> 'c)
        -> ('c, e) Result.t comp
        =
        fun
          a (b, b_result)
          ~f_ab ~f_ba
          -> match b_result with
            | None -> (* [b] was cancelled, hopefully not by us *)
              assert (Heart.is_broken cancel);
              Deferred.return None
            | Some (Error e, b_heart) -> (* [b] is in error *)
              Heart.Glass.break a.cancel;
              Deferred.return (Some (Error e, b_heart))
            | Some (Ok b_value, b_heart) -> (* b is good, let's wait for [a] as well *)
              Heart.or_broken b_heart a.deferred
              >>= function
              | None -> (* [b_heart] was broken *)
                both_running a (spawn b) ~f_ab ~f_ba
              | Some None -> (* [a] was cancelled, hopefully not by us *)
                assert (Heart.is_broken cancel);
                Deferred.return None
              | Some (Some (Error e, a_heart)) -> (* [a] is in error *)
                Deferred.return (Some (Error e, a_heart))
              | Some (Some (Ok a_value, a_heart)) -> (* both are good! *)
                Deferred.return
                  (Some (Ok (f_ab a_value b_value), Heart.combine2 a_heart b_heart))
      and
        both_running :
        'a 'b 'c. 'a computation
        -> 'b computation
        -> f_ab:('a -> 'b -> 'c)
        -> f_ba:('b -> 'a -> 'c)
        -> ('c, e) Result.t comp
        =
        fun
          a b
          ~f_ab ~f_ba
          ->
            Deferred.choose
              [ choice a.deferred (fun x -> `a x)
              ; choice b.deferred (fun x -> `b x)
              ] >>= function
            | `a result ->
              one_done b (a.tenacious, result) ~f_ba:f_ab ~f_ab:f_ba
            | `b result ->
              one_done a (b.tenacious, result) ~f_ba ~f_ab

      let go a b ~f =
        both_running
          (spawn a)
          (spawn b)
          ~f_ab:f
          ~f_ba:(Fn.flip f)
    end
    in
    M.go
  in
  fun a b ~f -> Embed (fun ~cancel -> go cancel a b ~f)

let embed f = Embed f

let lift f =
  (* Make initial check of [cancel]; afterwards [cancel] is ignored.
     Waits until [f ()] is determined before returning *)
  embed (fun ~cancel ->
    if Heart.is_broken cancel then Deferred.return None else
      f () >>| fun def -> Some def
  )

let with_semantics inner_t ~f =
  Embed (fun ~cancel ->
    f (fun ~cancel -> sample inner_t ~cancel) ~cancel
  )

let bracket t ~running ~finished ~cancelled =
  let count = ref 0 in
  Embed (fun ~cancel ->
    running !count;
    incr count;
    sample t ~cancel >>| fun res ->
    begin match res with
    | None -> cancelled ()
    | Some (res, _) -> finished res
    end;
    res
  )

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

module Var = struct
  type 'a t = {
    mutable value : 'a;
    mutable glass : Heart.Glass.t
  }
  let create value =
    { value
    ; glass = Heart.Glass.create ()
    }
  let watch var =
    embed (fun ~cancel:_ ->
      Deferred.return (Some (var.value, Heart.watch var.glass))
    )
  let set var value =
    let old_glass = var.glass in
    var.value <- value;
    var.glass <- Heart.Glass.create ();
    Heart.Glass.break old_glass

  let get var =
    var.value

  let replace var ~f =
    set var (f (get var))
end

let in_progress_cutoff_count_is_zero = Var.create true
let in_progress_cutoff_count = ref 0
let incr_in_progress_cutoff_count () =
  let old = !in_progress_cutoff_count in
  in_progress_cutoff_count := old + 1;
  if Int.(=) old 0
  then
    Var.set in_progress_cutoff_count_is_zero false
let decr_in_progress_cutoff_count () =
  decr in_progress_cutoff_count;
  if Int.(=) (!in_progress_cutoff_count) 0
  then
    Var.set in_progress_cutoff_count_is_zero true

let protecting_cutoffs (Embed t) = Embed (
  fun ~cancel ->
    t ~cancel
    >>= function
    | None -> Deferred.return None
    | Some (res, heart) ->
      let ok = Deferred.return (Some (res, heart)) in
      if Heart.is_broken heart
      then
        ok
      else
        let rec wait_for_cutoffs () =
          exec (Var.watch in_progress_cutoff_count_is_zero)
          >>= fun (no_cutoffs_running, cutoffs_heart) ->
          if no_cutoffs_running
          then
            ok
          else
            Heart.when_broken (Heart.combine [heart; cutoffs_heart; cancel])
            >>= fun () ->
            if Heart.is_broken heart || Heart.is_broken cancel
            then
              ok
            else
              wait_for_cutoffs ()
        in
        wait_for_cutoffs ())

let cutoff ~equal ten =
  embed (fun ~cancel:_ ->
    let my_weak_glass, my_heart = Weak_glass.create () in
    exec ten >>| fun (first_result,first_heart) ->
    let rec loop heart =
      Heart.upon_broken_or_determined heart (Weak_glass.unwatched my_weak_glass)
      ~broken:(fun () ->
          incr_in_progress_cutoff_count ();
          exec ten >>> fun (replacement_result,heart) ->
          decr_in_progress_cutoff_count ();
          if (equal first_result replacement_result)
          then loop heart
          else Weak_glass.break my_weak_glass
        )
      ~determined:(fun () -> ())
    in
    loop first_heart;
    Some (first_result, my_heart)
  )

let race_errors errors =
  let rmap ~f = map ~f:(Result.map ~f) in
  (* reduce_balanced to avoid potential inefficiencies due to long chains of dependencies
     Dlist to bring the complexity further down from O(n*log(n)) to O(n)
  *)
  List.reduce_balanced
    ~f:(race_error ~f:(Dlist.(@)))
    (List.map ~f:(rmap ~f:Dlist.singleton) errors)
  |> Option.value ~default:(return (Ok Dlist.empty))
  |> rmap ~f:Dlist.to_list

(**
   Alternative implementations of functions, intended for tests.
   These are intended to be easy to understand so we can test the real implementations
   against them.

   Here we also implement some of the primitives one in terms of the other.
*)
module For_tests = struct

  let race_error a b ~f =
    let a = reify a in
    let b = reify b in
    bind
      (race (map ~f:(fun x -> `a x) a) (map ~f:(fun x -> `b x) b))
      (function
        | `a (Error e)
        | `b (Error e) -> return (Error e)
        | `a (Ok a) -> map ~f:(Result.map ~f:(fun b -> f a b)) b
        | `b (Ok b) -> map ~f:(Result.map ~f:(fun a -> f a b)) a)

  let map2 a b ~f =
    let no_error = function
      | Ok x -> x
      | Error e -> never_returns e
    in
    race_error ~f (map ~f:(fun x -> Ok x) a) (map ~f:(fun x -> Ok x) b)
    |> map ~f:no_error

  let both = map2 ~f:(fun a b -> (a, b))

  let all x =
    List.map x ~f:(map ~f:List.return)
    |> List.reduce_balanced ~f:(map2 ~f:(@))
    |> Option.value ~default:(return [])

  let all_via_race_errors ts =
    map
      ~f:(function
        | Ok x -> x
        | Error _ -> assert false)
      (race_errors (List.map ts ~f:(map ~f:(fun x -> Ok x))))
end

let all_ignore = all_unit
let join x = bind x (fun x -> x)
let ignore_m = map ~f:ignore
let (>>|) x f = map x ~f
let (>>=) = bind
module Monad_infix = struct
  let (>>|) = (>>|)
  let (>>=) = (>>=)
end

module Result = struct

  module Tenacious = struct
    let return = return
    let bind = bind
    let map = map
  end
  module T = struct
    type nonrec ('a, 'e) t = ('a, 'e) Result.t t
  end

  include T

  include Monad.Make2 (struct
    include T

    let return a = Tenacious.return (Ok a)

    let bind t f =
      Tenacious.bind t (function
        | Ok a -> f a
        | Error _ as error -> Tenacious.return error)
    ;;

    let map t ~f = Tenacious.map t ~f:(fun r -> Result.map r ~f)
    let map = `Custom map
  end)

  let fail e = Tenacious.return (Error e)
  let map_error t ~f = Tenacious.map t ~f:(fun r -> Result.map_error r ~f)

end
