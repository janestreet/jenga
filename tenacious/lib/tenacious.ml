open! Core.Std
open Async.Std
open! Int.Replace_polymorphic_compare

module Heart = Heart
module Glass = Heart.Glass
module Weak_glass = Heart.Weak_glass

let version = sprintf "new-tenacious; %s" Heart.version

type 'a result = ('a * Heart.t) option (* [None] when cancelled *)
type 'a comp = 'a result Deferred.t

(* The [~dep] is both an additional signal to cancel the computation (i.e. a second
   [cancel]), and must be made a dependency of any result of the computation.  In other
   words, [f] must behave like:

     fun ~cancel ~dep ->
       f ~cancel:(Heart.combine cancel dep) ~dep:Heart.unbreakable
       >>| Option.map ~f:(fun (res, heart_res) -> (res, Heart.combine heart_res dep))

   but sometimes it can do so more efficiently (less intermediate hearts). In practice,
   right now, the [dep] are dependencies of the left hand side of surrounding binds. *)
type 'a semantics = cancel:Heart.t -> dep:Heart.t -> 'a comp

type 'a state =
| Wait of 'a comp
| Running of 'a semantics
| Ready of 'a * Heart.t

type 'a t =
  | Return : 'a -> 'a t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | All : 'a t list -> 'a list t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Reify : 'a state ref * 'a t Lazy.t -> 'a t
  | Lift : (unit -> ('a * Heart.t) Deferred.t) -> 'a t
  | Embed : 'a semantics -> 'a t

let rec reduce : type a . a t -> a t =
  fun t ->
    match t with
    | Return _ -> t
    | Map (s, f) -> begin
        let s' = reduce s in
        match s' with
        | Return x -> Return (f x)
        | _ -> if phys_equal s s' then t else Map(s', f)
      end
    | All ss ->
        let ss' = reduce_list ss in
        let rec loop acc = function
          | [] -> Return (List.rev acc)
          | Return x :: rest -> loop (x :: acc) rest
          | _ :: _ ->
            if phys_equal ss ss' then t else All ss'
        in
        loop [] ss'
    | Bind (s, k) -> begin
        let s' = reduce s in
        match s' with
        | Return x -> reduce (k x)
        | _ -> if phys_equal s s' then t else Bind(s', k)
      end
    | Reify (_, lazy s) -> begin
        (* The lazy contains the already reduced version. *)
        match s with
        | Return _ -> s
        | _ -> t
      end
    (* It is up to the function building the closures in these contructors to reduce any
       [t] the closures [sample]. *)
    | Lift _ -> t
    | Embed _ -> t

and reduce_list : type a . a t list -> a t list =
  fun ts ->
    let rec loop eq acc = function
      | [] -> if eq then ts else List.rev acc
      | t :: rest ->
        let t' = reduce t in
        let eq = eq && phys_equal t t' in
        loop eq (t' :: acc) rest
    in
    loop true [] ts

let combine_result comp ~with_:heart =
  if Heart.is_unbreakable heart then comp
  else
    comp >>| function
    | None -> None
    | Some(v, certificate) -> Some(v, Heart.combine2 certificate heart)

let rec sample : type a . a t -> a semantics =
  fun t ~cancel ~dep ->
    match t with
    | Return x -> return x ~cancel ~dep
    | Bind (t, f) -> bind t f ~cancel ~dep
    | Map (t, f) -> map t f ~ cancel ~dep
    | All ts -> all ts ~cancel ~dep
    | Reify (st, lazy t) -> reify st t ~cancel ~dep
    | Lift f -> lift f ~cancel ~dep
    | Embed emb -> emb ~cancel ~dep

and bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b semantics =
  fun t1 f ~cancel ~dep ->
    sample t1 ~cancel ~dep >>= function
    | None -> Deferred.return None
    | Some (v1, certificate1) ->
      match reduce (f v1) with
      | Return v2 -> Deferred.return (Some (v2, certificate1))
      | t2 ->
        sample t2 ~cancel ~dep:certificate1 >>= function
        | None ->
            (* [t2] was cancelled *)
          if Heart.is_broken cancel || Heart.is_broken dep then
            (* so was [t] *)
            Deferred.return None
          else begin
            (* [t] has not been cancelled.
               Cancellation of [t2] must be because [v1] has been invalidated.
               Must sample again. *)
            assert (Heart.is_broken certificate1);
            bind t1 f ~cancel ~dep
          end
        | Some _ as res -> Deferred.return res

and return : 'a . 'a -> 'a semantics =
  fun v ~cancel:_ ~dep ->
    Deferred.return (Some (v, dep))

and map : 'a 'b. 'a t -> ('a -> 'b) -> 'b semantics =
  fun t1 f ~cancel ~dep ->
    sample t1 ~cancel ~dep >>| function
    | None -> None
    | Some (v1, certificate) -> Some (f v1, certificate)

and all : 'a . 'a t list -> 'a list semantics =
  fun ts ~cancel ~dep ->
    let cancel = Heart.combine2 cancel dep in
    let len = List.length ts in
    let cells = Array.create ~len None in
    let ivar = Ivar.create () in
    let finished = Ivar.read ivar in
    let collect_results_and_finish () =
      (* This function must be called exactly once, but not until either:
         - [t] has been cancelled, or
         - we have collected a value from each [t] *)
      let res =
        if Heart.is_broken cancel
        then None
        else begin
          let results = Array.map cells ~f:(fun v -> Option.value_exn v) in
          let values, hearts = Array.unzip results in
          let values = Array.to_list values in
          let hearts = Array.to_list hearts in
          let heart = Heart.combine (dep :: hearts) in
          Some (values, heart)
        end
      in
      Ivar.fill ivar res
    in
    let count =
      (* the number of sub-tenacious sampled, but still running *)
      ref 0
    in
    List.iteri ts ~f:(fun i t ->
      match t with
      | Return x -> cells.(i) <- Some (x, Heart.unbreakable)
      | _ ->
        let rec loop () =
          incr count;
          sample t ~cancel ~dep:Heart.unbreakable >>> fun res ->
          decr count;
          let certificate_opt =
            match res with
            | None -> None
            | Some ((_,certificate) as res) ->
              (* [res] is stashed before checking if [t] is the last to finish *)
              cells.(i) <- Some res;
              Some certificate
          in
          if (!count = 0)
          then collect_results_and_finish()
          else
            match certificate_opt with
            | None ->
              (* This [t] was cancelled (along with all the others).  We weren't the
                 last to finish, so we don't call [collect_results_and_finish] *)
              ()
            | Some certificate ->
              (* This [t] has completed with a value. While our siblings are still
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
                            Ensured by the smart constructor [all]. *)
    finished

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
and reify : 'a . 'a state ref -> 'a t -> 'a semantics =
  fun state t ~cancel ~dep ->
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
        else sample t ~cancel:stop_heart ~dep:Heart.unbreakable
      in
      let count = ref 0 in
      let join ~cancel:my_cancel ~dep:my_dep =
        (* join a new client, with its own [my_cancel] to the running [shared_computation] *)
        let my_cancel = Heart.combine2 my_cancel my_dep in
        incr count;
        Heart.or_broken my_cancel shared_computation >>= function
        | Some _ -> combine_result shared_computation ~with_:my_dep
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
            state := Ready(x, certificate)
          )
      end;
      state := Running join;
      join ~cancel ~dep
    in
    match !state with
    | Wait last -> start ~last
    | Running join -> join ~cancel ~dep
    | Ready(x, certificate) ->
      if Heart.is_broken certificate
      then start ~last:(Deferred.return None)
      else Deferred.return (Some(x, Heart.combine2 certificate dep))

and lift : type a . (unit -> (a * Heart.t) Deferred.t) -> a semantics =
  fun f ~cancel ~dep ->
    (* Make initial check of [cancel]; afterwards [cancel] is ignored.
       Waits until [f ()] is determined before returning *)
    if Heart.is_broken cancel || Heart.is_broken dep
    then Deferred.return None
    else f () >>| fun (v, heart) -> Some (v, Heart.combine2 heart dep)

let return x = Return x

let bind t f = Bind(t, f)

let map t ~f = Map(t, f)

let all = function
  | [] -> Return []
  | [t] -> Map(t, List.return)
  | ts -> All ts

let lift f = Lift f

let reify t =
  let state = ref (Wait (Deferred.return None)) in
  Reify (state, lazy (reduce t))

let embed f =
  Embed (fun ~cancel ~dep ->
    let cancel = Heart.combine2 cancel dep in
    combine_result (f ~cancel) ~with_:dep
  )

let all_unit ts =
  map (all ts) ~f:(fun (_:unit list) -> ())

let race a b =
  Embed (fun ~cancel ~dep ->
    let cancel_a = Heart.Glass.create () in
    let cancel_b = Heart.Glass.create () in
    let ad =
      match reduce a with
      | Return v -> Deferred.return (Some (v, dep))
      | a ->
        sample ~cancel:(Heart.combine2 cancel (Heart.watch cancel_a)) ~dep a
    in
    let bd =
      match reduce b with
      | Return v -> Deferred.return (Some (v, dep))
      | b ->
        sample ~cancel:(Heart.combine2 cancel (Heart.watch cancel_b)) ~dep b
    in
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
  let go cancel dep =
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
        ; deferred = begin
            match reduce tenacious with
            | Return v -> Deferred.return (Some (v, dep))
            | tenacious ->
                sample tenacious
                ~cancel:(Heart.combine2 cancel (Heart.watch g))
                ~dep
          end
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
              assert (Heart.is_broken cancel || Heart.is_broken dep);
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
                assert (Heart.is_broken cancel || Heart.is_broken dep);
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
  fun a b ~f -> Embed (fun ~cancel ~dep -> go cancel dep a b ~f)

let with_semantics inner_t ~f =
  Embed (fun ~cancel ~dep ->
    f (fun ~cancel ~dep -> sample (reduce inner_t) ~cancel ~dep)
      ~cancel ~dep)

let bracket t ~running ~finished ~cancelled =
  let count = ref 0 in
  Embed (fun ~cancel ~dep ->
    running !count;
    incr count;
    sample (reduce t) ~cancel ~dep >>| fun res ->
    begin match res with
    | None -> cancelled ()
    | Some (v, _) -> finished v
    end;
    res
  )

let uncancellable t =
  with_semantics t ~f:(fun sample ~cancel:_ ~dep ->
    combine_result
      (sample ~cancel:Heart.unbreakable ~dep:Heart.unbreakable)
      ~with_:dep
  )

let desensitize t =
  let f sample ~cancel ~dep =
    let cancel = Heart.combine2 cancel dep in
    sample ~cancel ~dep:Heart.unbreakable >>| function
    | None -> None
    | Some value_and_heart -> Some (value_and_heart, dep)
  in
  with_semantics t ~f

let exec t =
  sample (reduce t) ~cancel:Heart.unbreakable ~dep:Heart.unbreakable >>| function
  | Some result -> result
  | None ->
    (* This holds because [sample] only return a [None] if the [~cancel] or [~dep] it
       receives is broken. *)
    assert false

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
    Embed (fun ~cancel:_ ~dep ->
      let heart = Heart.watch var.glass in
      Deferred.return (Some (var.value, Heart.combine2 heart dep))
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

let cutoff ~equal ten =
  Embed (fun ~cancel:_ ~dep ->
    let my_weak_glass, my_heart = Weak_glass.create () in
    exec ten >>| fun (first_result,first_heart) ->
    let rec loop heart =
      Heart.or_broken heart (Weak_glass.unwatched my_weak_glass)
      >>> function
      | None ->
        exec ten >>> fun (replacement_result,heart) ->
        if (equal first_result replacement_result)
        then loop heart
        else Weak_glass.break my_weak_glass
      | Some () -> ()
    in
    loop first_heart;
    Some (first_result, Heart.combine2 my_heart dep)
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
module Let_syntax = struct
  let return = return
  let map    = map
  let bind   = bind
  let both   = both
  module Open_on_rhs  = struct let return = return end
  module Open_in_body = struct let return = return end
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
