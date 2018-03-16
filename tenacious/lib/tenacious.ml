open! Core
open Async
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
   right now, the [dep] are dependencies of the left hand side of surrounding binds.

   The thread represents who is asking for the computation, some in some sense it is the
   same kind of information as [cancel] and [dep], except that instead of being passed for
   the purpose of cancelling the computation, it is passed around for the purpose of
   finding cycles. Cycles can only be introduced by memoize nodes, so everywhere else the
   [thread] is just passed around (unlike [cancel] and [dep] which support cancel/restart
   at a granularity finer than whole computations between [memoize] nodes). *)
type 'a semantics =
  thread:Graph.Node.t -> cancel:Heart.t -> dep:Heart.t -> 'a comp

type 'a state =
| Wait of 'a result Deferred.t * Graph.Node.t
| Running of 'a semantics
| Ready of 'a * Heart.t

type 'a t =
  | Return : 'a -> 'a t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | All : 'a t list -> 'a list t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Memoize : 'a state ref * String.t Lazy.t * 'a t Lazy.t -> 'a t
  | Lift : (unit -> ('a * Heart.t) Deferred.t) -> 'a t
  | Embed : 'a semantics -> 'a t
type 'a tenacious = 'a t

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
    | Memoize (_, _name, lazy s) -> begin
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

let combine_result' ~with_:heart =
  function
  | None -> None
  | Some(v, certificate) -> Some(v, Heart.combine2 certificate heart)

let combine_result comp ~with_:heart =
  if Heart.is_unbreakable heart then comp
  else comp >>| combine_result' ~with_:heart

module Tenacious_throttle = struct
  (* About throttling: the user callbacks (in lift mostly) run slow operations like
     calling commands, or more precisely, pushing computations that will run commands in a
     throttle, as there are only so many commands we can run before running out of file
     descriptors for instance.
     While these commands are running, the tenacious computation that are not blocked on
     something keep executing. And this execution keeps pushing more things in the command
     throttle. This is not only not useful, but in fact all of this allocates things that
     will only be necessary much later, thus creating long lived allocations, which are
     bad for memory usage and performance.
     So this module is used to implement a form of pushback: we are going to run tenacious
     computations but only so many of them at a time. And in practice we pick a number of
     tenacious computations which is slightly higher than the number of external commands,
     so that we can actually fill the throttle of external commands, and have a few extra
     computations buffered up.
     We also use this to change the order in which computations are run. Async jobs are
     pushed in a queue, which means if we rely on the async queue, we do something like a
     breadth-first traversal of the dependency graph, which is also not good for memory.
     So we use a stack and favor depth-first traversal when possible. *)

  (* The maximum number of running async computations we can have, inclusive. No limit on
     the pending async computations, as in theory it could be the entire computation. *)
  let concurrency = ref 0

  (* The computations that could run, except we are limited by [!concurrency]. It's a
     stack so we try to traverse the dependency graph in a depth-first way, to limit
     memory usage. *)
  type stack_elt =
    | Elt : { t : 'a t
            ; thread : Graph.Node.t
            ; cancel : Heart.t
            ; dep : Heart.t
            ; k :  ('a result -> unit)
            } -> stack_elt
    (* Would write this:
       | K of ('a -> unit)
       but instead we use a flat sum which probably helps a bit with memory, and "pattern
       match" using [Obj.tag], so it's more like:
       | ('a -> unit) *)
  let waiting : stack_elt Stack.t = Stack.create ()

  (* How many computations are running. This value is in the interval [0, !concurrency].
     If it is < [!concurrency], then the waiting stack must be empty. *)
  let running = ref 0
  let max_running = ref 0

  let init ~concurrency:v =
    if !concurrency > 0 then failwith "init was already called";
    concurrency := max 1 v
  ;;

  let incr_running () =
    assert (!running < !concurrency);
    incr running;
    if !running > !max_running then max_running := !running

  let decr_running () =
    assert (!running > 0);
    decr running
end

let init = Tenacious_throttle.init

(* A tenacious computation that starts [n] tenacious computations must use this on [n - 1]
   of them, otherwise it leads to assertion failures (like too many or not enough running
   jobs). A non tenacious computation that starts a tenacious computation (like [exec])
   must also use this. *)
let rec sample_enqueue_k : type a. a t -> thread:Graph.Node.t -> cancel:Heart.t -> dep:Heart.t -> k:(a result -> unit) -> unit =
  fun t ~thread ~cancel ~dep ~k ->
    let open Tenacious_throttle in
    if !running < !concurrency
    then (incr_running ();
          sample t ~thread ~cancel ~dep >>> fun a ->
          decr_running ();
          k a;
          reschedule ())
    else Stack.push waiting (Elt { t; thread; cancel; dep; k })

and reschedule () =
  let open Tenacious_throttle in
  if !running < !concurrency
  && not (Stack.is_empty waiting)
  then begin
    let v = Stack.pop_exn waiting in
    if Obj.tag (Obj.repr v) <> 0
    then begin
      let (k : _ -> _) = (Obj.magic v) in
      incr_running ();
      k ()
    end else
      let Elt { t; thread; cancel; dep; k } = Obj.magic v in
      (incr_running ();
       sample t ~thread ~cancel ~dep >>> fun a ->
       decr_running ();
       k a;
       reschedule ())
  end

and sample_enqueue : type a. a t -> thread:Graph.Node.t -> cancel:Heart.t -> dep:Heart.t -> a result Deferred.t = fun t ~thread ~cancel ~dep ->
  let i = Ivar.create () in
  sample_enqueue_k t ~thread ~cancel ~dep ~k:(Ivar.fill i);
  Ivar.read i

(* [release_running] is useful when the current computation will become idle (ie wait for
   the results of other computations), and [retake_running_k] is used when these other
   computations finish and we want to resume the initial computations. Any use of
   [release_running] must be followed eventually by a [retake_running_k]. *)
and release_running ~reschedule:b =
  let open Tenacious_throttle in
  decr_running ();
  if b then reschedule ()

and retake_running_k k =
  let open Tenacious_throttle in
  if !running < !concurrency
  then (incr_running (); k ())
  else (Stack.push waiting (Obj.magic k))

and retake_running () =
  let i = Ivar.create () in
  retake_running_k (Ivar.fill i);
  Ivar.read i

and sample : type a . a t -> a semantics =
  fun t ~thread ~cancel ~dep ->
    match t with
    | Return x -> return x ~thread ~cancel ~dep
    | Bind (t, f) -> bind t ~f ~thread ~cancel ~dep
    | Map (t, f) -> map t f ~thread ~cancel ~dep
    | All ts -> all ts ~thread ~cancel ~dep
    | Memoize (st, name, lazy t) -> memoize st ~name t ~thread ~cancel ~dep
    | Lift f -> lift f ~thread ~cancel ~dep
    | Embed emb -> emb ~thread ~cancel ~dep

and bind : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b semantics =
  fun t1 ~f ~thread ~cancel ~dep ->
    sample t1 ~thread ~cancel ~dep >>= function
    | None -> Deferred.return None
    | Some (v1, certificate1) ->
      match reduce (f v1) with
      | Return v2 -> Deferred.return (Some (v2, certificate1))
      | t2 ->
        sample t2 ~thread ~cancel ~dep:certificate1 >>= function
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
            bind t1 ~f ~thread ~cancel ~dep
          end
        | Some _ as res -> Deferred.return res

and return : 'a . 'a -> 'a semantics =
  fun v ~thread:_ ~cancel:_ ~dep ->
    Deferred.return (Some (v, dep))

and map : 'a 'b. 'a t -> ('a -> 'b) -> 'b semantics =
  fun t1 f ~thread ~cancel ~dep ->
    sample t1 ~thread ~cancel ~dep >>| function
    | None -> None
    | Some (v1, certificate) -> Some (f v1, certificate)

and all : 'a . 'a t list -> 'a list semantics =
  fun ts ~thread ~cancel ~dep ->
    let cancel = Heart.combine2 cancel dep in
    let len = List.length ts in
    let cells = Array.create ~len None in
    let ivar = Ivar.create () in
    let finished = Ivar.read ivar in
    let collect_results_and_finish () =
      retake_running_k (fun () ->
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
      )
    in
    let count =
      (* the number of sub-tenacious sampled, but still running *)
      ref 0
    in
    release_running ~reschedule:false; (* we'll schedule some children instead *)
    List.iteri ts ~f:(fun i t ->
      match t with
      | Return x -> cells.(i) <- Some (x, Heart.unbreakable)
      | _ ->
        let rec loop () =
          incr count;
          sample_enqueue_k t ~thread ~cancel ~dep:Heart.unbreakable ~k:(fun res ->
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
                ))
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

   [shared_dep] is an optimization for the case where all the computations sampling a
   memoize are known to have a common Heart.t in their [~dep] argument. This common
   dependency can be pushed inside of [memoize], so the dep can be combined with the cancel
   heart and the result heart once per computation of memoize, instead of once per sampling
   of [memoize]. *)
and memoize_with_shared_dep
  : 'a . 'a state ref -> name:(String.t Lazy.t) -> shared_dep:Heart.t -> 'a t -> 'a semantics =
  fun state ~name ~shared_dep t ->
    let start ~last_result ~last_node =
      let stop_glass =
        (* This is filled to cancel the [shared_computation] *)
        Glass.create ()
      in
      let stop_heart = Heart.watch stop_glass in
      let shared_computation_node =
        Graph.Node.create (lazy ("memoize: " ^ Lazy.force name))
      in
      let shared_computation =
        (* wait for the previous (cancelled) computation to finish *)
        Graph.edge_until shared_computation_node ~blocked_on:last_node last_result
        >>= fun res ->
        let last_still_valid =
          match res with
          | None -> false
          | Some (_,cert) -> not (Heart.is_broken cert)
        in
        (* if the last computation returns a result despite having been cancelled, we can
           use that result if the certificate is still valid, avoiding a new sampling *)
        if last_still_valid
        then Deferred.return res
        else
          sample_enqueue t
            ~thread:shared_computation_node ~cancel:stop_heart ~dep:shared_dep
      in
      let count = ref 0 in
      let join ~thread ~cancel:my_cancel ~dep:my_dep =
        (* join a new client, with its own [my_cancel] to the running [shared_computation] *)
        let my_cancel = Heart.combine2 my_cancel my_dep in
        release_running ~reschedule:true;
        incr count;
        Graph.edge_until thread ~blocked_on:shared_computation_node
          (Heart.or_broken my_cancel shared_computation)
        >>= fun shared_result ->
        match shared_result with
        | Some res ->
          retake_running ()
          >>| fun () ->
          combine_result' res ~with_:my_dep
        | None -> (* this client has cancelled *)
          assert (!count >= 1);
          decr count;
          if (!count = 0) then (
            state := Wait (shared_computation, shared_computation_node);
            Glass.break stop_glass
          );
          retake_running ()
          >>| fun () ->
          None
      in
      begin
        shared_computation  >>> function
        | None -> () (* was stopped *)
        | Some (x,certificate) ->
          (* wasn't stopped, of stop requested *)
          if Heart.is_broken stop_heart || Heart.is_broken shared_dep then (
          (* Cancel of [shared_computation] requested, but it returned a result anyway.
             We cannot prevent this race. *)
          ) else (
            state := Ready(x, certificate)
          )
      end;
      state := Running join;
      join
    in
    fun ~thread ~cancel ~dep ->
      (* If we are called with broken [cancel] or [dep] and the state is [Wait], we would
         switch from [Wait] to [Running] and almost immediately back to [Wait], where the
         graph node in the new [Wait] would be blocked on the old one. Doing do repeatedly
         means creating an ever longer chain, and ultimately make the cycle check overflow
         the stack on these chains. This check avoids this.
         One may wonder why [cancel] and [dep] would be broken, and well, this module does
         not abort computations as quickly as possible, and in fact doing so changes the
         behavior of jenga, because it doesn't report mtimes check errors anymore (because
         of how jenga self-triggers when detecting on mtimes check errors). Instead it
         retries to run the command. *)
      if Heart.is_broken cancel || Heart.is_broken dep
      then Deferred.return None
      else
        match !state with
        | Wait (result, node) ->
          let join = start ~last_result:result ~last_node:node in
          join ~thread ~cancel ~dep
        | Running join ->
          join ~thread ~cancel ~dep
        | Ready(x, certificate) ->
          if Heart.is_broken certificate
          then begin
            let join = start
              ~last_result:(Deferred.return None)
              ~last_node:(Graph.Node.create (lazy "memoize_last_broken"))
            in
            join ~thread ~cancel ~dep
          end
          else Deferred.return (Some (x, Heart.combine2 certificate dep))

and memoize : 'a . 'a state ref -> name:(String.t Lazy.t) -> 'a t -> 'a semantics =
  fun r ~name -> memoize_with_shared_dep r ~name ~shared_dep:Heart.unbreakable

and lift : type a . (unit -> (a * Heart.t) Deferred.t) -> a semantics =
  fun f ~thread:_ ~cancel ~dep ->
    (* Make initial check of [cancel]; afterwards [cancel] is ignored.
       Waits until [f ()] is determined before returning *)
    if Heart.is_broken cancel || Heart.is_broken dep
    then Deferred.return None
    else f () >>| fun (v, heart) -> Some (v, Heart.combine2 heart dep)

let return x = Return x

let bind t ~f = Bind(t, f)

let map t ~f = Map(t, f)

let all = function
  | [] -> Return []
  | [t] -> Map(t, List.return)
  | ts -> All ts

let lift f = Lift f

let create_memoize_state () =
  ref (Wait (Deferred.return None, Graph.Node.create (lazy "memoize wait")))
;;

let memoize ~name t =
  Memoize (create_memoize_state (), name, lazy (reduce t))

let embed f =
  Embed (fun ~thread:_ ~cancel ~dep ->
    let cancel = Heart.combine2 cancel dep in
    combine_result (f ~cancel) ~with_:dep
  )

let all_unit ts =
  map (all ts) ~f:(fun (_:unit list) -> ())

let all_ignore = all_unit

let race a b =
  Embed (fun ~thread ~cancel ~dep ->
    let cancel_a = Heart.Glass.create () in
    let cancel_b = Heart.Glass.create () in
    let ad =
      match reduce a with
      | Return v -> Deferred.return (Some (v, dep))
      | a ->
        sample ~thread ~cancel:(Heart.combine2 cancel (Heart.watch cancel_a)) ~dep a
    in
    let bd =
      match reduce b with
      | Return v -> Deferred.return (Some (v, dep))
      | b ->
        sample_enqueue ~thread ~cancel:(Heart.combine2 cancel (Heart.watch cancel_b)) ~dep b
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
  let go ~thread ~cancel ~dep =
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
                  ~thread
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
  fun a b ~f -> Embed (fun ~thread ~cancel ~dep -> go ~thread ~cancel ~dep a b ~f)

let with_semantics inner_t ~f =
  Embed (fun ~thread ~cancel ~dep ->
    f (fun ~thread ~cancel ~dep -> sample (reduce inner_t) ~thread ~cancel ~dep)
      ~thread ~cancel ~dep)

let bracket t ~running ~finished ~cancelled =
  let count = ref 0 in
  Embed (fun ~thread ~cancel ~dep ->
    running !count;
    incr count;
    sample (reduce t) ~thread ~cancel ~dep >>| fun res ->
    begin match res with
    | None -> cancelled ()
    | Some (v, _) -> finished v
    end;
    res
  )

let uncancellable t =
  with_semantics t ~f:(fun sample ~thread ~cancel:_ ~dep ->
    combine_result
      (sample ~thread ~cancel:Heart.unbreakable ~dep:Heart.unbreakable)
      ~with_:dep
  )

let desensitize t =
  let f sample ~thread ~cancel ~dep =
    let cancel = Heart.combine2 cancel dep in
    sample ~thread ~cancel ~dep:Heart.unbreakable >>| function
    | None -> None
    | Some value_and_heart -> Some (value_and_heart, dep)
  in
  with_semantics t ~f

let exec t ~name =
  let node = Graph.Node.create (lazy ("exec: " ^ Lazy.force name)) in
  Graph.root_until ~node
    (sample_enqueue (reduce t) ~thread:node ~cancel:Heart.unbreakable ~dep:Heart.unbreakable
     >>| function
     | Some result -> result
     | None ->
       (* This holds because [sample] only return a [None] if the [~cancel] or [~dep] it
          receives is broken. *)
       assert false)

module Stream = struct

  type 'a t = 'a stream_elt Lazy.t
  and 'a stream_elt = Stream_elt of 'a * 'a t tenacious

  let unfold init next ~name =
    let rec loop count init heart1 =
      lazy (
        let a, init_dep = next init in
        let stream_tail =
          Embed
            (memoize_with_shared_dep
               (create_memoize_state ())
               ~name:(lazy (force name ^ " #" ^ Int.to_string count))
               ~shared_dep:heart1
               (Embed (fun ~thread ~cancel ~dep ->
                  sample (reduce init_dep) ~thread ~cancel ~dep >>| function
                  | None -> None
                  | Some (init, heart2) -> (* heart2 includes heart1 *)
                    Some (loop (count + 1) init heart2, heart2))))
        in
        Stream_elt (a, stream_tail)
      )
    in
    loop 0 init Heart.unbreakable

  type ('a, 'res) query =
    | Return of 'res
    | Continue of ('a -> ('a, 'res) query)

  let query t q =
    Embed (fun ~thread ~cancel ~dep ->
      let cancel = Heart.combine2 cancel dep in
      let rec restart () = loop (return t) q Heart.unbreakable
      and loop (t : _ t tenacious) q heart1 =
        match q with
        | Return res -> Deferred.return (Some (res, Heart.combine2 dep heart1))
        | Continue k ->
          (* We do not pass [~dep] here because we don't want to combine [dep] with either
             cancel or with the resulting heart at every step in the stream. *)
          sample t ~thread ~cancel ~dep:Heart.unbreakable
          >>= function
          | Some (lazy (Stream_elt (a, t)), heart2) -> loop t (k a) heart2
          | None ->
            if Heart.is_broken cancel
            then Deferred.return None
            else
              (* At this point, we don't have enough information to tell at which point of
                 the stream hearts became invalid, not because of some fundamental issue
                 but simply because we don't keep enough information to figure it out when
                 we recurse. *)
              restart ()
      in
      restart ())
end

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
    Embed (fun ~thread:_ ~cancel:_ ~dep ->
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
  Embed (fun ~thread ~cancel:_ ~dep ->
    let my_weak_glass, my_heart = Weak_glass.create () in
    sample ~thread ~cancel:Heart.unbreakable ~dep:Heart.unbreakable (reduce ten) >>| function
    | None -> None
    | Some (first_result,first_heart) ->
      let rec loop heart =
        Heart.or_broken heart (Weak_glass.unwatched my_weak_glass)
        >>> function
        | None ->
            exec ~name:(lazy "cutoff") ten
            >>> fun (replacement_result,heart) ->
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
    let a = memoize ~name:(lazy "race-error-a") a in
    let b = memoize ~name:(lazy "race-error-b") b in
    bind
      (race (map ~f:(fun x -> `a x) a) (map ~f:(fun x -> `b x) b))
      ~f:(function
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

let join x = bind x ~f:(fun x -> x)
let ignore_m = map ~f:ignore
let (>>|) x f = map x ~f
let (>>=) t f = bind t ~f
module Monad_infix = struct
  let (>>|) = (>>|)
  let (>>=) = (>>=)
end
module Let_syntax = struct
  let return = return
  include Monad_infix
  module Let_syntax = struct
    let return = return
    let map    = map
    let bind   = bind
    let both   = both
    module Open_on_rhs  = struct end
  end
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

    let bind t ~f =
      Tenacious.bind t ~f:(function
        | Ok a -> f a
        | Error _ as error -> Tenacious.return error)
    ;;

    let map t ~f = Tenacious.map t ~f:(fun r -> Result.map r ~f)
    let map = `Custom map
  end)

  let fail e = Tenacious.return (Error e)
  let map_error t ~f = Tenacious.map t ~f:(fun r -> Result.map_error r ~f)

end
