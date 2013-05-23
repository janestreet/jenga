
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let (=) = Int.(=)
let (>) = Int.(>)

module T = struct

  type 'a res = Res_none | Res_some of 'a * Heart.t | Res_return of 'a

  type 'a running = {
    mutable num_waiting : int;
    glass : Heart.Glass.t;
    def : 'a res Deferred.t
  }

  let incr_waiting r = r.num_waiting <- r.num_waiting + 1
  let decr_waiting r = r.num_waiting <- r.num_waiting - 1
  let zero_waiting r = r.num_waiting = 0
  let some_waiting r = r.num_waiting > 0

  type 'a state = Running of 'a running | Dormant of 'a res

  type 'a runner = {
    run : (cancel:Heart.t ->  'a res Deferred.t);
    mutable state : 'a state
  }

  let string_of_res = function
    | Res_none -> "N"
    | Res_some _ -> "S"
    | Res_return _ -> "R"

  let string_of_state = function
    | Running _ -> "Running"
    | Dormant res -> sprintf "Dormant/%s" (string_of_res res)

  let set_state runner state =
    let ok =
      match runner.state, state with
      | Dormant _, Running _
      | Running _, Dormant _
        -> true
      | Running _, Running _
      | Dormant _, Dormant _
        -> false
    in
    if ok then runner.state <- state else
      (* think this is harmless, dont display it to screen *)
      Message.trace "tenacious: invalid transition, %s -> %s"
        (string_of_state runner.state) (string_of_state state)

  let start runner : Heart.Glass.t * 'a res Deferred.t =
    let glass = Heart.Glass.create ~desc:"tenacious/request" in
    let heart = Heart.of_glass glass in
    let rec again() =
      runner.run ~cancel:heart >>= fun res ->
      match res with
      | Res_return _ -> Deferred.return res
      | Res_none -> Deferred.return Res_none (* was cancelled *)
      | Res_some (_,heart) ->
        if Heart.is_broken heart then (
          again()
        ) else ( (* we have a valid result *)
          set_state runner (Dormant res);
          Deferred.return res
        )
    in
    let def = again() in
    glass,def

  let request runner : 'a res Deferred.t =
    match runner.state with
    | Running r -> incr_waiting r; r.def
    | Dormant res ->
      let good =
        match res with
        | Res_return _ -> true
        | Res_none -> false
        | Res_some (_,heart) ->
          if Heart.is_broken heart
          then false
          else true
      in
      if good
      then Deferred.return res
      else
        let glass,def = start runner in
        let running = { num_waiting = 1; glass; def; } in
        set_state runner (Running running);
        def

  let unrequest runner : unit =
    match runner.state with
    | Dormant _ -> ()
    | Running r ->
      assert (some_waiting r);
      decr_waiting r;
      if (zero_waiting r) then (
        Heart.Glass.break r.glass;
        set_state runner (Dormant Res_none)
      )


  type 'a t =
  | Run of 'a runner
  (*| Return of 'a*)

  let get ~cancel t : 'a res Deferred.t =
    match t with
    (*| Return x -> Deferred.return (Res_return x)*)
    | Run runner ->
      (*let already_broken = Heart.is_broken cancel in
      if already_broken then (
      );*)
      choose [
        choice (Heart.when_broken cancel) (fun () ->
          unrequest runner;
          Res_none;
        );
        choice (request runner) (fun x -> x);
      ]

  let exec_cancelable t ~cancel : ('a * Heart.t) option Deferred.t =
    get ~cancel t >>= function
    | Res_none -> Deferred.return None
    | Res_some (x,heart) -> Deferred.return (Some (x,heart))
    | Res_return x -> Deferred.return (Some (x,Heart.unbreakable))

  let create ~run =
    Run {
      run;
      state = Dormant Res_none;
    }

  (*let return x = Return x*)
  let return x =
    let run ~cancel:_ =
      Deferred.return (Res_return x)
    in
    create ~run

  let lift_cancelable f =
    let computed_but_unwanted = ref None in
    let run ~cancel =
      let res = !computed_but_unwanted in
      match res with
      | Some (x,heart) ->
        computed_but_unwanted := None;
        Deferred.return (Res_some (x,heart))
      | None ->
        f ~cancel >>= function
        | None -> Deferred.return Res_none
        | Some res ->
          if Heart.is_broken cancel then (
            computed_but_unwanted := Some res;
            Deferred.return Res_none
          ) else (
            let x,heart = res in
            Deferred.return (Res_some (x,heart))
          )
    in
    create ~run

  let bind t f =
    let run ~cancel =
      let rec stage1() =
        get t ~cancel >>= function
        | Res_none -> Deferred.return Res_none
        | Res_return x -> (
          get (f x) ~cancel >>= function
          | Res_none -> Deferred.return Res_none
          | Res_return y -> Deferred.return (Res_some (y, Heart.unbreakable))
          | Res_some (y,heart2) -> Deferred.return (Res_some (y, heart2))
        )
        | Res_some (x,heart1) ->
          get (f x) ~cancel:(Heart.combine2 heart1 cancel) >>= function
          | Res_none ->
            if Heart.is_broken cancel then (
              Deferred.return Res_none
            ) else (
              assert (Heart.is_broken heart1);
              stage1()
            )
          | Res_return y ->
            Deferred.return (Res_some (y, heart1))
          | Res_some (y,heart2) ->
            Deferred.return (Res_some (y, Heart.combine2 heart1 heart2))
      in
      stage1()
    in
    create ~run

  let all ts =
    let count_todo = ref (List.length ts) in
    let all_done = Ivar.create () in
    let run ~cancel =
      let for_each t =
        let rec again () =
          get ~cancel t >>= fun res ->
          match res with
          | Res_none -> Deferred.return Res_none

          | Res_return _ ->
            decr count_todo;
            if (!count_todo = 0)
            then (Ivar.fill all_done (); Deferred.return res)
            else (
              Ivar.read all_done >>= fun () ->
              Deferred.return res
            )

          | Res_some (_,heart) ->
            decr count_todo;
            if (!count_todo = 0 && not (Heart.is_broken heart))
            then (Ivar.fill all_done (); Deferred.return res)
            else
              choose [
                choice (Heart.when_broken heart) (fun () -> `again);
                choice (Ivar.read all_done) (fun () -> `all_done);
              ] >>= function
              | `again -> incr count_todo; again()
              | `all_done -> Deferred.return res
        in
        again()
      in
      let rec collect acc acc_hearts = function
        | Res_none :: _ -> Res_none
        | Res_return x :: rest -> collect (x::acc) acc_hearts rest
        | Res_some (x,heart) :: rest -> collect (x::acc) (heart::acc_hearts) rest
        | [] -> Res_some (List.rev acc, Heart.combine acc_hearts)
      in
      Deferred.List.map ~how:`Parallel ts ~f:for_each >>= fun xs ->
      Deferred.return (collect [] [] xs)
    in
    create ~run

end

include T


(* non primitive ops... *)

let before_redo t ~f =
  let first_time = ref true in
  lift_cancelable (fun ~cancel ->
    (if not !first_time then f() else first_time := false);
    exec_cancelable t ~cancel
  )

let ( *>>= ) = bind

let all = function
  | [] -> return []
  | [x] -> x *>>= fun v -> return [v]
  | xs -> all xs

let all_unit ts =
  all ts *>>= fun (_:unit list) ->
  return ()


let exec t =
  exec_cancelable t ~cancel:Heart.unbreakable >>= function
  | None -> assert false (* impossible! *)
  | Some x -> Deferred.return x

let lift f =
  lift_cancelable (fun ~cancel:_ ->
    f () >>= fun x ->
    Deferred.return (Some x)
  )


let prevent_overlap
    : (
      table : ('key, 'a t) Hashtbl.t ->
      keys: 'key list ->
      ?notify_wait: ('key -> unit) ->
      ?notify_add: ('key -> unit) ->
      ?notify_rem: ('key -> unit) ->
      'a t ->
      'a t
    ) =
  (* Prevent overlapping tenacious execution *)
  fun ~table ~keys
    ?(notify_wait = fun _ -> ())
    ?(notify_add = fun _ -> ())
    ?(notify_rem = fun _ -> ())
    tenacious ->
      lift (fun () ->
        let run() =
          List.iter keys ~f:(fun key ->
            notify_add key;
            Hashtbl.add_exn table ~key ~data:tenacious
          );
          exec tenacious >>= fun res ->
          List.iter keys ~f:(fun key ->
            notify_rem key;
            Hashtbl.remove table key
          );
          Deferred.return res
        in
        let rec maybe_wait_then_run () =
          match
            List.filter_map keys ~f:(fun key ->
              match (Hashtbl.find table key) with
              | None -> None
              | Some t_running ->
                Some (
                  notify_wait key;
                  exec t_running >>= fun _ ->
                  Deferred.return ()
                )
            )
          with
          | [] -> run ()
          | xs ->
            let wait = Deferred.all_unit xs in
            wait >>= maybe_wait_then_run
        in
        maybe_wait_then_run()
      )
