open Core
open Async

let mes fmt = ksprintf (fun s -> Core.Printf.eprintf "%s\n%!" s) fmt
let _ = mes

module Tenacious_tests(T1: Tenacious_intf.S) : sig end = struct
  module Tenacious = struct
    include T1

    let sample_count =
      (* Count number of samples of all lifted computations, to allow simple detection of
         looping tests executed by [run] *)
      ref 0

    let exec tenacious =
      (* not expected to be used in re-entrant way *)
      sample_count := 0;
      T1.exec tenacious

    let lift f =
      T1.lift (fun () ->
        incr sample_count;
        if (!sample_count > 1000) then failwith "lift: too many steps";
        f ()
      )

    let memoize = memoize ~name:(lazy "test-memoize")
  end

  module Heart = Tenacious.Heart
  module Glass = Heart.Glass
  module Var = Tenacious.Var
  let ( *>>= ) t f = Tenacious.bind t ~f
  let ( *>>| ) t f = Tenacious.map t ~f
  let return = Tenacious.return

  let stabilize = Async_kernel_scheduler.run_cycles_until_no_jobs_remain

  let push x l = (l := x :: !l)

  let make_counter () =
    (* Construct a leaf tenacious computation, using [lift], which returns an int-value;
       incremented each time it is sampled, together with a [break] function to invalid
       hearts form all previous samplings. *)
    let glass_list = ref [] in
    let n = ref 0 in
    let t = Tenacious.lift (fun () ->
      let glass = Glass.create () in
      push glass glass_list;
      incr n;
      Deferred.return (!n,Heart.watch glass)
    )
    in
    let break () =
      List.iter !glass_list ~f:Glass.break;
      glass_list := [];
    in
    t,break

  let improve exec ten =
    let rec loop count =
      if count>1000 then failwith "improve: too many steps" else
        let d = exec ten in
        d >>= fun (_,h) ->
        if Heart.is_broken h then loop (count+1) else d
    in
    loop 0

  let run' tenacious =
    (* Run a tenacious computation to get a result *)
    let r = ref None in
    begin
      Monitor.try_with (fun () ->
        improve (Tenacious.exec ~name:(lazy "run")) tenacious
      ) >>> fun res -> (r := Some res)
    end;
    stabilize();
    match Option.value_exn (!r) with
    | Error e -> raise e
    | Ok (x, h) -> assert (not (Heart.is_broken h)); x, h
  let run tenacious = fst (run' tenacious)

  (* Tests start here... *)

  let%test_unit _ = (
    (* check independant samplings from multiple use of same tenacious *)
    let tenacious =
      begin
        let a,_ = make_counter() in
        a *>>= fun a1 ->
        a *>>= fun a2 ->
        a *>>= fun a3 ->
        return (a1,a2,a3)
      end
    in
    [%test_result: int * int * int] (run tenacious) ~expect:(1,2,3)
  )

  let%test_unit _ = (
    (* check:
       - invalidation causes rerun of invalidated computation and dependants
       - cancellation is acted upon immediately *)
    let tenacious =
      begin
        let a,_ = make_counter() in
        let b,b_break = make_counter() in
        let c,_ = make_counter() in
        let d,_ = make_counter() in
        let e,_ = make_counter() in
        a *>>= fun a ->
        b *>>= fun b ->
        c *>>= fun c ->
        if c<3 then b_break();
        d *>>= fun d ->
        e *>>= fun e ->
        return (a,b,c,d,e)
      end
    in
    [%test_result: int * int * int * int * int] (run tenacious) ~expect:(1,3,3,1,1)
  )

  let%test_unit _ = (
    let tenacious =
      begin
        let ta,_ = make_counter() in
        Tenacious.all [ta;ta]
      end
    in
    let res = run tenacious in
    [%test_result: Int.Set.t] (Int.Set.of_list res) ~expect:(Int.Set.of_list [1;2])
  )

  let%test_unit _ = (
    (* [all] -- force specific ordering; right(first), left(second) *)
    let tenacious =
      begin
        let ta,_ = make_counter() in
        let proceed = ref false in
        let wait,again = make_counter() in
        let first =
          ta *>>| fun x ->
          proceed := true;
          x
        in
        let second =
          wait *>>= fun _ ->
          if not !proceed then again();
          ta
        in
        Tenacious.all [second; first]
      end
    in
    [%test_result: int list] (run tenacious) ~expect:[2; 1]
  )

  let%test_unit _ = (
    (* [all] -- ensure it is indeed concurrent *)
    let var1 = Ivar.create () in
    let var2 = Ivar.create () in
    let cr r w =
      Tenacious.lift (fun () ->
        Ivar.fill w ();
        Ivar.read r >>| fun () ->
        ((), Heart.unbreakable))
    in
    [%test_result: unit list]
      (run (Tenacious.all [cr var1 var2; cr var2 var1]))
      ~expect:[();()]
  )

  let%test_unit "[all []]" = (
    [%test_result: unit list] (run (Tenacious.all [])) ~expect:[]
  )

  let once f =
    let r = ref false in
    fun () -> if !r then () else (r := true; f ())

  let%test_unit _ = (
    (* check independant samplings from multiple use of same tenacious; with break *)
    let tenacious =
      begin
        let a,break_a = make_counter() in
        let break = once break_a in
        a *>>= fun a1 -> (* 1   3 *)
        a *>>= fun a2 -> (*   2   4 *)
        break();
        a *>>= fun a3 -> (*         5 *)
        return (a1,a2,a3)
      end
    in
    [%test_result: int * int * int] (run tenacious) ~expect:(3,4,5)
  )

  let%test_unit _ = (
    (* mod of above test; introduce use of memoization *)
    let tenacious =
      begin
        let a,break_a = make_counter() in
        let a = Tenacious.memoize a in
        let break = once break_a in
        a *>>= fun a1 -> (* 1   2 *)
        a *>>= fun a2 -> (*   1   2 *)
        break();
        a *>>= fun a3 -> (*         2 *)
        return (a1,a2,a3)
      end
    in
    [%test_result: int * int * int] (run tenacious) ~expect:(2,2,2)
  )

  let check_dependency_when_on_rhs_of_bind rhs =
    let lhs, break_lhs = make_counter () in
    let bind = Tenacious.bind lhs ~f:(fun _ -> rhs) in
    let _v, heart =
      Thread_safe.block_on_async_exn (fun () ->
        Tenacious.exec ~name:(lazy "check") bind)
    in
    assert (not (Heart.is_broken heart));
    break_lhs ();
    assert (Heart.is_broken heart);
  ;;

  let%test_unit _ =
    (* checking that various functions don't drop the dependencies on
       the lhs of the surrounding bind (these cases used to broken):
       - a memoize that's already computed,
       - a desensitize *)
    let some_tenacious, _ = make_counter () in
    check_dependency_when_on_rhs_of_bind
      (let a = Tenacious.memoize some_tenacious in
       [%test_result: int] (run a) ~expect:1;
       a);
    check_dependency_when_on_rhs_of_bind
      (Tenacious.desensitize some_tenacious);
  ;;

  let%test_unit _ = (
    let ten1 = return (Ok 1) in
    let ten2 = return (Ok 2) in
    let ten3 = return (Ok 3) in
    [%test_result: (int list, string) Result.t]
      (run (Tenacious.race_errors [ten1; ten2; ten3]))
      ~expect:(Ok [1;2;3])
  )

  let%test_unit _ = (
    let ten1 = return (Ok 1) in
    let ten2 = return (Ok 2) in
    [%test_result: (int * int, string) Result.t]
      (run (Tenacious.race_error ~f:(fun x y -> (x, y)) ten1 ten2))
      ~expect:(Ok (1,2))
  )

  let%test_unit _ = (
    let ten1 = return (Error "err") in
    let ten2 = return (Ok 2) in
    [%test_result: (int * int, string) Result.t]
      (run (Tenacious.race_error ~f:(fun x y -> (x, y)) ten1 ten2))
      ~expect:(Error "err")
  )

  let%test_unit _ = (
    let ten1 = return (Ok 1) in
    let ten2 = return (Error "err") in
    [%test_result: (int * int, string) Result.t]
      (run (Tenacious.race_error ~f:(fun x y -> (x, y)) ten1 ten2))
      ~expect:(Error "err")
  )

  (* a Tenacious that never completes.
     Hearts are there so we can test that the computation has indeed been cancelled
  *)
  let never () =
    let hearts = ref [] in
    Tenacious.embed (fun ~cancel ->
      hearts := cancel :: !hearts;
      Deferred.never ()), hearts

  let test_race f =
    let ten1, cancels = never () in
    let ten2 = return (Error "err") in
    [%test_result: (int * int, string) Result.t]
      (run (f ten1 ten2 ~f:(fun x y -> (x, y))))
      ~expect:(Error "err");
    List.iter !cancels ~f:(fun h -> assert (Heart.is_broken h))

  let%test_unit _ = (
    test_race Tenacious.race_error;
  )

  let%test_unit _ = (
    test_race (Fn.flip Tenacious.race_error);
  )

  let%test_unit _ = (
    [%test_result: (int list, string) Result.t]
      (run (Tenacious.race_errors []))
      ~expect:(Ok []);
  )

  let%test_unit "Var behaves like normal ref" =
    let var = Var.create 1 in
    [%test_result: int] ~expect:1 (Var.get var);
    Var.set var 2;
    [%test_result: int] ~expect:2 (Var.get var);
    Var.replace var ~f:(fun x -> x + 1);
    [%test_result: int] ~expect:3 (Var.get var)

  let%test_unit "Var can be watched" = (
    let var1 = Var.create 10 in
    let var2 = Var.create 1 in
    let sum =
      Tenacious.map2 ~f:(+)
        (Var.watch var1) (Var.watch var2)
    in
    [%test_result: int] (run sum) ~expect:11;
    Var.set var1 20;
    [%test_result: int] (run sum) ~expect:21;
    Var.set var2 2;
    [%test_result: int] (run sum) ~expect:22;
  )

  let make_events (test_result : ?here:_ -> ?message:_ -> ?equal:_ -> _) =
    let q = Queue.create () in
    (fun e -> Queue.enqueue q e),
    (fun expect ->
       test_result ~expect (Queue.to_list q);
       Queue.clear q)
  ;;

  let%test_unit "stream compute/recompute correctly" =
    let add, check =
      make_events [%test_result: [ `Computing_stream_elt of int
                                 | `Query of int * int] list]
    in
    let glasses = Array.init 4 ~f:(fun _ -> Heart.Glass.create ()) in
    let stream =
      Tenacious.Stream.unfold ~name:(lazy "")
        0
        (fun count ->
          add (`Computing_stream_elt count);
          count, Tenacious.embed (fun ~cancel:_ ->
            (* Breaking glasses.(X) makes any computation using [`Computing_stream_elt X]
               stale. *)
            let index = count + 1 in
            if Heart.Glass.is_broken glasses.(index)
            then glasses.(index) <- Heart.Glass.create ();
            Deferred.return (Some (count + 1, Heart.watch glasses.(index)))))

    in
    let queries =
      Array.init 4 ~f:(fun stop_at ->
        let rec loop i : _ Tenacious.Stream.query =
          if i = stop_at then Return i
          else Continue (fun value ->
            assert (value = i);
            add (`Query (stop_at, i));
            loop (i + 1))
        in
        let query = loop 0 in
        Tenacious.Stream.query stream query
      )
    in
    let run index =
      let value, heart = run' queries.(index) in
      [%test_result:int] value ~expect:index;
      heart
    in
    (* First we check that stream elements are:
       - properly generated (the state is not dropped or something)
       - only on demand
       - only once *)
    check [];
    let heart0 = run 0 in
    check [];
    let heart1 = run 1 in
    check [ `Computing_stream_elt 0; `Query (1, 0) ];
    let heart3 = run 3 in
    check [ `Query (3, 0)
          ; `Computing_stream_elt 1; `Query (3, 1)
          ; `Computing_stream_elt 2; `Query (3, 2) ];
    let heart2 = run 2 in
    check [ `Query (2, 0); `Query (2, 1) ];
    (* And now if some suffix of the stream becomes invalid, only the affected queries
       need to be rerun. And rerunning these queries only forces the invalid part of the
       scheme to be recomputed. *)
    Heart.Glass.break glasses.(1);
    assert (not (Heart.is_broken heart0));
    assert (not (Heart.is_broken heart1));
    assert (Heart.is_broken heart2); (* queries.(2) is the first computation that uses
                                        `Computing_stream_elt 1 *)
    assert (Heart.is_broken heart3);
    let _heart3 = run 3 in
    check [ `Query (3, 0); `Computing_stream_elt 1
          ; `Query (3, 1); `Computing_stream_elt 2; `Query (3, 2) ];
    (* Finally, queries share the same hearts, if they force the same prefix of the
       same stream. *)
    assert (phys_equal (run 3) (run 3));
    check [ `Query (3, 0); `Query (3, 1); `Query (3, 2)
          ; `Query (3, 0); `Query (3, 1); `Query (3, 2) ];
  ;;

  let%test_unit "cancellation during stream execution" =
    let add, check =
      make_events [%test_result: [ `Computing_stream_elt of int
                                 | `Query of int ] list]
    in
    let glass = Heart.Glass.create () in
    let stream =
      (* We create a stream where the computation of the first elements breaks during the
         computation ofthe second element (only once). *)
      Tenacious.Stream.unfold ~name:(lazy "")
        0 (fun count ->
          count, Tenacious.embed (fun ~cancel ->
                add (`Computing_stream_elt count);
                assert (not (Heart.is_broken cancel));
                if count = 0 || Heart.Glass.is_broken glass
                then Deferred.return (Some (count + 1, Heart.unbreakable))
                else begin
                  if count = 1
                  then Deferred.return (Some (count + 1, Heart.watch glass))
                  else begin
                    Heart.Glass.break glass;
                    assert (Heart.is_broken cancel);
                    Deferred.return None
                  end
                end))
    in
    let rec query i : _ Tenacious.Stream.query =
      if i = 4 then Return ()
      else Continue (fun a -> add (`Query a); assert (a = i); query (i + 1))
    in
    run (Tenacious.Stream.query stream (query 0));
    check [ `Query 0; `Computing_stream_elt 0; `Query 1; `Computing_stream_elt 1
          ; `Query 2; `Computing_stream_elt 2
          (*  [`Computing_stream_elt 2] invalidates [`Computing_stream_elt 1], so we
              restart the query. *)
          ; `Query 0; `Query 1; `Computing_stream_elt 1
          ; `Query 2; `Computing_stream_elt 2
          ; `Query 3
          ];
  ;;
end

let%test_module _ = (module struct
  let () = Tenacious.init ~concurrency:5
  module Test_reference = Tenacious_tests (struct
    include Tenacious
    include Tenacious.For_tests
  end)
  module Test_production = Tenacious_tests (Tenacious)
end)
