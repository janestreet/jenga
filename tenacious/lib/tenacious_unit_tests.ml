open Core.Std
open Async.Std

let mes fmt = ksprintf (fun s -> Core.Std.Printf.eprintf "%s\n%!" s) fmt
let _ = mes

module Tenacious_tests(T1: Tenacious_intf.S) = struct
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

    let reify = reify ~name:(lazy "test-reify")
  end

  module Heart = Tenacious.Heart
  module Glass = Heart.Glass
  module Var = Tenacious.Var
  let ( *>>= ) = Tenacious.bind
  let ( *>>| ) t f = Tenacious.map t ~f
  let return = Tenacious.return

  let stabilize = Async_kernel.Scheduler.run_cycles_until_no_jobs_remain

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

  let run tenacious =
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
    | Ok (x,h) -> assert (not (Heart.is_broken h)); x

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
    (* mod of above test; introduce use of reification *)
    let tenacious =
      begin
        let a,break_a = make_counter() in
        let a = Tenacious.reify a in
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
    let bind = Tenacious.bind lhs (fun _ -> rhs) in
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
       - a reify that's already computed,
       - a desensitize *)
    let some_tenacious, _ = make_counter () in
    check_dependency_when_on_rhs_of_bind
      (let a = Tenacious.reify some_tenacious in
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
end

module Test_reference = Tenacious_tests (struct
  include Tenacious
  include Tenacious.For_tests
end)
module Test_production = Tenacious_tests (Tenacious)
