
open Core.Std
open Async.Std

let mes fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt
let _ = mes

module T1 = Tenacious

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
end

module Heart = Tenacious.Heart
module Glass = Heart.Glass
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
  ) in
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
      improve Tenacious.exec tenacious
    ) >>> fun res -> (r := Some res)
  end;
  stabilize();
  match Option.value_exn (!r) with
  | Error e -> raise e
  | Ok (x,h) -> assert (not (Heart.is_broken h)); x

let run ?show t =
  let res = run t in
  begin
    match show with | None -> ()
    | Some sexp_of_t -> mes "run/show -> %s" (Sexp.to_string (sexp_of_t res));
  end;
  res

(* Tests start here... *)

TEST_UNIT = (
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
  assert (run tenacious = (1,2,3))
)

TEST_UNIT = (
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
  (*let show = <:sexp_of<int*int*int*int*int>> in*)
  assert (run (*~show*) tenacious = (1,3,3,1,1))
)

TEST_UNIT = (
  let tenacious =
    begin
      let ta,_ = make_counter() in
      Tenacious.all [ta;ta]
    end
  in
  let res = run tenacious in
  assert (Int.Set.equal (Int.Set.of_list res) (Int.Set.of_list [1;2]))
)

TEST_UNIT = (
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
      Tenacious.all [second;first]
    end
  in
  assert (run tenacious = [2;1])
)

TEST_UNIT = (
  (* [all] -- ensure it is indeed concurrent *)
  let var1 = Ivar.create () in
  let var2 = Ivar.create () in
  let cr r w =
    Tenacious.lift (fun () ->
      Ivar.fill w ();
      Ivar.read r >>| fun () ->
      ((), Heart.unbreakable))
  in
  assert (run (Tenacious.all [cr var1 var2; cr var2 var1]) = [();()])
)

let once f =
  let r = ref false in
  fun () -> if !r then () else (r := true; f ())

TEST_UNIT = (
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
  assert (run tenacious = (3,4,5))
)

TEST_UNIT = (
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
  assert (run tenacious = (2,2,2))
)
