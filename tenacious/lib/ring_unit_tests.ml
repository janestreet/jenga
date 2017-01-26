open! Core
open! Int.Replace_polymorphic_compare

module Slow_ring = struct
  type 'a t = (Uuid.t * 'a) list ref
  type 'a elem = 'a t * Uuid.t

  let create () = ref []
  let add l x = let uid = Uuid.create () in (l := !l @ [uid, x]); l, uid
  let detach ((l, uid) : _ elem) = l := List.filter ~f:(fun (u,_) -> not (Uuid.(=) u uid)) !l
  let iter l ~f = List.iter ~f:(fun (_u, x) -> f x) !l
end

module Verifying_ring = struct
  type 'a t = 'a Slow_ring.t * 'a Ring.t
  type 'a elem = 'a Slow_ring.elem * 'a Ring.elem
  let iter ((r1, r2) : _ t) ~f ~(test_a_list : [%test_eq: 'a list]) =
    let iter_to_list iter =
      let l = ref [] in
      iter ~f:(fun x -> l := x :: !l);
      !l
    in
    let l1 = iter_to_list (Slow_ring.iter r1) in
    let l2 = iter_to_list (Ring.iter r2) in
    test_a_list l1 l2;
    List.iter l1 ~f
  let verify ~test_a_list x = ignore (iter x ~f:ignore ~test_a_list)
  let create () = Slow_ring.create (), Ring.create ()
  let add (r1, r2) x : _ elem = Slow_ring.add r1 x, Ring.add r2 x
  let detach (r1, r2) = Slow_ring.detach r1; Ring.detach r2
end

let iter_max_steps t ~f =
  let i = ref 0 in
  Ring.iter t ~f:(fun x ->
    incr i;
    if !i > 100 then failwith "iter_max_steps, too many steps";
    f x;
  )

let ring_to_list t =
  let xs = ref [] in
  iter_max_steps t ~f:(fun x -> xs := x::!xs);
  List.rev !xs

let ring_to_list_and_iter ring ~f =
  let xs = ref [] in
  iter_max_steps ring ~f:(fun x -> f x; xs := x::!xs);
  List.rev !xs

let add_detaching r x es =
  Ring.add_calling r x ~f:(fun () -> List.iter es ~f:Ring.detach)

let rec take_one_head = function
  | [] -> []
  | (l :: ls) ->
    (* don't take the head of l *)
    List.map (take_one_head ls) ~f:(fun (head, ls) -> head, l :: ls)
    @
    (* take the head of l *)
    match l with
    | [] -> []
    | x :: xs ->
      [x, xs :: ls]
let take_one_head ls = List.rev (take_one_head ls)

let rec all_interleavings ls = match take_one_head ls with
  | [] -> [[]]
  | l -> List.bind l ~f:(fun (h, ls) ->
    List.map (all_interleavings ls) ~f:(fun l -> h :: l))

let%test_unit "all_interleavings" =
  [%test_result: int list list]
    (all_interleavings [[1;2];[3;4]])
    ~expect:[[1;2;3;4];[1;3;2;4];[1;3;4;2];[3;1;2;4];[3;1;4;2];[3;4;1;2]]

(* This test covers all the small non-concurrent tests including double-detach.
   So we might remove some of the tests below... but lets have both. *)
let%test_unit "all_small_tests" = (
  List.iter [0;1;2;3]
    ~f:(fun length ->
      List.iter
        (all_interleavings (
           List.init length ~f:(fun i -> [`add i; `del i; `del i])))
        ~f:(fun interleaving ->
          let arr = Array.init length ~f:(fun _ -> None) in
          let ring = Verifying_ring.create () in
          List.iter interleaving ~f:(function
            | `add i ->
              Array.set arr i (Some (Verifying_ring.add ring i));
              Verifying_ring.verify ring ~test_a_list:[%test_eq: int list]
            | `del i ->
              let x = Option.value_exn (Array.get arr i) in
              Verifying_ring.detach x;
              Verifying_ring.verify ring ~test_a_list:[%test_eq: int list])
        )
    )
)

(* zero elements *)
let%test_unit _ = (
  let r = Ring.create() in
  [%test_result: int list] (ring_to_list r) ~expect:[]
)

(* one element *)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  [%test_result: int list] (ring_to_list r) ~expect:[1]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)


(* two elements *)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  [%test_result: int list] (ring_to_list r) ~expect:[1;2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  Ring.detach e1;
  let _e2 = Ring.add r 2 in
  [%test_result: int list] (ring_to_list r) ~expect:[2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[1]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  Ring.detach e1;
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)

(* three elements *)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  [%test_result: int list] (ring_to_list r) ~expect:[1;2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  Ring.detach e1;
  let _e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  [%test_result: int list] (ring_to_list r) ~expect:[2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  Ring.detach e1;
  let _e3 = Ring.add r 3 in
  [%test_result: int list] (ring_to_list r) ~expect:[2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  let _e3 = Ring.add r 3 in
  [%test_result: int list] (ring_to_list r) ~expect:[1;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[1;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  Ring.detach e3;
  [%test_result: int list] (ring_to_list r) ~expect:[1;2]
)

(* [detach] during [iter] *)

let check_ring_to_list_and_iter ring ~f expect_before expect_during expect_after =
  [%test_result: int list] (ring_to_list ring) ~expect:expect_before;
  [%test_result: int list] (ring_to_list_and_iter ring ~f) ~expect:expect_during;
  [%test_result: int list] (ring_to_list ring) ~expect:expect_after;
;;

let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [1;2;3]
)
let%test_unit _ = (
  (* First testcase which fails on a buggy [detach] which changes [t.next] so the
     [detach]ed element no longer points into the ring *)
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 1 -> Ring.detach e1
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 2 -> Ring.detach e1
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 3 -> Ring.detach e1
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 1 -> Ring.detach e2
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;3] [1;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 2 -> Ring.detach e2
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [1;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 3 -> Ring.detach e2
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [1;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 1 -> Ring.detach e3
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2] [1;2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 2 -> Ring.detach e3
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2] [1;2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 3 -> Ring.detach e3
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [1;2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 1 -> (Ring.detach e1; Ring.detach e2)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 1 -> (Ring.detach e1; Ring.detach e2)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 2 -> (Ring.detach e1; Ring.detach e2)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = Ring.add r 3 in
  let f = function
    | 3 -> (Ring.detach e1; Ring.detach e2)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 1 -> (Ring.detach e2; Ring.detach e3)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1] [1]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 2 -> (Ring.detach e2; Ring.detach e3)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [1]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 3 -> (Ring.detach e2; Ring.detach e3)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [1]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 1 -> (Ring.detach e1; Ring.detach e3)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2] [2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 2 -> (Ring.detach e1; Ring.detach e3)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2] [2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = Ring.add r 3 in
  let f = function
    | 3 -> (Ring.detach e1; Ring.detach e3)
    | _ -> ()
  in
  check_ring_to_list_and_iter r ~f [1;2;3] [1;2;3] [2]
)

(* [detach] during [add] *)

let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = add_detaching r 2 [e1] in
  [%test_result: int list] (ring_to_list r) ~expect:[2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = add_detaching r 2 [e1] in
  let _e3 = Ring.add r 3 in
  [%test_result: int list] (ring_to_list r) ~expect:[2;3]
)
let%test_unit _ = (
  (* This is the simplest test case which fails on a buggy [add] which fails to (re)assign
     t'.prev after the potential GC which detaches the origin [t.prev] *)
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = add_detaching r 2 [e1] in
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = add_detaching r 3 [e1] in
  [%test_result: int list] (ring_to_list r) ~expect:[2;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = add_detaching r 3 [e2] in
  [%test_result: int list] (ring_to_list r) ~expect:[1;3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = add_detaching r 3 [e1;e2] in
  [%test_result: int list] (ring_to_list r) ~expect:[3]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let _e3 = add_detaching r 3 [e1] in
  let _e4 = Ring.add r 4 in
  [%test_result: int list] (ring_to_list r) ~expect:[2;3;4]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = add_detaching r 3 [e2] in
  let _e4 = Ring.add r 4 in
  [%test_result: int list] (ring_to_list r) ~expect:[1;3;4]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let _e3 = add_detaching r 3 [e1;e2] in
  let _e4 = Ring.add r 4 in
  [%test_result: int list] (ring_to_list r) ~expect:[3;4]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  let e3 = add_detaching r 3 [e1] in
  Ring.detach e3;
  [%test_result: int list] (ring_to_list r) ~expect:[2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let e3 = add_detaching r 3 [e2] in
  Ring.detach e3;
  [%test_result: int list] (ring_to_list r) ~expect:[1]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  let e3 = add_detaching r 3 [e1;e2] in
  Ring.detach e3;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)

(* double detach *)

let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  Ring.detach e1;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let _e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[2]
)
let%test_unit _ = (
  let r = Ring.create() in
  let _e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[1]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e1;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  (* This test break if the code fails to guard against double [detach]ing. *)
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e2;
  Ring.detach e1; (* Must not accidentally re-attach e2 ! *)
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e1;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e2;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e1;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e2;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)

let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e1;
  Ring.detach e2;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e2;
  Ring.detach e1;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e1;
  Ring.detach e2;
  Ring.detach e2;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e1;
  Ring.detach e1;
  Ring.detach e2;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e1;
  Ring.detach e2;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)
let%test_unit _ = (
  let r = Ring.create() in
  let e1 = Ring.add r 1 in
  let e2 = Ring.add r 2 in
  Ring.detach e2;
  Ring.detach e2;
  Ring.detach e1;
  Ring.detach e1;
  [%test_result: int list] (ring_to_list r) ~expect:[]
)

