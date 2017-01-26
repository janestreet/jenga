open Core
open! Int.Replace_polymorphic_compare

(* A [Ring.t] is represented as a cyclic doubly-linked list. The same representation is
   used for the ring itself, and for it elements; the ring being represented as a sentinel
   element containing a dummy value which is never used.

   The implementation supports calls to [detach] which preempt a running [iter] or [add],
   by maintaining the invariant that by following [next] pointers from any element, even
   an element which has been detached, we will reach the [sentinel] element of the [ring]
   in which the element was originally added.

   Two places, marked "ATOMIC" in the code below, require a block of code to be executed
   atomically. We claim this atomicity is guaranteed by the (current) Ocaml implementation
   because the statements do no allocation and so can not be preempted. *)

type 'a t = {mutable prev: 'a t; mutable value: 'a; mutable next: 'a t}
type 'a elem = 'a t

(* An empty ring is represented by self-referencing [sentinel] element. The never-used
   [value] is created using [Obj.magic] since there are no values of type ['a] to hand. *)
let create () =
  let rec t = {prev = t; next = t; value = Obj.magic None} in
  t

let keep_alive t parent =
  (* Note that this only works on type [t] and not [elem], because no function ever looks
     at the [value] inside a [t]. *)
  t.value <- Obj.magic parent

(* New elements are added at the end of the ring. The new element becomes the [next] of
   the previously added element. The new element's [next] points to the [sentinel]. *)
let add_calling t value ~f =
  let elem = {prev = t.prev; value; next = t} in (* may cause GC *)
  f();
  begin (* ATOMIC block *)
    (* [elem.prev] must be assigned in case the original [t.prev] was detached *)
    elem.prev <- t.prev;
    t.prev.next <- elem;
    t.prev <- elem;
  end;
  elem

let add t value = add_calling t value ~f:(fun () -> ())

(* An element is detached from a ring by updating the [next] and [prev] pointers of the
   elements found before and after the elements being detached.

   The detached element is marked by setting its [prev] pointer to self-reference.  This
   allows a check to ensure subsequent calls to detach have no effect.

   It is critical that the [next] pointer of the detached element continues to reference
   back into the linked list from which the [elem] itself is no longer a part. *)
let detach elem =
  begin (* ATOMIC block *)
    if not (phys_equal elem elem.prev) then (* if not detached.. *)
      begin
        elem.next.prev <- elem.prev;
        elem.prev.next <- elem.next;
        elem.prev <- elem; (* mark as detached *)
      end
  end

(* Iteration over a ring is achieved by following [next] pointers until the [sentinel]
   element which represents the start of the ring is reached. We do not call [f] with the
   (dummy) value of the [sentinel]. *)
let iter =
  let rec loop ~sentinel ~elem ~f =
    if phys_equal sentinel elem
    then ()
    else (f elem.value; loop ~sentinel ~elem:elem.next ~f)
  in
  fun t ~f ->
    loop ~sentinel:t ~elem:t.next ~f

let size t =
  let n = ref 0 in
  iter t ~f:(fun _ -> incr n);
  !n
