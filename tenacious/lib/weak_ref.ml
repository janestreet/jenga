open Core
open! Int.Replace_polymorphic_compare

(* [Core.Weak] provides support for an array of weak references (to heap blocks).
   Specialize this to support individual weak-refs, by always using arrays of length 1 *)

type 'a t = 'a Weak.t

let create x =
  let wa = Weak.create ~len:1 in
  let hb = Heap_block.create_exn x in
  Weak.set wa 0 (Some hb);
  wa

let get t =
  match (Weak.get t 0) with
  | Some hb -> Some (Heap_block.value hb)
  | None -> None
