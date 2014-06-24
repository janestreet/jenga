
open Core.Std
open Async.Std

let select =
  (* Select tenacious implementation:
     Default is the existing/working sample-based version.

     Use env-var to select the new lifetime-based version; a work in progress; not
     complete or working yet. *)
  match Core.Std.Sys.getenv "NEW_LIFETIME_TENACIOUS" with
  | None -> false
  | Some _ -> true

let () =
  if select then (
    Printf.eprintf "using lifetime tenacious\n%!"
  )

module type S = sig
  include Tenacious_intf.S
  val version : string
end

module Existing_sample_based_version : S = struct

  include Tenacious_sample_lib.Tenacious_sample

  (* The sample based implementation contains the inscrutable [with_semantics] in its
     interface. This function has been removed from the Tenacious interface, in favour of
     4 specializations:

        before_redo
        uncancellable
        with_acquire_release
        desensitize

     which are implemented here for the sample-based implementation.
  *)

  let before_redo t ~f =
    let first_time = ref true in
    let hook () =
      if !first_time
      then first_time := false
      else f ()
    in
    let f sample ~cancel = hook (); sample ~cancel in
    with_semantics t ~f

  let uncancellable t =
    with_semantics t ~f:(fun sample ~cancel:__ ->
      sample ~cancel:Heart.unbreakable
  )

  let with_acquire_release t ~acquire ~release =
    with_semantics t ~f:(fun sample ~cancel ->
      acquire () >>= fun () ->
      sample ~cancel >>| fun x ->
      release();
      x
    )

  let desensitize t =
    let f sample ~cancel =
      sample ~cancel >>| function
      | None -> None
      | Some (x, heart) ->
        Some ((x,heart), Heart.unbreakable)
    in
    with_semantics t ~f

  let version = "sample-based"

end

module Work_in_progress_lifetime_version : S = struct
  include Tenacious_lifetime_lib.Ten
  let version = "lifetime-based"
end

let m =
  match select with
  | false -> ( module Existing_sample_based_version : S )
  | true ->  ( module Work_in_progress_lifetime_version : S )

module M = ( val m : S )

include M
