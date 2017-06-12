open Core
open! Int.Replace_polymorphic_compare

module Weak_ref = Tenacious_lib.Weak_ref

module String(X : sig val who : string end) = struct
  open X let _ = who

  module Shared : sig

    (* [Shared.t] represents an interned string when loaded in memory *)
    type t
    val intern : string -> t
    val extern : t -> string
    val iter_all : f:(t -> unit) -> unit

  end = struct

    type t = {
      (* [string] field is marked as [mutable] so [t]s are always created on the heap, as
         opposed to the static data section, which would cause [Heap_block.create_exn] to
         fail when we attempt to create a [Weak_ref.t] to the [t] *)
      mutable string : string;
    }

    let extern t = t.string

    let the_interning_table : (t Weak_ref.t String.Table.t) = String.Table.create()

    let intern string =
      match
        match String.Table.find the_interning_table string with
        | None -> None
        | Some w -> Weak_ref.get w
      with
      | Some t -> t
      | None ->
        let t = { string } in
        let w = Weak_ref.create t in
        (* Use [Table.set], not [Table.add_exn] to allow reinserting a string in the case
           the weak-ref is None, but it not yet been [gc]ed from the table *)
        String.Table.set the_interning_table ~key:string ~data:w;
        t

    let iter_all ~f =
      (* During the iteration, elements which have been garbage-collected,
         (i.e. [Weak_ref.get] returns [None]) are removed from the table *)
      String.Table.filter_inplace the_interning_table ~f:(fun w ->
        match Weak_ref.get w with
        | Some t -> f t; true
        | None -> false
      )

  end

  (* [t] is just [Shared.t]. compare/hash defined using the contained string data. *)
  include Shared

  let compare t1 t2 = String.compare (extern t1) (extern t2)
  let hash_fold_t state t = hash_fold_string state (extern t)
  let hash = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t

  (* For sexp conversion, we use extern/intern *)
  let sexp_of_t t = String.sexp_of_t (Shared.extern t)
  let t_of_sexp sexp = Shared.intern (String.t_of_sexp sexp)

  module Handle = struct
    (* [Handle.t] represents an interned-string when saved persistently (via bin_io) *)
    module T = struct
      type t = int [@@deriving sexp, bin_io, hash, compare]
      let hash x = x
    end
    include T
    include Hashable.Make_binable(T)
  end

  module Snap : sig

    (* [Snap.t] represents a interning-store suitable for bin_io loading/saving *)
    type t [@@deriving bin_io]
    val create : (string * Handle.t) list -> t
    val alist : t -> (string * Handle.t) list

  end = struct

    type t = {
      alist: (string * Handle.t) list;
    }
    [@@deriving bin_io, fields]

    let create alist = { alist }

  end

  module Loading : sig

    (* This module is used when loading from bin_io.
       It provides a mapping from [Handle.t] to [Shared.t] during the load
       which is initialized by [unsnap] and cleared by [has_ended]. *)

    val to_shared : Handle.t -> Shared.t
    val unsnap : Snap.t -> unit
    val has_ended : unit -> unit

  end = struct

    let via_handle = Handle.Table.create()

    let to_shared handle = Handle.Table.find_exn via_handle handle

    let unsnap snap =
      List.iter (Snap.alist snap) ~f:(fun (string,handle) ->
        Handle.Table.add_exn via_handle ~key:handle ~data:(Shared.intern string)
      )

    let has_ended () = Handle.Table.clear via_handle

  end

  module Saving : sig

    (* This module is used when saving to bin_io.
       It provides a mapping from [Shared.t] to [Handle.t] during the save.
       You can take a snapshot of the interning table by using [snap] and then
       use [with_table] to delimit the (possibly multiple) periods when you want to
       use [to_handle]. *)

    val snap : unit -> Snap.t * Handle.t String.Table.t

    (** [to_handle] only works if called from within the function given to
        [with_table] *)
    val to_handle : Shared.t -> Handle.t
    (** set up the table to be used by [to_handle] for the duration of [f] *)
    val with_table : Handle.t String.Table.t -> f:('a -> 'b) -> 'a -> 'b

  end = struct

    let via_string = ref None

    let to_handle shared =
      String.Table.find_exn (
        Option.value_exn
          ~message:"not dynamically scoped inside with_table" (!via_string))
        (Shared.extern shared)

    let snap () =
      let via_string = String.Table.create () in
      let xs = ref [] in
      let u = ref 0 in
      Shared.iter_all ~f:(fun t ->
        incr u;
        let handle = !u in
        let string = extern t in
        String.Table.add_exn via_string ~key:string ~data:handle;
        xs := (string, handle) :: !xs
      );
      let alist = !xs in
      Snap.create alist, via_string

    let with_table table ~f x =
      match !via_string with
      | Some _ -> failwith "nested with_table is not allowed"
      | None ->
        via_string := Some table;
        let res = f x in
        via_string := None;
        res

  end

  module With_store = struct

    module Loaded_snap =
    struct

      type t = Snap.t
      include Binable.Stable.Of_binable.V1 (Snap) (struct
          type nonrec t = t
          let to_binable x = x
          let of_binable snap =
            Loading.unsnap snap; snap
        end)
    end

    module Bin = struct

      type 'a t = {
        snap : Loaded_snap.t;
        via_string : Handle.t String.Table.t;
        value : 'a;
      } [@@deriving fields, bin_shape ~basetype:"89927dee-490a-11e6-82df-ef5b401cd1a1"]

      type 'a disk = {
        snap : Loaded_snap.t;
        value : 'a
      }
      [@@deriving bin_io]

      let of_disk { snap; value } =
        { snap; value
        (* empty via_string is wrong, but we don't care as long as we don't try
           to save the thing we just loaded. *)
        ; via_string = String.Table.create ()
        }
      let to_disk (x : 'a t) : 'a disk = { snap = x.snap; value = x.value }

      let bin_write_t f buf ~pos x : int =
        Saving.with_table x.via_string ~f:(fun () ->
          bin_write_disk f buf ~pos (to_disk x)
        ) ()

      let bin_size_t f x : int =
        Saving.with_table x.via_string ~f:(fun () ->
          bin_size_disk f (to_disk x)
        ) ()

      let bin_read_t f buf ~pos_ref =
        let r = of_disk (bin_read_disk f buf ~pos_ref) in
        Loading.has_ended ();
        r

      let __bin_read_t__ _ar _buf ~pos_ref:_ _vtag =
        failwith "not a polymorphic variant"
    end

    type 'a t = 'a Bin.t [@@deriving bin_io]

    let snapshot value =
      let snap, via_string = Saving.snap () in
      {Bin.
        snap;
        via_string;
        value;
      }
    let value = Bin.value

  end

  include Binable.Stable.Of_binable.V1 (Handle) (struct
    type nonrec t = t
    let to_binable = Saving.to_handle
    let of_binable = Loading.to_shared
  end)

end
