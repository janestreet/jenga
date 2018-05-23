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

  end = struct
    type t = string

    module Weak_set = Caml.Weak.Make(String)

    let the_interning_set = Weak_set.create 65536

    let extern = Fn.id
    let intern x = Weak_set.merge the_interning_set x
  end

  (* [t] is just [Shared.t]. compare/hash defined using the contained string data. *)
  include Shared

  let compare t1 t2 = String.compare (extern t1) (extern t2)
  let hash_fold_t state t = hash_fold_string state (extern t)
  let hash = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t

  (* For sexp conversion, we use extern/intern *)
  let sexp_of_t t = String.sexp_of_t (Shared.extern t)
  let t_of_sexp sexp = Shared.intern (String.t_of_sexp sexp)

  include Bin_prot.Utils.Make_binable(struct
      module Binable = String
      type nonrec t = t
      let to_binable = Shared.extern
      let of_binable = Shared.intern
    end)
end
