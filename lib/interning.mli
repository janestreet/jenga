(** Hash consing of string, for space savings, both on disk and in memory. *)

open! Core

module String : functor (X : sig val who : string end) -> sig

  (** Interned string handle. Serialization does not preserve the sharing. *)
  type t [@@deriving sexp, bin_io, hash, compare]

  val intern : string -> t
  val extern : t -> string
  val hash : t -> int
end
