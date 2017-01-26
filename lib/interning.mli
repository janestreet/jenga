(** Hash consing of string, for space savings, both on disk and in memory. *)

open! Core

module String : functor (X : sig val who : string end) -> sig

  (** Interned string handle. Binprot representation only makes sense when written as a
      part of a larger ['a With_store.t] data structure. *)
  type t [@@deriving sexp, bin_io, hash, compare]

  val intern : string -> t
  val extern : t -> string
  val hash : t -> int

  (** ['a With_store.t] lets you serialize and later deserialize a data structure that
      contains [t]s in it.
  *)
  module With_store : sig
    type 'a t [@@deriving bin_io]
    val snapshot : 'a -> 'a t
    val value : 'a t -> 'a
  end

end
