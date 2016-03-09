
open! Core.Std

(* [Gen_key.t] is the key used to select a [Scheme.t] for rule-generation.
   Now just a [Path.Rel.t] *)

type t [@@deriving sexp, compare, bin_io]
include Hashable_binable with type t := t
include Comparable_binable with type t := t

val create : dir:Path.Rel.t -> t
val to_string : t -> string
val directory : t -> Path.Rel.t
