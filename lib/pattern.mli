
open Core.Std

type t with sexp, bin_io

val to_string : t -> string

val create_from_glob_string : string -> t
val create_from_regexp_string : string -> t (* not used yet *)

val matches : t -> string -> bool

val compare : t -> t -> int
