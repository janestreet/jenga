
open! Core

type t [@@deriving sexp, bin_io, hash, compare]

val to_string : t -> string

(** only matches the given string *)
val create_from_literal_string : string -> t

(** accepts custom glob syntax (see examples in glob_tests.ml) *)
val create_from_glob_string : string -> t

(** accepts regex syntax as supported by Str module *)
val create_from_regexp_string : string -> t (* not used yet *)

val matches : t -> string -> bool
