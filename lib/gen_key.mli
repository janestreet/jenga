
open Core.Std

(* [Goal.t] is the key to rule-generation. A directory paired with a string tag. *)

type t = {tag : string; dir : Path.Rel.t;} with sexp, bin_io
include Hashable_binable with type t := t

val create : tag:string -> dir:Path.Rel.t -> t
val to_string : t -> string
val directory : t -> Path.Rel.t
