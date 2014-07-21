
open Core.Std

(* [Goal.t] is a build goal, as demanded on the command line. Either a repo-relative path,
   or an alias (i.e. .DEFAULT) not associated with any file. *)

type t = Path of Path.Rel.t | Alias of Alias.t
with sexp, bin_io
include Hashable with type t := t

val to_string : t -> string
val directory : t -> Path.Rel.t
val parse_string : dir:Path.Rel.t -> string -> t
