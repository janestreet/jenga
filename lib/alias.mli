
open Core.Std
open Async.Std

(* [Alias.t] is a build-goal which is not associated with any generated files.  The user
   indicates an alias when running jenga by a leading period.  So for example ".DEFAULT"
   or ".runtest". Aliases are directory relative. *)

type t [@@deriving sexp, bin_io]
include Hashable with type t := t

val split : t -> Path.Rel.t * string
val to_string : t -> string
val directory : t -> Path.Rel.t

val create : dir:Path.t -> string -> t
val default : dir:Path.t -> t

