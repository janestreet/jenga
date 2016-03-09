
open! Core.Std

(* [Alias.t] is a build-goal which is not associated with any generated files.  The user
   indicates an alias when running jenga by a leading period.  So for example ".DEFAULT"
   or ".runtest". Aliases are directory relative. *)

type t [@@deriving sexp, bin_io]
include Hashable with type t := t

(** [split a/b/.DEFAULT] is [("a/b", "DEFAULT")]. *)
val split : t -> Path.Rel.t * string
(** [to_string a/b/.DEFAULT] is ["a/b/.DEFAULT"]. *)
val to_string : t -> string
val directory : t -> Path.Rel.t
(** [basename "a/b/.DEFAULT"] is [".DEFAULT"]. *)
val basename : t -> string

val create : dir:Path.t -> string -> t
val default : dir:Path.t -> t

