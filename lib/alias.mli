
open! Core

(** [Alias.t] is a symbolic target, ie build-goal which is not associated with any
    generated files. It is used as a way of asking jenga to do an arbitrary [Dep]
    computation, for instance build a set of files ("all the libraries in the tree" for
    instance) and run actions that produce no target, like tests.
    The user indicates an alias on the command line with a leading period.  So for example
    ".DEFAULT" or ".runtest". Aliases are directory relative. *)

type t [@@deriving hash, sexp, bin_io]
include Comparable with type t := t
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

