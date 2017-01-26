(** [Scheme.t] supports the description of rule-generation schemes in jenga, where the
    scheme may itself have dependencies. This allows generation of rules based on a glob
    pattern, say [*.c], or generation w.r.t to a config file. *)

open! Core

include module type of Scheme_type

val rules : ?sources:Path.t list -> Rule.t list -> t
val sources : Path.t list -> t
val dep : t Dep.t -> t
val all : t list -> t
val glob : Db.Glob.t -> (Path.t list -> t) -> t

val rules_dep : Rule.t list Dep.t -> t
val contents : Path.t -> (string -> t) -> t
val empty : t
