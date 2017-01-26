(** [Ruleset.t] represent a set of rules, with lookup. *)

open! Core

type t
[@@deriving sexp_of]

val empty : t
val create
  : sources:Path.t list
  -> Rule.t list
  -> [ `Duplicate_target of Path.Rel.t
     | `Ok of t ]
val union : t -> into:t -> [ `Duplicate_target of Path.Rel.t | `Ok of t ]

val targets : t -> Path.Rel.t list
val check_for_non_local_targets
  : t
  -> dir:Path.Rel.t
  -> [ `Alias of Alias.t
     | `Rule_target of Path.Rel.t
     | `Sources of Path.Rel.t ] list

val find_target : t -> Path.Rel.t ->
  [ `Found of Rule.Target_rule.t option | `Not_found ]
val find_alias : t -> Alias.t -> unit Dep.t list option
