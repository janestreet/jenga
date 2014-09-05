
type t

(* [Ruleset.t] supports a set if rules, with lookup *)

val create : Rule.t list -> t

val lookup_target : t -> Path.Rel.t -> [ `ok of Rule.Target_rule.t option | `dup ]
val lookup_alias : t -> Alias.t -> unit Dep.t list

val rules : t -> Rule.t list
