(** [Rule.t] supports the description of build-rules in jenga.
    Two varieties:

    - alias-rule: Allows a symbolic [Alias.t] to stand for some dependencies.
    - target-rule: A rule to generate some targets using an [Action.t].

    An alias-rule is constructed by [alias] of which [default] is a useful special case.
    A target-rule is constructed by [create].

    [Target-rule.t] allows the description of dynamic build rules: Although the targets
    must be static, the dependencies and action may be contingent on further dependencies.
    This is achieved by constructing a target-rule from a value of type: [Action.t Dep.t]

    Make-style rule-triples are a trivial special case, and can be constructed using the
    convenience wrapper [simple], defined as:

        let simple ~targets ~deps ~action =
          create ~targets (
            Dep.all_unit deps *>>| fun () ->
            action
          )
*)

module Target_rule : sig
  type t
  [@@deriving sexp_of]
  val create : targets:Path.Rel.t list -> Action.t Dep.t -> t
  val targets : t -> Path.Rel.t list
  val head_target : t -> Path.Rel.t
  val head_target_and_rest : t -> Path.Rel.t * Path.Rel.t list
  val action_depends : t -> Action.t Dep.t
end

type t =
| Target of Target_rule.t
| Alias of Alias.t * unit Dep.t
[@@deriving sexp_of]

val targets : t -> Path.Rel.t list

val alias : Alias.t -> unit Dep.t list -> t
val default : dir:Path.t -> unit Dep.t list -> t

val create : targets:Path.t list -> Action.t Dep.t -> t
val simple : targets:Path.t list -> deps:unit Dep.t list -> action:Action.t -> t
