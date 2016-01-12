open Core.Std

(* [Scheme_type.t] is the variant implementation behind [Scheme.t].  Values of this type
   are interpreted by jenga's build algorithm.*)

type t =
| Dep of (int * t Dep.t)
| Rules of Ruleset.t
| All of t list
| Exclude of (Path.Rel.t -> bool) * t
[@@deriving sexp_of]
