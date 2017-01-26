open Core
open! Int.Replace_polymorphic_compare

(* [Scheme_type.t] is the variant implementation behind [Scheme.t].  Values of this type
   are interpreted by jenga's build algorithm.*)

type t =
| Dep of t Dep.t
| Glob of Db.Glob.t * (Path.t list -> t)
| Rules of [ `Duplicate_target of Path.Rel.t | `Ok of Ruleset.t ]
| All of t list
[@@deriving sexp_of]
