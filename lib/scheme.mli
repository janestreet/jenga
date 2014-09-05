
open Core.Std
open Async.Std


(* [Scheme.t] supports the description of rule-generation schemes in jenga, where the
   scheme may itself have dependencies. This allows generation of rules based on a glob
   pattern, say [*.c], or generation w.r.t to a config file. *)

include module type of Scheme_type

val rules : Rule.t list -> t
val dep : t Dep.t -> t
val all : t list -> t
val exclude : (Path.t -> bool) -> t -> t

val rules_dep : Rule.t list Dep.t -> t
val contents : Path.t -> (string -> t)-> t
val no_rules : t
val switch_glob : ?def:t -> (string * t) list -> t
