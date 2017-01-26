open Core
open! Int.Replace_polymorphic_compare

include Scheme_type

let ( *>>| ) = Dep.map

let rules ?(sources = []) xs = Rules (Ruleset.create ~sources xs)
let sources paths = rules ~sources:paths []

let dep x = Dep x

let all ts = All ts

let glob g f = Glob (g, f)

let rules_dep x = dep (x *>>| rules)
let empty = rules []

let contents path f =
  dep (Dep.contents path *>>| f)
