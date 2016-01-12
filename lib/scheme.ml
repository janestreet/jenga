
open Core.Std
open! Int.Replace_polymorphic_compare

include Scheme_type

let ( *>>| ) = Dep.map

let rules xs = Rules (Ruleset.create xs)

let dep =
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun x ->
    let u = genU() in
    Dep (u,x)

let all ts = All ts
let exclude f t = Exclude ((fun rel -> f (Path.of_relative rel)),t)

let rules_dep x = dep (x *>>| rules)
let no_rules = rules []

let contents path f =
  exclude (Path.equal path) (
    dep (Dep.contents path *>>| f)
  )
