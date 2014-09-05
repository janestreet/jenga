
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

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

let if_cond ~f ~yes no =
  all [
    exclude (fun x -> not (f x)) yes;
    exclude f no;
  ]

let rec switch_cond ?(def=no_rules) = function
  | [] -> def
  | (cond,gen)::xs -> if_cond ~f:cond ~yes:gen (switch_cond ~def xs)

let switch_glob ?def xs =
  switch_cond ?def (
    List.map xs ~f:(fun (string,gen) ->
      let pat = Pattern.create_from_glob_string string in
      let cond path = Pattern.matches pat (Path.to_string path) in
      (cond,gen))
  )
