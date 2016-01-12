
open Core.Std
open! Int.Replace_polymorphic_compare

module Target_rule = Rule.Target_rule

let equal_using_compare compare = fun x1 x2 -> 0 = compare x1 x2

type t = { rules : Rule.t list }
[@@deriving sexp_of]

let create rules = { rules }
let rules t = t.rules

let lookup_target t rel =
  match
    List.concat_map t.rules ~f:(function
    | Rule.Alias _ -> []
    | Rule.Target tr -> if List.mem (Target_rule.targets tr) rel then [tr] else [])
  with
  | [] -> `ok None
  | [tr] -> `ok (Some tr)
  | _::_::_ -> `dup

let lookup_alias t alias =
  List.concat_map t.rules ~f:(function
  | Rule.Alias (a,dep) ->
    if Alias.(equal_using_compare compare alias a) then [dep] else []
  | Rule.Target _ -> [])
