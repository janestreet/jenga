open Core
open! Int.Replace_polymorphic_compare

type t =
| No_record_of_being_run_before
| Action_changed
| Deps_have_changed               of Db.Pm_key.t list
| Targets_missing                 of Path.Rel.t list
| Targets_not_as_expected         of Path.Rel.t list
[@@deriving sexp_of]

let to_string config = function
  | No_record_of_being_run_before   -> "initial"
  | Action_changed                  -> "action"
  | Deps_have_changed keys ->
    if Config.show_actions_run_verbose config then
      sprintf "deps: %s" (String.concat ~sep:" " (List.map keys ~f:Db.Pm_key.to_string))
    else
      "deps"

  | Targets_missing _               -> "missing"
  | Targets_not_as_expected _       -> "unexpected"
