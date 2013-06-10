
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

let jenga_root_basename =
  match Core.Std.Sys.getenv "JENGA_ROOT" with
  | None -> "JengaRoot.ml"
  | Some x -> x

let discover_root() =
  match Repo_root.discover ~marker:jenga_root_basename with
  | `cant_find_root ->
    failwithf "Cant find '%s' in start-dir or any ancestor dir"
      jenga_root_basename ()
  | `ok root -> root
