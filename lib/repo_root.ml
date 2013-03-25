
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

let r = ref None

let set ~dir =
  match !r with
  | Some _ -> failwith "Repo_root.set - called more than once"
  | None -> r := Some dir

let discover ~marker =
  let start_dir = Core.Std.Sys.getcwd() in
  let rec loop dir =
    match Sys.file_exists (dir ^/ marker) with
      | `No | `Unknown ->
        if String.equal dir Filename.root
        then `cant_find_root
        else loop (Filename.dirname dir)
      | `Yes -> set ~dir; `ok dir

  in
  loop start_dir

let get () =
  match !r with
  | None -> failwith "Repo_root.get - called before discover/set"
  | Some v -> v
