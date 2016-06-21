open Core.Std

module Rel = Path.Rel

module Dot_jenga = struct

  let dot_jenga = ".jenga"

  let prepare () =
    Core.Std.Unix.mkdir_p (Path.to_absolute_string (Path.root_relative dot_jenga))

  let file suf = Rel.create (dot_jenga ^/ suf)

  let debug = file "debug"
  let metrics = file "metrics"
  let server = file "server"
  let plugin_cache = file "plugin-cache"
  let db ~version = file ("db-v" ^ version)
  let local_lock = file "lock"

  let matches path =
    match Path.case path with
    | `relative rel -> String.is_prefix ~prefix:dot_jenga (Path.Rel.to_string rel)
    | `absolute _ -> false

end

(* While [jenga_root] and [jenga_conf] are defined, there are calls to [Rel.root_relative]
   before [discover_root] calls [Path.Root.set] - but that is ok. *)

let jenga_root =
  Rel.create (
    match Core.Std.Sys.getenv "JENGA_ROOT_BASENAME" with
    | None -> "jengaroot.ml"
    | Some x -> x)

let jenga_conf =
  Rel.create (
    match Core.Std.Sys.getenv "JENGA_CONF_BASENAME" with
    | None -> "jenga.conf"
    | Some x -> x)

let discover_root () =
  let jenga_root_exists_in ~dir =
    let exists path =
      match Core.Std.Sys.file_exists (dir ^/ Rel.to_string path) with
      | `No | `Unknown -> false
      | `Yes -> true
    in
    exists jenga_root ||
    exists jenga_conf
  in
  let start_dir = Core.Std.Sys.getcwd() in
  let rec loop dir =
    if jenga_root_exists_in ~dir
    then Ok (Path.Abs.create dir)
    else
    if String.equal dir Filename.root
    then
      Or_error.errorf
        !"Cant find '%{Path.Rel}' or '%{Path.Rel}' in start-dir or any ancestor dir"
        jenga_conf jenga_root
    else loop (Filename.dirname dir)
  in
  loop start_dir

let discover_and_set_root () =
  Result.map ~f:Path.Repo.set_root (discover_root ())
