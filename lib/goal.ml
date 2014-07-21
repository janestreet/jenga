
open Core.Std

module T = struct
  type t = Path of Path.Rel.t | Alias of Alias.t with sexp, bin_io, compare
  let hash = Hashtbl.hash
end

include T
include Hashable.Make(T)

let to_string = function
  | Path path -> Path.Rel.to_string path
  | Alias alias -> Alias.to_string alias

let directory = function
  | Path path -> Path.Rel.dirname path
  | Alias alias -> Alias.directory alias

let parse_string ~dir string = (* for command-line selection of  top-level demands *)
    (* syntax...
       foo             - target
       path/to/foo     - target
       .foo            - alias
       path/to/.foo    - alias
    *)
  let dir,base =
    match String.rsplit2 string ~on:'/' with
    | None -> dir, string
    | Some (rel_dir_string,base) ->
      match rel_dir_string with
      | "." -> dir,string
      | _ -> Path.Rel.relative ~dir rel_dir_string, base
  in
  match String.chop_prefix base ~prefix:"." with
  | None -> Path (Path.Rel.relative ~dir base)
  | Some after_dot -> Alias (Alias.create ~dir:(Path.of_relative dir) after_dot)
