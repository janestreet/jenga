
open Core.Std
open Async.Std

module T = struct
  type t = Path of Path.Rel.t | Alias of Alias.t
  with sexp, compare, bin_io
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

let parse_string ~dir string =
  let path = Path.Rel.relative ~dir string in
  let dir, base = Path.Rel.split path in
  match String.chop_prefix base ~prefix:"." with
  | Some after_dot when not (String.is_empty after_dot) ->
    return (Alias (Alias.create ~dir:(Path.of_relative dir) after_dot))
  | _ ->
    Sys.is_directory_exn ~follow_symlinks:false (
      Path.to_absolute_string (Path.of_relative path))
    >>| function
    | true -> Alias (Alias.default ~dir:(Path.of_relative path))
    | false -> Path path
