open Core
open Async
open! Int.Replace_polymorphic_compare

module T = struct
  type t =
    | Jengaroot
    | Path of Path.Rel.t
    | Alias of Alias.t
  [@@deriving sexp, hash, compare, bin_io]
end

include T
include Hashable.Make_binable(T)
include Comparable.Make_binable(T)

let to_string = function
  | Jengaroot -> ".jengaroot"
  | Path path -> Path.Rel.to_string path
  | Alias alias -> Alias.to_string alias

let directory = function
  | Jengaroot -> Path.Rel.the_root
  | Path path -> Path.Rel.dirname path
  | Alias alias -> Alias.directory alias

let parse_string ~dir string =
  let path = Path.Rel.relative ~dir string in
  let dir, base = Path.Rel.split path in
  match String.chop_prefix base ~prefix:"." with
  | Some "jengaroot" -> return Jengaroot
  | Some after_dot when not (String.is_empty after_dot) ->
    return (Alias (Alias.create ~dir:(Path.of_relative dir) after_dot))
  | _ ->
    Sys.is_directory_exn ~follow_symlinks:false (
      Path.to_absolute_string (Path.of_relative path))
    >>| function
    (* Map: [jenga dir] -> [jenga dir/.DEFAULT] *)
    | true -> Alias (Alias.default ~dir:(Path.of_relative path))
    | false -> Path path
