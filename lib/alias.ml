open Core
open! Int.Replace_polymorphic_compare

module T = struct
  type t = {
    dir : Path.Rel.t;
    name : string;
  } [@@deriving sexp, bin_io, hash, compare]
end

include T
include Comparable.Make(T)
include Hashable.Make(T)

let create ~dir name = { dir; name; }
let split t = t.dir, t.name
let default ~dir = create ~dir "DEFAULT"

let to_string t =
  if Path.Rel.equal t.dir Path.Rel.the_root
  then sprintf ".%s" t.name
  else sprintf "%s/.%s" (Path.Rel.to_string t.dir) t.name

let directory t = t.dir
let basename t = "." ^ t.name

let create ~dir name =
  match (Path.case dir) with
  | `relative dir -> create ~dir name
  | `absolute _ ->
    failwith "[Alias.create] called with absolute directory"

let default ~dir =
  match (Path.case dir) with
  | `relative dir -> default ~dir
  | `absolute _ ->
    failwith "[Alias.default] called with absolute directory"
