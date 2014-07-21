
open Core.Std

module T = struct
  type t = {
    tag : string;
    dir : Path.Rel.t;
  } with sexp, bin_io, compare
  let hash = Hashtbl.hash
end

include T
include Hashable.Make_binable(T)

let create ~tag ~dir = { tag; dir; }
let to_string t = sprintf "%s:%s" t.tag (Path.Rel.to_string t.dir)
let directory t = t.dir
