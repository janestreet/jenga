
open Core.Std

module T = struct
  type t = {
    dir : Path.Rel.t;
  } with sexp, bin_io, compare
  let hash = Hashtbl.hash
end

include T
include Hashable.Make_binable(T)
include Comparable.Make_binable(T)

let create ~dir = { dir }
let to_string t = sprintf "%s" (Path.Rel.to_string t.dir)
let directory t = t.dir
let of_goal goal = create ~dir:(Goal.directory goal)
