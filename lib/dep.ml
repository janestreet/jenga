
(* Define constructors for [Dep.t] & additional non-primitive functions, to be provided in
   the API, allowing the GADT representation to be hidden. *)

open Core
open! Int.Replace_polymorphic_compare

include Dep_type

module Glob = Fs.Glob

let return x = Return x
let bind t ~f = Bind (t, f)
let map t f = Map (t, f)
let all ts = All ts
let cutoff ~equal x = Cutoff (equal, x)
let deferred t = Deferred t
let action_stdout t = Action_stdout t
let alias x = Alias x
let path p = Path p
let getenv v = Var v
let source_if_it_exists p = Source_if_it_exists p
let contents p = Contents p
let group_dependencies t = Group_dependencies t
let ( *>>| ) = map
let memoize ~name t = Memoize { name; t; cached_exec = None; cached_reflect = None }

let all_unit ts = all ts *>>| fun (_:unit list) -> ()

(* glob function with old FS-only semantics *)
let fs_glob_listing g = Glob_listing_OLD g *>>| Path.Set.to_list
let fs_glob_change g = Glob_change_OLD g

let subdirs ~dir =
  (* ok to use [fs_glob_change] because jenga cant build directories *)
  fs_glob_listing (Glob.create ~dir ~kinds:[`Directory] "*")

let file_exists path =
  fs_glob_listing (Glob.create_from_path ~kinds:None path)
  *>>| function | [] -> false | _::_ -> true

let file_existence path =
  fs_glob_change (Glob.create_from_path ~kinds:None path)

let both : ('a t -> 'b t -> ('a * 'b) t) =
  fun a b ->
    all [
      (a *>>| fun a -> `a a);
      (b *>>| fun b -> `b b);
    ] *>>| function
    | [`a a; `b b] -> (a,b)
    | _ -> assert false

let action a =
  action_stdout a *>>| fun (_:string) -> ()

let contents_cutoff p =
  cutoff ~equal:String.equal (contents p)

let buildable_targets ~dir =
  Buildable_targets dir *>>| Path.Set.to_list

let source_files ~dir =
  Source_files dir *>>| Path.Set.to_list

let glob_listing g = Glob_listing g *>>| Path.Set.to_list
let glob_change g = Glob_change g

module List = struct
  let concat_map xs ~f = map (all (List.map xs ~f)) List.concat
  let concat xs = map (all xs) List.concat
end
