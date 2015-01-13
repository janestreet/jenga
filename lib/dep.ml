
(* Define constructors for [Dep.t] & additional non-primitive functions, to be provided in
   the API, allowing the GADT representation to be hidden. *)

open Core.Std

include Dep_type

module Glob = Fs.Glob

let to_string : type a. a t -> string = function
| Return _x -> "Return"
| Bind (_f,_x) -> "Bind"
| All _x -> "All"
| Cutoff (_f,_x) -> "Cutoff"
| Deferred _x -> "Deferred"
| Action_stdout _x -> "Action_stdout"
| Alias _x -> "Alias"
| Path x -> sprintf "Path: %s" (Path.to_string x)
| Source_if_it_exists _x -> "Source_if_it_exists"
| Contents x -> sprintf "Contents: %s" (Path.to_string x)
| Reflect_path _x -> "Reflect_path"
| Reflect_alias _x -> "Reflect_alias"
| Reflect_putenv -> "Reflect_putenv"
| On_filesystem path -> sprintf "On_filesystem: %s" (Path.to_string path)
| Buildable_targets path -> sprintf "Buildable_targets: %s" (Path.to_string path)
| Source_files path -> sprintf "Source_files: %s" (Path.to_string path)
| Glob_listing_OLD x -> sprintf "Glob_listing_OLD: %s" (Glob.to_string x)
| Glob_listing x -> sprintf "Glob_listing: %s" (Glob.to_string x)
| Glob_change_OLD x -> sprintf "Glob_change_OLD: %s" (Glob.to_string x)
| Glob_change x -> sprintf "Glob_change: %s" (Glob.to_string x)

let return x = Return x
let bind t f = Bind (t,f)
let map t f = bind t (fun x -> return (f x))
let all ts = All ts
let cutoff ~equal x = Cutoff (equal,x)
let deferred t = Deferred t
let action_stdout t = Action_stdout t
let alias x = Alias x
let path p = Path p
let source_if_it_exists p = Source_if_it_exists p
let contents p = Contents p

let ( *>>| ) = map

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

let on_filesystem ~dir =
  On_filesystem dir *>>| Path.Set.to_list

let buildable_targets ~dir =
  Buildable_targets dir *>>| Path.Set.to_list

let source_files ~dir =
  Source_files dir *>>| Path.Set.to_list

let glob_listing g = Glob_listing g *>>| Path.Set.to_list
let glob_change g = Glob_change g

module List = struct
  let concat_map xs ~f = map (all (List.map xs ~f)) List.concat
end
