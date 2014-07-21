
open Core.Std

include Dep_type

module Glob = Fs.Glob

(* Define constructors for [Dep.t] & additional non-primitive functions, to be provided in
   the API, allowing the GADT representation to be hidden. *)

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
let glob_listing g = Glob_listing g
let glob_change g = Glob_change g

let ( *>>| ) = map

let all_unit ts = all ts *>>| fun (_:unit list) -> ()

let subdirs ~dir =
  glob_listing (Glob.create ~dir ~kinds:[`Directory] "*")

let file_exists path =
  glob_listing (Glob.create_from_path ~kinds:None path)
  *>>| function | [] -> false | _::_ -> true

let ( *>>| ) = map

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

module List = struct
  let concat_map xs ~f = map (all (List.map xs ~f)) List.concat
end

let buildable_targets ~dir =
  Buildable_targets dir *>>| Path.Set.to_list
