
open Core.Std

(* todo - should the failwiths in this file be soft errors? *)

type t = {
  rrr : string (* "rrr" = repo-root-relative *)
} with sexp

let starts_with_slash s =
  s <> "" && (String.get s 0 = '/')

let create ~rrr =
  assert (not (starts_with_slash rrr));
  { rrr }

let the_root = create ~rrr:""

let create_from_absolute =
  fun s ->
  let repo_root = Repo_root.get() in
    let repo_root_slash = repo_root ^ "/" in
    if s = repo_root then the_root else
      match String.chop_prefix s ~prefix:repo_root_slash with
      | Some rrr ->
        create ~rrr
      | None ->
        failwith (
          sprintf "Path.create_from_absolute: '%s' does not start with repo_root prefix: %s"
            s repo_root_slash
        )

let relative =
  let check_not_special s =
    if starts_with_slash s then failwithf "Path.relative, starts with / - %s" s ()
    else if s = "." then failwith "Path.relative, s=."
    else if s = ".." then failwith "Path.relative, s=.."
    else ()
  in
  fun ~dir s ->
    check_not_special s;
    create ~rrr:(if dir = the_root then s else dir.rrr ^ "/" ^ s)

let suffix t s = create ~rrr:(t.rrr ^ s)

let to_rrr_string t =
  (* Special case for repo_root. Represented as ""; Displayed as "." *)
  if t.rrr = "" then "." else t.rrr

let to_absolute_string t =
  if t = the_root
  then Repo_root.get()
  else Repo_root.get() ^ "/" ^ t.rrr

let equal = (=)
let compare t1 t2 = String.compare t1.rrr t2.rrr

let split t =
  if t = the_root then failwith "Path.split, cant split the_root" else
  match String.rsplit2 t.rrr ~on:'/' with
  | Some (dir,base) -> { rrr = dir }, base
  | None -> the_root, t.rrr

let dirname t = fst (split t)
let basename t = snd (split t)

let cwd () =
  create_from_absolute (Core.Std.Sys.getcwd ())

let root_relative s =
  relative ~dir:the_root s

let special_prefix = ".jenga"

let is_special_jenga_path t = String.is_prefix ~prefix:special_prefix t.rrr

let special suf = special_prefix ^ suf

let log_basename = special ".debug"
let db_basename = special ".db"
let dot_basename = special ".dot"


type path = t with sexp
module LR = struct
  type t = [`local of path | `remote of string] with sexp
  let local x = `local x
  let remote x = `remote x
  let case t = t
  let to_absolute_string = function
    | `local path -> to_absolute_string path
    | `remote s -> s
  let to_rrr_string = function
    | `local path -> to_rrr_string path
    | `remote s -> s
  let basename =  function
    | `local path -> basename path
    | `remote s -> Filename.basename s
  let dirname =  function
    | `local path -> `local (dirname path)
    | `remote s -> `remote (Filename.dirname s)
end
