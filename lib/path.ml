
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Inner : sig

  type t with sexp, bin_io
  include Hashable_binable with type t := t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val split : t -> t * string
  val dirname : t -> t
  val basename : t -> string
  val create_from_absolute : string -> t option

  val relative : dir:t -> string -> t
  val the_root : t

  val to_rrr_string : t -> string

end = struct

  module T = struct

    type t = {
      rrr : string (* "rrr" = repo-root-relative *)
    } with bin_io

    let hash t = String.hash t.rrr
    let equal t1 t2 = String.equal t1.rrr t2.rrr
    let compare t1 t2 = String.compare t1.rrr t2.rrr

  (* avoid "rrr" in sexp conversion *)
    let sexp_of_t t = String.sexp_of_t t.rrr
    let t_of_sexp sexp = { rrr = String.t_of_sexp sexp }

  end

  include T
  include Hashable.Make_binable(T)


  let starts_with_slash s =
    (match s with "" -> false | _ -> true)
    && (match (String.get s 0) with | '/' -> true | _-> false)

  let create ~rrr =
    assert (not (starts_with_slash rrr));
    { rrr }

  let the_root = create ~rrr:""

  let create_from_absolute_exn =
    fun s ->
      let repo_root = Repo_root.get() in
      let repo_root_slash = repo_root ^ "/" in
      if String.equal s repo_root then the_root else
        match String.chop_prefix s ~prefix:repo_root_slash with
        | Some rrr ->
          create ~rrr
        | None ->
          failwith (
            sprintf "Path.create_from_absolute_exn: '%s' does not start with repo_root prefix: %s"
              s repo_root_slash
          )

  let create_from_absolute s =
    try (Some (create_from_absolute_exn s)) with _ -> None

  let split t =
    if equal t the_root then failwith "Path.split, cant split the_root" else
      match String.rsplit2 t.rrr ~on:'/' with
      | Some (dir,base) -> { rrr = dir }, base
      | None -> the_root, t.rrr

  let dirname t = fst (split t)
  let basename t = snd (split t)



  let relative_seg ~dir ~seg =
    assert (match seg with "" -> false | _ -> true);
    let at_root = T.equal dir the_root in
    match seg with
    | "." -> dir
    | ".." ->
      if at_root
      then failwithf "Path.relative, cant .. the_root" ()
      else dirname dir
    | _ ->
      if at_root
      then create ~rrr:seg
      else create ~rrr:(dir.rrr ^ "/" ^ seg)


  let relative ~dir s =
    if starts_with_slash s then failwithf "Path.relative, starts with / - %s" s ();
    let segs = String.split s ~on:'/' in
    List.fold segs ~init:dir ~f:(fun dir seg -> relative_seg ~dir ~seg)

  let to_rrr_string t =
    (* Special case for repo_root. Represented as ""; Displayed as "." *)
    if String.equal t.rrr "" then "." else t.rrr


end

include Inner

(* compute relative ".."-based-path-string path to reach [path] from [dir] *)
let dotdot ~dir path =
  let segs = String.split (to_rrr_string dir) ~on:'/' in
  let dots = List.map segs ~f:(fun seg -> assert (not (String.(seg=".."))); "../") in
  String.concat dots ^ (to_rrr_string path)

let suffix t s =
  let dir,b = split t in
  relative ~dir (b ^ s)

let to_absolute_string t =
  if equal t the_root
  then Repo_root.get()
  else Repo_root.get() ^ "/" ^ (to_rrr_string t)

let root_relative s =
  relative ~dir:the_root s

let special_prefix = ".jenga"

let is_special_jenga_path t = String.is_prefix ~prefix:special_prefix (to_rrr_string t)

let special suf = special_prefix ^ suf

let log_basename = special ".debug"
let sexp_db_basename = special ".sexp-db"
let db_basename = special ".db"
let dot_basename = special ".dot"
let lock_basename = special ".lock"
let server_basename = special ".server"


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
