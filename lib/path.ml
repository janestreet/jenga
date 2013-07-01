
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

let special_prefix = ".jenga"
let special suf = special_prefix ^ suf

let log_basename = special ".debug"
let sexp_db_basename = special ".sexp-db"
let db_basename = special ".db"
let dot_basename = special ".dot"
let lock_basename = special ".lock"
let server_basename = special ".server"
let plugin_cache_basename = special ".plugin-cache"

let empty_string = function "" -> true | _ -> false

let starts_with_slash s =
  not (empty_string s)
  && (match (String.get s 0) with | '/' -> true | _-> false)

module Root = struct

  let r = ref None

  let set ~dir =
    match !r with
    | Some _ -> failwith "Path.Root.set - called more than once"
    | None -> r := Some dir

  let jenga_root_exists_in ~dir =
    match Sys.file_exists (dir ^/ Init.jenga_root_basename) with
    | `No | `Unknown -> false
    | `Yes -> true

  let discover () =
    let start_dir = Core.Std.Sys.getcwd() in
    let rec loop dir =
      if jenga_root_exists_in ~dir
      then (set ~dir; `ok)
      else
        if String.equal dir Filename.root
        then `cant_find_root
        else loop (Filename.dirname dir)
    in
    loop start_dir

  let get () =
    match !r with
    | None -> failwith "Path.Root.get - called before discover/set"
    | Some v -> v

end

(* The string representations for root-relative and absolute paths are disjoint:
   Absolute strings start with a /
   Relative strings dont start with a /, or are empty (the root) *)

module Rel = struct

  module Inner : sig

    type t with sexp, compare, bin_io
    include Hashable_binable with type t := t
    val unpack : t -> string
    val the_root : t
    val extend : t -> seg:string -> t
    val split : t -> t * string

  end = struct

    module T = struct
      type t = string with compare, sexp, bin_io
      let hash t = String.hash t
    end

    include T
    include Hashable.Make_binable(T)

    let create s = assert (match s with "." -> false | _ -> true); s

    let unpack t = t

    let the_root = create ""

    let extend t ~seg =
      assert (match seg with "." | ".." -> false | _ -> true);
      assert (not (String.contains seg '/'));
      let res = match unpack t with | "" -> seg | dir -> dir ^ "/" ^ seg in
      create res

    let split t =
      let dir,base = Filename.split (unpack t) in
      let dir = match dir with | "." -> the_root | _ -> create dir in
      dir, base

  end

  include Inner

  let equal t1 t2 = String.equal (unpack t1) (unpack t2)

  let is_root t = match (unpack t) with "" -> true | _ -> false

  let dirname t = fst (split t)
  let basename t = snd (split t)

  let relative_seg ~dir ~seg =
    match seg with
    | "" -> dir (* "foo//bar" == "foo/bar" *)
    | "." -> dir
    | ".." ->
      if is_root dir
      then failwithf "Path.relative, cant .. the_root" ()
      else dirname dir
    | _ -> extend dir ~seg

  let relative ~dir s =
    if starts_with_slash s then failwithf "Path.relative, starts with / - %s" s ();
    let segs = String.split s ~on:'/' in
    List.fold segs ~init:dir ~f:(fun dir seg -> relative_seg ~dir ~seg)

  let create s = relative ~dir:the_root s

  let create_from_absolute s =
    let root = Root.get() in
    let root_slash = root ^ "/" in
    if String.equal s root then Some the_root else
      match String.chop_prefix s ~prefix:root_slash with
      | Some x -> Some (create x)
      | None -> None

  let root_relative = create

  (* compute relative ".."-based-path-string path to reach [path] from [dir] *)
  let dotdot ~dir path =
    if is_root dir then unpack path else
    let segs = String.split (unpack dir) ~on:'/' in
    let dots = List.map segs ~f:(fun seg -> assert (not (String.(seg=".."))); "../") in
    String.concat dots ^ (unpack path)

  let to_string t =
    (* Special case for root. Represented as ""; Displayed as "." *)
    if is_root t then "." else unpack t

  let to_absolute_string t =
    if is_root t
    then Root.get()
    else Root.get() ^ "/" ^ (unpack t)

  let is_special_jenga_path t =
    String.is_prefix ~prefix:special_prefix (to_string t)

end

module Abs : sig

  type t with sexp, sexp, compare, bin_io
  val create : string -> t
  val to_string : t -> string

end = struct

  type t = string with sexp, compare, bin_io

  let create x =
    if not (starts_with_slash x)
    then failwithf "Path.Abs.create, doesn't start with / - %s" x ()
    else x

  let to_string t = t

end

module X = struct

  module T = struct
    type t = string with sexp, compare, bin_io
    let hash = String.hash
  end

  include T
  include Hashable.Make_binable(T)

  let of_relative x = Rel.to_string x
  let of_absolute x = Abs.to_string x

  let case t =
    if starts_with_slash t
    then `absolute (Abs.create t)
    else `relative (Rel.create t)

  let split t = Filename.split t

  let dirname t = fst (split t)
  let basename t = snd (split t)

  let to_string t =
    match (case t) with
    | `relative p -> Rel.to_string p
    | `absolute a -> Abs.to_string a

  let to_absolute_string t =
    match (case t) with
    | `relative p -> Rel.to_absolute_string p
    | `absolute a -> Abs.to_string a

end

include Rel
