
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

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
    let exists name =
      match Sys.file_exists (dir ^/ name) with
      | `No | `Unknown -> false
      | `Yes -> true
    in
    exists Misc.jenga_root_basename ||
    exists Misc.jenga_conf_basename

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

module IPS = Interning.String(struct let who = "<path>" end)

(* The string representations for root-relative and absolute paths are disjoint:
   Absolute strings start with a /
   Relative strings dont start with a /, or are empty (the root) *)

module Rel = struct

  module Inner : sig

    type t with sexp, bin_io, compare
    include Hashable_binable with type t := t
    include Comparable_binable with type t := t
    val unpack : t -> string
    val the_root : t
    val extend : t -> seg:string -> t
    val split : t -> t * string

  end = struct

    module T = IPS

    include T
    include Hashable.Make_binable(T)
    include Comparable.Make_binable(T)

    let create s = assert (match s with "." -> false | _ -> true); IPS.intern s

    let unpack = IPS.extern

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
    if starts_with_slash s then failwithf "Path.Rel.relative, starts with / - %s" s ();
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
    (*if (dirname path = dir) then basename path else*)
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
    String.is_prefix ~prefix:Misc.special_prefix (to_string t)

end

module Abs : sig

  type t with sexp_of, compare, bin_io
  val create : string -> t
  val to_string : t -> string
  val relative_seg : dir:t -> string -> t
  val the_root : unit -> t
  val dirname : t -> t

end = struct

  type t = IPS.t
  with sexp_of, compare, bin_io

  let create x =
    if not (starts_with_slash x)
    then failwithf "Path.Abs.create, doesn't start with / - %s" x ()
    else IPS.intern x

  let to_string t = IPS.extern t

  let the_root () = IPS.intern (Root.get())

  let dirname t = IPS.intern (Filename.dirname (IPS.extern t))

  let relative_seg ~dir seg =
    match seg with
    | "" -> dir (* "foo//bar" == "foo/bar" *)
    | "." -> dir
    | ".." -> dirname dir
    | _ -> IPS.intern (IPS.extern dir ^/ seg)

end

module T = IPS

include T
include Hashable.Make_binable(T)
include Comparable.Make_binable(T)

let of_relative x = intern (Rel.to_string x)
let of_absolute x = intern (Abs.to_string x)

let case t =
  let s = IPS.extern t in
  if starts_with_slash s
  then `absolute (Abs.create s)
  else `relative (Rel.create s)

let is_absolute t = starts_with_slash (extern t)

let split t = Filename.split (extern t)

let dirname t = intern (fst (split t))
let basename t = snd (split t)

let to_string t =
  match (case t) with
  | `relative p -> Rel.to_string p
  | `absolute a -> Abs.to_string a

let to_absolute_string t =
  match (case t) with
  | `relative p -> Rel.to_absolute_string p
  | `absolute a -> Abs.to_string a

let absolute s = of_absolute (Abs.create s)

let relative_seg ~dir ~seg =
  match (case dir) with
  | `absolute dir -> of_absolute (Abs.relative_seg ~dir seg)
  | `relative dir ->
    match seg with
    | "" -> of_relative dir (* "foo//bar" == "foo/bar" *)
    | "." -> of_relative dir
    | ".." ->
      if Rel.is_root dir
      then of_absolute (Abs.dirname (Abs.the_root ()))
      else of_relative (Rel.dirname dir)
    | _ ->
      of_relative (Rel.extend dir ~seg)

let relative ~dir s =
  if starts_with_slash s then failwithf "Path.relative, starts with / - %s" s ();
  let segs = String.split s ~on:'/' in
  List.fold segs ~init:dir ~f:(fun dir seg -> relative_seg ~dir ~seg)

let relative_or_absolute ~dir s =
  if starts_with_slash s
  then absolute s (* dir ignored *)
  else relative ~dir s

let equal t1 t2 =
  String.(to_absolute_string t1 = to_absolute_string t2)

let the_root = of_relative Rel.the_root

let root_relative = relative ~dir:the_root

let dotdot ~dir t =
  match (case dir, case t) with
  | `relative dir, `relative t -> Rel.dotdot ~dir t
  | _,_ ->
    (* give up, just use absolute path *)
    to_absolute_string t


let is_descendant ~dir path =
  String.is_prefix ~prefix:(to_absolute_string dir^"/") (to_absolute_string path)

let reach_from ~dir path =
  if is_descendant ~dir path
  then String.chop_prefix_exn ~prefix:(to_absolute_string dir^"/") (to_absolute_string path)
  else dotdot ~dir path

let is_special_jenga_path path =
  match case path with
  | `relative rel -> Rel.is_special_jenga_path rel
  | `absolute _ -> false

let of_absolute_string s =
  match (Rel.create_from_absolute s) with
  | Some rel -> of_relative rel
  | None -> absolute s

let relativize_if_possible p =
  of_absolute_string (to_absolute_string p)
