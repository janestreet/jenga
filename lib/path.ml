
open Core
open! Int.Replace_polymorphic_compare

let starts_with_slash s =
  not (String.is_empty s)
  && (match s.[0] with '/' -> true | _ -> false)

module IPS = Interning.String(struct let who = "<path>" end)

let reach_from_common =
  let rec go = function
    | [], [] -> "."
    | [], fs -> "./" ^ String.concat ~sep:"/" fs
    | ds, [] -> String.concat ~sep:"/" (List.map ~f:(fun _ -> "..") ds)
    | ((d :: dt) as ds), ((f :: ft) as fs) ->
      if String.(=) d f
      then
        go (dt, ft)
      else
        String.concat ~sep:"/" (List.map ~f:(fun _ -> "..") ds @ fs)
  in
  fun ~dir path -> go (dir, path)

(* The string representations for root-relative and absolute paths are disjoint:
   - Absolute paths start with a /
   - Relative paths dont start with a /, or are empty (the root)
   In both cases, paths are canonicalized by removing all "" and "." and ".." components.
   The type of relative-or-absolute path is the flat union of absolute paths and relative
   paths, except that the root is represented as "." (instead of "" in relative paths).
*)

module Rel = struct

  module Inner : sig

    type t [@@deriving sexp, bin_io, hash, compare]
    include Hashable_binable with type t := t
    include Comparable_binable with type t := t
    val unpack : t -> string
    val create_from_path : string -> t
    val the_root : t
    val extend : t -> seg:string -> t
    val split : t -> t * string

  end = struct

    module T = IPS

    include T
    include Hashable.Make_binable(T)
    include Comparable.Make_binable(T)

    let create s = assert (match s with "." -> false | _ -> true); IPS.intern s

    let the_root = create ""

    let create_from_path = function
      | "." -> the_root
      | s -> IPS.intern s

    let unpack = IPS.extern

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
    | _ ->
      extend dir ~seg

  let of_parts_relative ~dir =
    List.fold ~init:dir ~f:(fun dir seg -> relative_seg ~dir ~seg)

  let of_parts = of_parts_relative ~dir:the_root

  let relative ~dir s =
    if starts_with_slash s then failwithf "Path.Rel.relative, starts with / - %s" s ();
    of_parts_relative ~dir (String.split s ~on:'/')

  let create s = relative ~dir:the_root s

  let parts path = match unpack path with
    | "" -> []
    | s -> String.split ~on:'/' s

  let%test_unit "parts 1" =
    [%test_result: string list]
      ~expect:[]
      (parts (create "."))

  let%test_unit "parts 2" =
    [%test_result: string list]
      ~expect:["foo"]
      (parts (create "foo"))

  let%test_unit "parts 3" =
    [%test_result: string list]
      ~expect:["foo"; "bar"]
      (parts (create "foo/bar"))

  let%test_unit "parts 4" =
    [%test_result: string list]
      ~expect:["bar"]
      (parts (create "foo/../bar"))

  let reach_from ~dir path =
    reach_from_common ~dir:(parts dir) (parts path)

  let%test_unit "reach_from 1" =
    [%test_result: string]
      ~expect:"../baz"
      (reach_from ~dir:(create "foo/bar") (create "foo/baz"))

  let%test_unit "reach_from 2" =
    [%test_result: string]
      ~expect:"."
      (reach_from ~dir:(create "foo/bar") (create "foo/bar"))

  let%test_unit "reach_from 2" =
    [%test_result: string]
      ~expect:"./baz"
      (reach_from ~dir:(create "foo/bar") (create "foo/bar/baz"))

  let%test_unit "reach_from 3" =
    [%test_result: string]
      ~expect:"../../bar/foo"
      (reach_from ~dir:(create "foo/bar") (create "bar/foo"))

  let to_string t =
    (* Special case for root. Represented as ""; Displayed as "." *)
    if is_root t then "." else unpack t

  let is_descendant ~dir p =
    dir = p
    || dir = the_root
    || String.is_prefix ~prefix:(unpack dir ^ "/") (unpack p)
end

module Abs : sig

  type t [@@deriving sexp_of, hash, compare, bin_io]
  val create : string -> t

  (** [of_canonical_string str] is a cheaper version of [create], for when [str] is
      already known to be in canonical form, probably because it comes from the output of
      [to_string]. *)
  val of_canonical_string : string -> t
  val to_string : t -> string
  val relative_seg : dir:t -> string -> t
  val relative : dir:t -> string -> t
  val unix_root : t
  val dirname : t -> t
  val basename : t -> string
  val split : t -> t * string
  val is_descendant : dir:t -> t -> bool
  val reach_from : dir:t -> t -> string

end = struct

  module T = IPS

  include T
  include Hashable.Make_binable(T)
  include Comparable.Make_binable(T)

  let unix_root = IPS.intern "/"

  let to_string = IPS.extern
  let of_canonical_string = IPS.intern

  let dirname t = IPS.intern (Filename.dirname (IPS.extern t))
  let basename t = Filename.basename (IPS.extern t)
  let split t =
    let dir, file = Filename.split (IPS.extern t) in
    IPS.intern dir, file

  let relative_seg ~dir seg =
    match seg with
    | "" -> dir (* "foo//bar" == "foo/bar" *)
    | "." -> dir
    | ".." -> dirname dir
    | _ -> IPS.intern (IPS.extern dir ^/ seg)

  let of_parts_relative ~dir =
    List.fold_left ~init:dir ~f:(fun dir seg -> relative_seg ~dir seg)

  let relative ~dir s =
    if starts_with_slash s then failwithf "Path.Abs.relative, starts with / - %s" s ();
    of_parts_relative ~dir (String.split s ~on:'/')

  let create s =
    if not (starts_with_slash s)
    then failwithf "Path.Abs.create, doesn't start with / - %s" s ()
    else of_parts_relative ~dir:unix_root (String.split s ~on:'/')

  let%test_unit "create 1" =
    [%test_result: string]
      ~expect:"/"
      (IPS.extern (create "/"))
  let%test_unit "create 2" =
    [%test_result: string]
      ~expect:"/"
      (IPS.extern (create "/.."))
  let%test_unit "create 3" =
    [%test_result: string]
      ~expect:"/foo"
      (IPS.extern (create "/./foo/bar/.."))

  let parts path = match (String.split ~on:'/' (IPS.extern path)) with
    | "" :: "" :: [] -> []
    | "" :: parts -> parts
    | [] -> failwith "bug in [split]"
    | _ :: _ -> failwith "Abs.t does not start with '/'"

  let reach_from ~dir path =
    reach_from_common ~dir:(parts dir) (parts path)

  let reach_descendant ~dir path =
    if dir = path then Some "."
    else
    if dir = unix_root
    then
      (* note that this is non-empty because not (dir = path) *)
      let res = String.chop_prefix ~prefix:"/" (extern path) in
      assert (Option.is_some res);
      res
    else
      String.chop_prefix ~prefix:(extern dir ^ "/") (extern path)

  let is_descendant ~dir path =
    dir = path || dir = unix_root ||
    String.is_prefix ~prefix:(extern dir ^ "/") (extern path)

  let test_descendant a b r1 r2 =
    let b1 = Option.is_some r1 in
    let b2 = Option.is_some r2 in
    [%test_result: bool]
      ~expect:b1
      (is_descendant ~dir:a b);
    [%test_result: bool]
      ~expect:b2
      (is_descendant ~dir:b a);
    [%test_result: string option]
      ~expect:r1
      (reach_descendant ~dir:a b);
    [%test_result: string option]
      ~expect:r2
      (reach_descendant ~dir:b a)

  let test_gt a b r =
    test_descendant a b (Some r) None

  let test_eq a b =
    test_descendant a b (Some ".") (Some ".")

  let test_unrelated a b =
    test_descendant a b None None

  let%test_unit "is_descendant 1" =
    test_gt unix_root (create "/foo/bar") "foo/bar"

  let%test_unit "is_descendant 2" =
    test_gt (create "/foo") (create "/foo/bar") "bar"

  let%test_unit "is_descendant 3" =
    test_eq (create "/foo/bar") (create "/foo/bar")

  let%test_unit "is_descendant 4" =
    test_unrelated (create "/foo/bar") (create "/foo/baz")

  let%test_unit "is_descendant 5" =
    test_eq (create "/foo/../bar") (create "/bar")

end

module Repo = struct

  let r = ref None

  let set_root dir =
    match !r with
    | Some _ -> failwith "Path.Root.set - called more than once"
    | None -> r := Some dir

  let root () =
    match !r with
    | None -> failwith "Path.Root.get - called before discover/set"
    | Some v -> v

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
  then `absolute (Abs.of_canonical_string s)
  else `relative (Rel.Inner.create_from_path s)

let is_absolute t = starts_with_slash (extern t)

let split t = Filename.split (extern t)

let dirname t = intern (fst (split t))
let basename t = snd (split t)
let split t =
  let a, b = split t in
  intern a, b

let to_string t =
  match (case t) with
  | `relative p -> Rel.to_string p
  | `absolute a -> Abs.to_string a

let to_absolute t =
  match (case t) with
  | `relative p ->
    Abs.relative ~dir:(Repo.root ()) (Rel.to_string p)
  | `absolute a ->
    a

let to_absolute_string t = Abs.to_string (to_absolute t)

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
      then
        failwith ".. from the root!"
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

let the_root = of_relative Rel.the_root

let unix_root = of_absolute Abs.unix_root

let root_relative = relative ~dir:the_root

let reach_from ~dir t =
  match (case dir, case t) with
  | `relative dir, `relative t -> Rel.reach_from ~dir t
  | `absolute dir, `absolute t -> Abs.reach_from ~dir t
  | `absolute _, `relative _ -> failwith "trying to reach relative from absolute!"
  | `relative _, `absolute p -> Abs.to_string p

let is_descendant ~dir path =
  match (case dir, case path) with
  | (`relative dir, `relative path) -> Rel.is_descendant ~dir path
  | `absolute dir, `absolute path -> Abs.is_descendant ~dir path
  | _, _ ->
    false

let of_absolute_relativizing path =
  let root = Repo.root () in
  match Abs.is_descendant ~dir:root path with
  | true -> of_relative (Rel.create (Abs.reach_from ~dir:root path))
  | false -> of_absolute path

let of_absolute_string s =
  of_absolute_relativizing (Abs.create s)

let is_a_root path = (=) (dirname path) path

let%test_unit "case" =
  match case (of_absolute (Abs.create "/./a")) with
  | `relative _ -> assert false
  | `absolute abs -> [%test_eq: string] (Abs.to_string abs) "/a"
