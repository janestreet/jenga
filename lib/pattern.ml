
open Core.Std
open! Int.Replace_polymorphic_compare

module T = struct

  type t = Literal of string | Glob of string | Regexp of string
  [@@deriving sexp, bin_io, compare]

  let hash = function
    | Literal s -> String.hash s
    | Glob s -> String.hash s
    | Regexp s -> String.hash s

end

include T
include Hashable.Make(T)

module Regexp : sig (* Predefined ocaml "Str" module *)
  type t
  val quote : string -> string
  val regexp : string -> t
  val pmatch : rex:t -> string -> bool
end = struct
  type t = Str.regexp
  let quote s = Str.quote s
  let regexp s = Str.regexp s
  let pmatch ~rex s =  Str.string_match rex s 0
end

(* Cache/share compiled Regexps for all patterns, using single global HT.
   Secondary benefit - values of type Pattern.t are ok for polymorphic =/compare
*)

let the_pat_cache : (t, Regexp.t) Hashtbl.t = Table.create()

let to_string t =
  match t with
  | Literal x -> sprintf "'%s'" x
  | Glob g -> g
  | Regexp r -> sprintf "%s[regexp]" r

let to_re_string t =
  match t with
  | Literal x -> Regexp.quote x
  | Regexp r -> r
  | Glob g -> Glob_to_re.convert_unanchored g

let to_anchored_re_string t = "^" ^ to_re_string t ^ "$"

let create pat =
  let () =
    match (Hashtbl.find the_pat_cache pat) with
    | Some _ -> ()
    | None ->
      let re_string = to_anchored_re_string pat in
      (*Message.message "Pattern.create: %s -> %s" (to_string pat) re_string;*)
      Hashtbl.add_exn the_pat_cache ~key:pat ~data:(Regexp.regexp re_string)
  in
  pat

let create_from_literal_string x = create (Literal x)
let create_from_glob_string g = create (Glob g)
let create_from_regexp_string r = create (Regexp r)

let t_of_sexp sexp = create (t_of_sexp sexp)

let to_regexp t =
  match (Hashtbl.find the_pat_cache t) with
  | Some regexp -> regexp
  | None -> assert false

let matches t string =
  let res = Regexp.pmatch ~rex:(to_regexp t) string in
  (*Message.message "matches: %s ~ %s -> %s" (to_string t) string
    (if res then "YES" else "no");*)
  res
