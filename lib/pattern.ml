
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module T = struct

  type t = Glob of string | Regexp of string
  with sexp, bin_io, compare

  let hash = function
    | Glob s -> String.hash s
    | Regexp s -> String.hash s

end

include T
include Hashable.Make(T)


(* Allow easy switching between different regexp implementations... *)

(* PCRE *)
(*module P_Regexp : sig
  type t
  val regexp : string -> t
  val pmatch : rex:t -> string -> bool
end = struct
  type t = Pcre.regexp
  let regexp s = Pcre.regexp s
  let pmatch ~rex s = Pcre.pmatch ~rex s
end*)

(* Predefined ocaml "Str" module *)
module S_Regexp : sig
  type t
  val regexp : string -> t
  val pmatch : rex:t -> string -> bool
end = struct
  type t = Str.regexp
  let regexp s = Str.regexp s
  let pmatch ~rex s =  Str.string_match rex s 0
end

(* for dev/test, compare two regexp implementations side-by-side  *)
(*module Compare_Regexp : sig
  type t
  val regexp : string -> t
  val pmatch : rex:t -> string -> bool
end = struct
  module R1 = P_Regexp
  module R2 = S_Regexp
  type t = string * R1.t * R2.t
  let regexp s = s, R1.regexp s, R2.regexp s
  let pmatch ~rex:(pat_string,r1,r2) s =
    let res1 = R1.pmatch ~rex:r1 s in
    let res2 = R2.pmatch ~rex:r2 s in
    if (not (Bool.(res1 = res2))) then (
      Message.message "matches: %s ~ %s -> %s / %s" pat_string s
        (if res1 then "YES" else "no")
        (if res2 then "YES" else "no");
    );
    assert (Bool.(res1 = res2));
    res1
end
*)

module Regexp = S_Regexp (* select "Str" regexps *)


(* Cache/share compiled Regexps for all patterns, using single global HT.
   Secondary benefit - values of type Pattern.t are ok for polymorphic =/compare
   (but we dont take advantage of that anymore!)
*)

let the_pat_cache : (t, Regexp.t) Hashtbl.t = Table.create()

let to_string t =
  match t with
  | Glob g -> g
  | Regexp r -> sprintf "%s[regexp]" r

let to_re_string t =
  match t with
  | Regexp r -> r
  | Glob g -> Glob_to_re.convert g

let create pat =
  let () =
    match (Hashtbl.find the_pat_cache pat) with
    | Some _ -> ()
    | None ->
      let re_string = to_re_string pat in
      (*Message.message "Pattern.create: %s -> %s" (to_string pat) re_string;*)
      Hashtbl.add_exn the_pat_cache ~key:pat ~data:(Regexp.regexp re_string)
  in
  pat

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
