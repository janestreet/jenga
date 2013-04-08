
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module T = struct

  type t = Glob of string | Regexp of string
  with sexp, compare

  let hash = function
    | Glob s -> String.hash s
    | Regexp s -> String.hash s

end

include T
include Hashable.Make(T)

(* Cache/share Pcre.regexp compilation for all patterns, using single global HT.
   Secondary benefit - values of type Pattern.t are ok for polymorphic =/compare
   (but we dont take advantage of that anymore!)
*)
let the_pat_cache : (t, Pcre.regexp) Hashtbl.t = Table.create()

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
      Hashtbl.add_exn the_pat_cache ~key:pat ~data:(Pcre.regexp re_string)
  in
  pat

let create_from_glob_string g = create (Glob g)
let create_from_regexp_string r = create (Regexp r)

let t_of_sexp sexp = create (t_of_sexp sexp)

let to_pcre t =
  match (Hashtbl.find the_pat_cache t) with
  | Some pcre -> pcre
  | None -> assert false

let matches t string =
  let res = Pcre.pmatch ~rex:(to_pcre t) string in
  (*Message.message "matches: %s ~ %s -> %s" (to_string t) string
    (if res then "YES" else "no");*)
  res
