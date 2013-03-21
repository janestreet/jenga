
open Core.Std

module Pat : sig

  type t with sexp
  val to_string : t -> string
  val create_from_glob_string : string -> t
  val create_from_regexp_string : string -> t
  val to_pcre : t -> Pcre.regexp
  val compare : t -> t -> int

end = struct

  type t = [`glob_string of string | `regex_string of string] with sexp, compare

  let to_string t =
    match t with
    | `glob_string g -> g
    | `regex_string r -> sprintf "%s[regexp]" r

  let create_from_glob_string g = `glob_string g
  let create_from_regexp_string r = `regex_string r

  let to_pcre =
    (*let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in*)
    fun t ->
      (*let u = genU() in*)
      let re_string =
        match t with
        | `regex_string r -> r
        | `glob_string g -> Glob_to_re.convert g
      in
      (*Message.message "pat[%d]: %s -> %s" u (to_string t) re_string;*)
      Pcre.regexp re_string

end

module V2 = struct
  (* Cache/share Pcre.regexp compilation across all values of type Pat.t
     Using single global HT.
     Secondary benefit - values of type Pat.t are ok for polymorphic =/compare
  *)

  type t = {
    pat : Pat.t;
  }

  let the_pat_cache : (Pat.t, Pcre.regexp) Hashtbl.t = Hashtbl.Poly.create()

  let create pat =
    let () =
      match (Hashtbl.find the_pat_cache pat) with
      | Some _ -> ()
      | None -> Hashtbl.add_exn the_pat_cache ~key:pat ~data:(Pat.to_pcre pat)
    in
    { pat }

  let to_pcre t =
    match (Hashtbl.find the_pat_cache t.pat) with
    | Some pcre -> pcre
    | None -> assert false

  let sexp_of_t t = Pat.sexp_of_t t.pat
  let t_of_sexp sexp = create (Pat.t_of_sexp sexp)

  let compare t1 t2 = Pat.compare t1.pat t2.pat
  let to_string t = Pat.to_string t.pat
  let create_from_glob_string g = create (Pat.create_from_glob_string g)
  let create_from_regexp_string r = create (Pat.create_from_regexp_string r)
  let matches t string = Pcre.pmatch ~rex:(to_pcre t) string

end

include V2
