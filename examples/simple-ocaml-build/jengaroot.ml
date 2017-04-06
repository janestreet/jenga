
open Core
open Async
open Jenga_lib.Api
let ( *>>| ) = Dep.map
let ( *>>= ) t f = Dep.bind t ~f
let relative = Path.relative
let basename = Path.basename
let depend xs = Dep.all_unit (List.map xs ~f:Dep.path)

let bash ~dir command_string = Action.process ~dir ~prog:"bash" ~args:["-c"; command_string] ()
let bashf ~dir fmt = ksprintf (fun str -> bash ~dir str) fmt

let read_sexp ~t_of_sexp string =
  Sexp.of_string_conv_exn string t_of_sexp

let recusive_default_scheme ~dir =
  Scheme.rules [
    Rule.default ~dir [
      Dep.subdirs ~dir *>>= fun subs ->
      Dep.all_unit (
        List.map subs ~f:(fun sub -> Dep.alias (Alias.create ~dir:sub "DEFAULT")))
    ]]

let non_blank s =
  match String.strip s with "" -> false | _ -> true

let split_into_lines string =
  List.filter ~f:non_blank (String.split ~on:'\n' string)

let split_into_words string =
  List.filter ~f:non_blank (String.split ~on:'\ ' string)

let parse_line line =
  let err s = failwith (line ^ " -- " ^ s) in
  match String.split line ~on:':' with
  | [before;after] ->
    (match (split_into_words before) with [target] -> target | _ ->
      err "expected exactly one word before ':' in ocamldep output line"
    ),
    split_into_words after
  | _ ->
    err "expected exactly one ':' in ocamldep output line"

let glob_ml = Glob.create "*.ml"
let glob_mli = Glob.create "*.mli"

let ocamldep ~dir ~source ~target =
  Dep.action_stdout (
    Dep.all_unit [
      Dep.glob_change (glob_ml ~dir);
      Dep.glob_change (glob_mli ~dir);
      Dep.path source;
    ] *>>| fun () ->
    bashf ~dir "ocamldep -native %s" (basename source)
  )
  *>>= fun string ->
  let dd =
    List.map (split_into_lines string) ~f:(fun line ->
      let target,deps = parse_line line in
      relative ~dir target, List.map deps ~f:(relative ~dir)
    )
  in
  match List.Assoc.find dd ~equal:Poly.equal target with
  | None -> failwithf "lookup: %s" (Path.to_string target) ()
  | Some xs -> depend xs

let compile_ml ~dir name =
  let p x = relative ~dir (name ^ x) in
  let cmi = p".cmi" in
  let cmx = p".cmx" in
  let o = p".o" in
  Rule.create ~targets:[cmi;cmx;o] (
    let ml = p".ml" in
    Dep.path ml *>>= fun ()->
    ocamldep ~dir ~source:ml ~target:cmx *>>| fun () ->
    bashf ~dir "ocamlopt -c %s" (basename ml)
  )

let compile_ml_mli ~dir name =
  let p x = relative ~dir (name ^ x) in
  let cmi = p".cmi" in
  let cmx = p".cmx" in
  let o = p".o" in
  [
    Rule.create ~targets:[cmx;o] (
      let ml = p".ml" in
      Dep.path ml *>>= fun ()->
      ocamldep ~dir ~source:ml ~target:cmx *>>| fun () ->
      bashf ~dir "ocamlopt -c %s" (basename ml)
    );
    Rule.create ~targets:[cmi] (
      let mli = p".mli" in
      Dep.path mli *>>= fun ()->
      ocamldep ~dir ~source:mli ~target:cmi *>>| fun () ->
      bashf ~dir "ocamlopt -c %s" (basename mli)
    );
  ]

let compile_rules ~dir =
  Scheme.rules_dep (
    Dep.glob_listing (glob_ml ~dir)  *>>= fun mls ->
    Dep.glob_listing (glob_mli ~dir) *>>| fun mlis ->
    let exists_mli x = List.mem mlis (relative ~dir (x ^ ".mli")) ~equal:Path.equal in
    List.concat_map mls ~f:(fun ml ->
      let name = String.chop_suffix_exn (basename ml) ~suffix:".ml" in
      let b = exists_mli name in
      if b
      then compile_ml_mli ~dir name
      else [compile_ml ~dir name]
    )
  )

module Config = struct

  type t = {
    exe_name : string;
    (* depend on configuration for link order - but this could be calculated! *)
    link_order : string list;
  } [@@deriving sexp, fields]

  let read x = read_sexp ~t_of_sexp x

end

let link_rule ~dir config =
  let suffixed x name = relative ~dir (name ^ x) in
  let exe = suffixed".exe" (Config.exe_name config) in
  let link_names_in_order = Config.link_order config in
  let cmxs = List.map link_names_in_order ~f:(suffixed".cmx") in
  let os   = List.map link_names_in_order ~f:(suffixed".o") in
  Rule.create ~targets:[exe] (
    depend (cmxs @ os) *>>| fun () ->
    bashf ~dir "ocamlopt.opt %s -o %s"
      (String.concat ~sep:" " (List.map ~f:basename cmxs))
      (basename exe)
  )

let default_exe_rule ~dir config =
  let suffixed x name = relative ~dir (name ^ x) in
  Rule.default ~dir [
    depend [suffixed".exe" (Config.exe_name config)]]

let ocaml_rules ~dir =
  let config_path = Path.relative ~dir "config.sexp" in
  Scheme.contents config_path (fun string ->
    let config = Config.read string in
    Scheme.all [
      compile_rules ~dir;
      Scheme.rules [
        link_rule ~dir config;
        default_exe_rule ~dir config;
      ]
    ]
  )

let scheme ~dir =
  Scheme.all [
    recusive_default_scheme ~dir;
    (if Path.the_root = dir then Scheme.empty else Scheme.all [
      (* only setup ocaml build rules in subdirs *)
      ocaml_rules ~dir;
     ])]

let env = Env.create (fun ~dir -> { scheme = scheme ~dir; directories_generated_from = None })
let setup () = Async.Deferred.return env
