
open Core.Std
open Jenga_lib.Api_v2
let return = Depends.return
let ( *>>| ) = Depends.map
let ( *>>= ) = Depends.bind
let relative = Path.relative
let basename = Path.basename

let need = Depends.path
let needs xs = Depends.all_unit (List.map xs ~f:Depends.path)

let glob_change g =
  Depends.glob g *>>| fun _ -> ()

let read_sexp ~t_of_sexp path =
  need path *>>= fun () ->
  Depends.deferred (fun () ->
    load_sexp_for_jenga t_of_sexp path
  )

let bash ~dir command =
  Action.shell ~dir ~prog:"bash" ~args:["-c"; command]

let file_exists path =
  (* simple version: wont work if basename contains glob-special chars *)
  Depends.glob (Glob.create ~dir:(Path.dirname path) (Path.basename path)) *>>| function
  | [] -> false | _::_ -> true

let recusive_default ~dir =
  Depends.subdirs ~dir *>>| fun subs -> [
  Rule.default ~dir (
    Depends.all_unit (
      List.map subs ~f:(fun sub -> Depends.alias (Alias.create ~dir:sub "DEFAULT"))))]

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
  Depends.action_stdout (
    Depends.all_unit [
      glob_change (glob_ml ~dir);
      glob_change (glob_mli ~dir);
      Depends.path source;
    ] *>>| fun () ->
    bash ~dir (sprintf "ocamldep -native %s" (basename source))
  )
  *>>= fun string ->
  let dd =
    List.map (split_into_lines string) ~f:(fun line ->
      let target,deps = parse_line line in
      relative ~dir target, List.map deps ~f:(relative ~dir)
    )
  in
  match List.Assoc.find dd target with
  | None -> failwithf "lookup: %s" (Path.to_string target) ()
  | Some xs -> needs xs

let compile_ml ~dir name =
  let p x = relative ~dir (name ^ x) in
  let cmi = p".cmi" in
  let cmx = p".cmx" in
  let o = p".o" in
  Rule.create ~targets:[cmi;cmx;o] (
    let ml = p".ml" in
    Depends.path ml *>>= fun ()->
    ocamldep ~dir ~source:ml ~target:cmx *>>= fun () ->
    return (bash ~dir (sprintf "ocamlopt -c %s" (basename ml)))
  )

let compile_ml_mli ~dir name =
  let p x = relative ~dir (name ^ x) in
  let cmi = p".cmi" in
  let cmx = p".cmx" in
  let o = p".o" in
  [
    Rule.create ~targets:[cmx;o] (
      let ml = p".ml" in
      Depends.path ml *>>= fun ()->
      ocamldep ~dir ~source:ml ~target:cmx *>>= fun () ->
      return (bash ~dir (sprintf "ocamlopt -c %s" (basename ml)))
    );
    Rule.create ~targets:[cmi] (
      let mli = p".mli" in
      Depends.path mli *>>= fun ()->
      ocamldep ~dir ~source:mli ~target:cmi *>>= fun () ->
      return (bash ~dir (sprintf "ocamlopt -c %s" (basename mli)))
    );
  ]

let compile_rules ~dir =
  Depends.glob (glob_ml ~dir)  *>>= fun mls ->
  Depends.glob (glob_mli ~dir) *>>| fun mlis ->
  let exists_mli x = List.mem mlis (relative ~dir (x ^ ".mli")) in
  List.concat_map mls ~f:(fun ml ->
    let name = String.chop_suffix_exn (basename ml) ~suffix:".ml" in
    let b = exists_mli name in
    if b
    then compile_ml_mli ~dir name
    else [compile_ml ~dir name]
  )

module Config = struct

  type t = {
    exe_name : string;
    (* depend on configuration for link order - but this could be calculated! *)
    link_order : string list;
  } with sexp, fields

  let read = read_sexp ~t_of_sexp

end

let config_path = Path.relative "config.sexp"

let link_rule ~dir =
  let suffixed x name = relative ~dir (name ^ x) in
  Config.read (config_path ~dir) *>>| fun config ->
  let exe = suffixed".exe" (Config.exe_name config) in
  let link_names_in_order = Config.link_order config in
  let cmxs = List.map link_names_in_order ~f:(suffixed".cmx") in
  let os   = List.map link_names_in_order ~f:(suffixed".o") in
  Rule.create ~targets:[exe] (
    needs (cmxs @ os) *>>| fun () ->
    bash ~dir (
      sprintf "ocamlopt.opt %s -o %s"
        (String.concat ~sep:" " (List.map ~f:basename cmxs))
        (basename exe)
    )
  )

let default_exe_rule ~dir =
  let suffixed x name = relative ~dir (name ^ x) in
  Rule.default ~dir (
    Config.read (config_path ~dir) *>>= fun config ->
    need (suffixed".exe" (Config.exe_name config)))

let link_rule_and_default_exe ~dir =
  file_exists (config_path ~dir) *>>= function
  | false -> return []
  | true ->
    link_rule ~dir *>>| fun link_rule ->
    [
      link_rule;
      default_exe_rule ~dir;
    ]

let scheme =
  Scheme.create ~tag:"the-scheme" (fun ~dir ->
    Generator.create (
      (* only setup ocaml build rules in subdirs *)
      if Path.the_root = dir
      then
        recusive_default ~dir
      else
        Depends.all [
          compile_rules ~dir;
          link_rule_and_default_exe ~dir;
          recusive_default ~dir;
        ] *>>| List.concat))

let env = Env.create ["**config.sexp",None; "**",Some scheme]
let setup () = Async.Std.Deferred.return env
