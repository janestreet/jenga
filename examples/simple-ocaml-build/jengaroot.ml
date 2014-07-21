
open Core.Std
open Async.Std
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind
let relative = Path.relative
let basename = Path.basename
let depend xs = Dep.all_unit (List.map xs ~f:Dep.path)

let bash ~dir command_string = Action.shell ~dir ~prog:"bash" ~args:["-c"; command_string]
let bashf ~dir fmt = ksprintf (fun str -> bash ~dir str) fmt

let read_sexp ~t_of_sexp path =
  Dep.contents path *>>= fun string ->
  Dep.deferred (fun () ->
    let info = Info.of_string ("sexp_of_string") in
    let pipe = Pipe.init (fun writer -> Pipe.write writer string) in
    Reader.of_pipe info pipe >>= fun reader ->
    Reader.read_sexp reader >>= fun outcome ->
    Reader.close reader >>| fun () ->
    match outcome with
    | `Eof -> failwith "read_sexp"
    | `Ok sexp -> t_of_sexp sexp
  )

let recusive_default ~dir =
  Dep.subdirs ~dir *>>| fun subs -> [
  Rule.default ~dir (
    List.map subs ~f:(fun sub -> Dep.alias (Alias.create ~dir:sub "DEFAULT")))]

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
  match List.Assoc.find dd target with
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
  Dep.glob_listing (glob_ml ~dir)  *>>= fun mls ->
  Dep.glob_listing (glob_mli ~dir) *>>| fun mlis ->
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
    depend (cmxs @ os) *>>| fun () ->
    bashf ~dir "ocamlopt.opt %s -o %s"
      (String.concat ~sep:" " (List.map ~f:basename cmxs))
      (basename exe)
  )

let keep_target ~dir path =
  Path.is_descendant ~dir path

let wrap_relative_cd_from ~dir path s =
  if dir = path then s else
    let relative_cd = Path.reach_from ~dir path in
    sprintf "(cd %s; %s)" relative_cd s

let format_paths ~dir paths =
  let paths = List.filter paths ~f:(keep_target ~dir) in
  String.concat ~sep:" " (List.map paths ~f:(Path.reach_from ~dir))

let format_makefile ~dir ~roots trips =
  sprintf "\n.PHONY: all\nall : %s\n\n%s\n"
    (format_paths ~dir roots)
    (String.concat ~sep:"\n\n" (List.map trips ~f:(fun trip ->
      let {Reflected.Trip.deps;targets;action} = trip in
      sprintf "%s : %s\n\t%s"
        (format_paths ~dir targets)
        (format_paths ~dir deps)
        (wrap_relative_cd_from ~dir (Reflected.Action.dir action)
           (Reflected.Action.string_for_one_line_make_recipe action)))))

let collect_local_dir_reachable_trips ~dir ~roots =
  Reflect.reachable ~keep:(keep_target ~dir) roots

let makefile_gen ~dir =
  let target = relative ~dir "Makefile.gen" in
  Rule.create ~targets:[target] (
    Reflect.alias (Alias.create ~dir "DEFAULT") *>>= fun roots ->
    let roots = List.filter roots ~f:(fun r -> not (r = target)) in
    collect_local_dir_reachable_trips ~dir ~roots *>>| fun trips ->
    Action.save ~target (format_makefile ~dir ~roots trips))

(* a meta Makefile rule: check action quoting for Makefile works! *)
let makefile_gen_gen ~dir =
  let target = relative ~dir "Makefile.gen.gen" in
  Rule.create ~targets:[target] (
    let roots = [relative ~dir "Makefile.gen"] in
    collect_local_dir_reachable_trips ~dir ~roots *>>| fun trips ->
    Action.save ~target (format_makefile ~dir ~roots trips))

let default_exe_rule ~dir =
  let suffixed x name = relative ~dir (name ^ x) in
  Rule.default ~dir [
    depend [relative ~dir "Makefile.gen"];
    Config.read (config_path ~dir) *>>= fun config ->
    depend [suffixed".exe" (Config.exe_name config)]]

let link_rule_and_default_exe ~dir =
  Dep.file_exists (config_path ~dir) *>>= function
  | false -> return []
  | true ->
    link_rule ~dir *>>| fun link_rule ->
    [
      link_rule;
      default_exe_rule ~dir;
      makefile_gen ~dir;
      makefile_gen_gen ~dir;
    ]

let scheme =
  Scheme.create ~tag:"the-scheme" (fun ~dir ->
    (* only setup ocaml build rules in subdirs *)
    if Path.the_root = dir
    then
      Dep.all [
        recusive_default ~dir;
        return [
          makefile_gen ~dir;
          makefile_gen_gen ~dir;
        ]
      ] *>>| List.concat
    else
      Dep.all [
        compile_rules ~dir;
        link_rule_and_default_exe ~dir;
        recusive_default ~dir;
      ] *>>| List.concat
  )

let env = Env.create ["**config.sexp",None; "**",Some scheme]
let setup () = Async.Std.Deferred.return env
