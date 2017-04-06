
open Core
open Async
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) t f = Dep.bind t ~f

let simple_rule ~targets ~deps ~action =
  Rule.create ~targets (
    Dep.all_unit deps *>>| fun () ->
    action)

let bash ~dir command =
  Action.process ~dir ~prog:"bash" ~args:["-c"; command] ()

let uncomment s =
  match String.lsplit2 s ~on:'#' with None -> s | Some (s,_comment) -> s

let non_blank s =
  match String.strip s with "" -> false | _ -> true

let split_into_non_blank_lines string =
  List.filter ~f:non_blank (List.map ~f:uncomment (String.split ~on:'\n' string))

let split_into_words string =
  List.filter ~f:non_blank (String.split ~on:'\ ' string)

let parse_simple_make_format path contents =
  (* Construct a set of rules from the [contents] of [path]
     where static rule triples are given in a simple make-stle format..

        target1 target2 : dep1 dep2 dep3  # comments!
            just-one-action
  *)
  let dir = Path.dirname path in
  let file s = Path.relative ~dir s in
  let need s = Dep.path (file s) in
  let err n mes = failwithf "%s, rule# %d : %s"  (Path.to_string path) n mes () in
  let rec loop n ~acc_targets ~acc_rules = function
    | [_] -> err n "expected header/command line-pairs"
    | [] ->
      Rule.default ~dir (List.map ~f:need acc_targets)
      :: acc_rules
    | header::command::rest ->
      let targets,deps =
        match String.split header ~on:':' with
        | [before;after] -> split_into_words before, split_into_words after
        | _ -> err n "expected exactly one ':' in header line"
      in
      let rule =
        simple_rule
          ~targets: (List.map ~f:file targets)
          ~deps:    (List.map ~f:need deps)
          ~action:  (bash ~dir (String.strip command))
      in
      loop (n+1) rest
        ~acc_targets:(targets @ acc_targets)
        ~acc_rules:(rule :: acc_rules)
  in
  return (loop 1 ~acc_targets:[] ~acc_rules:[] (split_into_non_blank_lines contents))

let make_style_rules path =
  Dep.file_exists path *>>= function
  | true -> Dep.contents path *>>= parse_simple_make_format path
  | false -> return []

let recusive_default ~dir =
  Dep.subdirs ~dir *>>| fun subs -> [
  Rule.default ~dir (
    List.map subs ~f:(fun sub -> Dep.alias (Alias.create ~dir:sub "DEFAULT")))]

let scheme ~dir =
  Scheme.all
    [ Scheme.sources [Path.relative ~dir "make.conf"]
    ; Scheme.rules_dep (
        let path = Path.relative ~dir "make.conf" in
        Dep.all [
          make_style_rules path;
          recusive_default ~dir;
        ] *>>| List.concat)
    ]

let env = Env.create (fun ~dir -> { scheme = scheme ~dir; directories_generated_from = None })
let setup () = Deferred.return env
