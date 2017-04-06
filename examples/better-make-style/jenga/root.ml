
open Core
open Async
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) t f = Dep.bind t ~f
let relative = Path.relative

let bash ~dir command =
  Action.process ~dir ~prog:"bash" ~args:["-c"; command] ()

let slice s a b = if a = b then "" else String.slice s a b

let expand_dollar_vars ~lookup s =
  let max = String.length s in
  let rec loop ~chunks ~last i =
    if i = max then String.concat (List.rev (slice s last i :: chunks)) else
      let c,i = s.[i], i+1 in
      match c with
      | '$' ->
        let c,i = s.[i], i+1 in
        let var_name = String.make 1 c in
        begin match lookup var_name with
        | None -> loop ~chunks ~last i
        | Some replacement ->
          let before = slice s last (i-2) in (* loose the '$' and var_name *)
          let chunks = replacement :: before :: chunks in
          loop ~chunks ~last:i i
        end
      | _ -> loop ~chunks ~last i
  in
  loop ~chunks:[] ~last:0 0

let is_literal s = not (String.contains s '*') (* This will do for the example *)

let mk_rules ~dir ~targets ~deps ~command =
  let targets = List.map targets ~f:(relative ~dir) in [
    Rule.create ~targets (
      let rec loop acc_globs acc = function
        | [] -> return (List.rev acc, acc_globs)
        | dep_string::xs ->
          if is_literal dep_string
          then
            let path = Path.relative ~dir dep_string in
            loop acc_globs (path :: acc) xs
          else
            let glob = Glob.create ~dir dep_string in
            Dep.glob_listing glob *>>= fun paths ->
            loop (glob :: acc_globs) (paths @ acc) xs
      in
    (* convert user deps allowing glob patterns into known deps *)
      loop [] [] deps *>>= fun (deps,globs) ->
    (* and use the paths when expanding the $< an $^ *)
      let lookup =
        let targets = List.map targets ~f:(Path.reach_from ~dir) in
        let deps = List.map deps ~f:(Path.reach_from ~dir) in
        function
        | "@" -> Some (String.concat ~sep:" " targets)
        | "<" -> Some (match deps with [] -> "" | dep1::_ -> dep1)
        | "^" -> Some (String.concat ~sep:" " deps)
        | _ -> None
      in
      let command = expand_dollar_vars ~lookup command in
      let action =  bash ~dir (String.strip command) in
      Dep.all_unit (List.map globs ~f:Dep.glob_change) *>>= fun () ->
      Dep.all_unit (List.map deps ~f:Dep.path) *>>| fun () ->
      action
    )
  ]

let expand_percents ~dir ~targets ~deps ~command =
  match
    List.exists targets ~f:(fun s -> String.contains s '%'),
    List.filter deps ~f:(fun s -> String.contains s '%')
  with
  | false,[] -> (* rules without % *)
    Scheme.rules (mk_rules ~dir ~targets ~deps ~command)

  | _,_::_::_ -> failwith "multiple deps contain %"
  | true,[] -> failwith "% in target, but not in deps"
  | _, [dep] ->
    (* single % in deps, can be any number in target *)
    match String.split dep ~on:'%' with
    | [] -> assert false
    | [_] -> assert false
    | _::_::_::_ -> failwith "multiple % within dep"
    | [prefix;suffix] ->
      let unpack s =
        let s = String.chop_prefix_exn s ~prefix in
        String.chop_suffix_exn s ~suffix
      in
      let glob = Glob.create ~dir (prefix^"*"^suffix) in
      Scheme.dep (
        Dep.glob_listing glob *>>| fun expansions ->
        Scheme.rules (
          List.concat_map expansions ~f:(fun expansion ->
            let expansion = Path.reach_from ~dir expansion in
            let expand path =
              match String.split path ~on:'%' with
              | [] -> assert false
              | x::xs ->
                List.fold xs ~init:x ~f:(fun acc x -> acc ^ unpack expansion ^ x)
            in
            let targets = List.map targets ~f:expand in
            let deps = List.map deps ~f:expand in
            mk_rules ~dir ~targets ~deps ~command
          )))

let uncomment s =
  match String.lsplit2 s ~on:'#' with None -> s | Some (s,_comment) -> s

let non_blank s =
  match String.strip s with "" -> false | _ -> true

let split_into_non_blank_lines string =
  List.filter ~f:non_blank (List.map ~f:uncomment (String.split ~on:'\n' string))

let split_into_words string =
  List.filter ~f:non_blank (String.split ~on:'\ ' string)

let parse_simple_make_format conf contents =
  let dir = Path.dirname conf in
  let err n mes = failwithf "%s, rule# %d : %s"  (Path.to_string conf) n mes () in
  let rec loop n ~acc_schemes = function
    | [_] -> err n "expected header/command line-pairs"
    | [] -> Scheme.all acc_schemes
    | header::command::rest ->
      let targets,deps =
        match String.split header ~on:':' with
        | [before;after] -> split_into_words before, split_into_words after
        | _ -> err n "expected exactly one ':' in header line"
      in
      let scheme = expand_percents ~dir ~targets ~deps ~command in
      loop (n+1) rest ~acc_schemes:(scheme :: acc_schemes)
  in
  loop 1 ~acc_schemes:[] (split_into_non_blank_lines contents)

let user_rules_scheme ~dir =
  let conf = relative ~dir "make.conf" in
  Scheme.dep (
    Dep.file_exists conf *>>| function
    | false -> Scheme.rules []
    | true ->
      Scheme.contents conf (fun string ->
        parse_simple_make_format conf string))

let alias_default ~dir = Alias.create ~dir "DEFAULT"

let makefile_basename = "Makefile.extracted-from-jenga"

let default_all_buildable_and_extract ~dir =
  let makefile = relative ~dir makefile_basename in
  Scheme.rules [
    Rule.default ~dir [
      Dep.buildable_targets ~dir *>>= fun all ->
      let all = List.filter all ~f:(fun r -> r <> makefile) in
      Dep.all_unit (
        List.map all ~f:Dep.path
      )
    ];
    Extract_makefile.extract ~from:(alias_default ~dir) ~makefile;
  ]

let recusive_default_scheme ~dir =
  Scheme.rules [
    Rule.default ~dir [
      Dep.subdirs ~dir *>>= fun subs ->
      Dep.all_unit (
        List.map subs ~f:(fun sub -> Dep.alias (alias_default ~dir:sub)))
    ]]

let scheme ~dir =
  Scheme.all [
    user_rules_scheme ~dir;
    default_all_buildable_and_extract ~dir;
    recusive_default_scheme ~dir;
  ]

let env = Env.create (fun ~dir -> { scheme = scheme ~dir; directories_generated_from = None })
let setup () = Deferred.return env
