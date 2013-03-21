
open Core.Std
open Async.Std

module Digest = Fs.Digest
module Glob = Fs.Glob

module Alias  = struct

  (* name of an alias target *)
  (* an id which associates to a set of deps
     - bit like an omake phony, but with no action *)

  type t = {
    dir : Path.t;
    name : string;
  } with sexp, compare

  let create ~dir name = { dir; name; }
  let split t = t.dir, t.name
  let default ~dir = create ~dir "DEFAULT"

  let to_string t =
    if t.dir = Path.the_root
    then t.name
    else sprintf "%s/.%s" (Path.to_rrr_string t.dir) t.name

  let directory t = t.dir

  let equal = (=)

end

module Scan_id = struct

  type t = Sexp.t with sexp, compare
  let of_sexp x = x
  let to_sexp x = x
  let to_string t = Sexp.to_string t

end

module Action_id = struct

  type t = Sexp.t with sexp
  let of_sexp x = x
  let to_sexp x = x
  let equal = (=)
  let to_string t = Sexp.to_string t

end

module Goal = struct

  type t = [ `path of Path.t | `alias of Alias.t ] with sexp, compare
  let path x = `path x
  let alias x = `alias x
  let case t = t

  let to_string = function
    | `path path -> Path.to_rrr_string path
    | `alias alias -> Alias.to_string alias

  let directory = function
    | `path path -> Path.dirname path
    | `alias alias -> Alias.directory alias

end

module Dep = struct

  module T = struct
    type t = [
    | `goal of Goal.t
    | `scan of t list * Scan_id.t
    | `glob of Glob.t
    | `null
    ]
    with sexp, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make(T)

  let case t = t
  let path path = `goal (Goal.path path)
  let alias alias = `goal (Goal.alias alias)
  let glob glob = `glob glob
  let scan1 ts id = `scan (ts,id)
  let scan ts sexp = `scan (ts,Scan_id.of_sexp sexp)
  let null = `null

  let rec to_string t =
    match t with
    | `goal goal -> Goal.to_string goal
    | `scan (deps,_) ->
      sprintf "scan: %s" (String.concat ~sep:" " (List.map deps ~f:to_string))
    | `glob glob -> sprintf "glob: %s" (Fs.Glob.to_string glob)
    | `null -> "null"

  let default ~dir = alias (Alias.default ~dir)

  let parse_string ~dir string = (* for command-line selection of  top-level demands *)
    (* syntax...
       foo             - target
       path/to/foo     - target
       .foo            - alias
       path/to/.foo    - alias
    *)
    let dir,base =
      match String.rsplit2 string ~on:'/' with
      | None -> dir, string
      | Some (rel_dir_string,base) -> Path.relative ~dir rel_dir_string, base
    in
    match String.chop_prefix base ~prefix:"." with
    | None ->  path (Path.relative ~dir base)
    | Some after_dot -> alias (Alias.create ~dir after_dot)

  let compare = compare (* polymorphic compare ok *)

end

module Xaction = struct

  type t = {
    dir : Path.t;
    prog : string;
    args : string list;
  } with sexp

  let shell ~dir ~prog ~args = { dir ; prog; args; }

  let need_quoting x = String.contains x ' '
  let quote_arg x = if need_quoting x then sprintf "'%s'" x else x
  let concat_args_quoting_spaces xs = String.concat ~sep:" " (List.map xs ~f:quote_arg)

  let to_string t = sprintf "%s %s" t.prog (concat_args_quoting_spaces t.args)

  let equal = (=)

end

module Action = struct

  type t = X of Xaction.t | I of Action_id.t with sexp

  let case = function
    | X x -> `xaction x
    | I x -> `id x
  let xaction x = X x
  let internal1 i = I i
  let internal sexp = I (Action_id.of_sexp sexp)
  let equal = (=)

  let shell ~dir ~prog ~args =
    xaction (Xaction.shell ~dir ~prog ~args)

  let to_string = function
    | X x -> Xaction.to_string x
    | I x -> sprintf "INTERNAL:%s"(Action_id.to_string x)

end

module Target_rule = struct

  type t = {
    targets : Path.t list;
    deps : Dep.t list;
    action : Action.t;
  }
  with sexp,fields

  let create ~targets ~deps ~action =
    (* Sort targets/deps on construction.
       This allows for better target-rule keyed caching, regarding as equivalent rules
       which differ only in the order of their targets/deps.
    *)
    {
      targets = List.sort ~cmp:Path.compare targets;
      deps = List.sort ~cmp:Dep.compare deps;
      action;
    }

  let triple t = t.targets, t.deps, t.action

  let to_action_string t =
    Action.to_string t.action

  let to_string t =
    sprintf "%s : %s : %s"
      (String.concat ~sep:" " (List.map t.targets ~f:Path.to_rrr_string))
      (String.concat ~sep:" " (List.map t.deps ~f:Dep.to_string))
      (Action.to_string t.action)

  let head_and_rest_targets t =
    match t.targets with
    (* It is possible to construct a rule with an empty list of targets, but once rules
       have been indexed (by target), and a rule obtained by lookup, then we can sure the
       returned rule will have at least one target! *)
    | [] -> assert false
    | x::xs -> x,xs

end

module Rule  = struct

  type t = [
  | `target of Target_rule.t
  | `alias of Alias.t * Dep.t list
  ]
  with sexp

  let create ~targets ~deps ~action = `target (Target_rule.create ~targets ~deps ~action)
  let alias alias deps = `alias (alias, deps)
  let default ~dir deps = `alias (Alias.default ~dir, deps)

  let targets = function
    | `target tr -> Target_rule.targets tr
    | `alias _ -> []

  let defines_alias_for a1 = function
    | `target _ -> false
    | `alias (a2,_) -> Alias.equal a1 a2

  let case t = t

  let to_string = function
    | `target tr -> Target_rule.to_string tr
    | `alias (a,deps) ->
      sprintf "%s = %s"
        (Alias.to_string a)
        (String.concat ~sep:" " (List.map deps ~f:Dep.to_string))

end

module Gen_key = struct
  type t = {
    tag : string;
    dir : Path.t;
  } with sexp
  let create ~tag ~dir = { tag; dir; }
  let to_string t = sprintf "%s:%s" t.tag (Path.to_rrr_string t.dir)
end

module Rule_generator = struct

  type t = {
    deps:Dep.t list;
    gen:(unit -> Rule.t list Deferred.t);
  }

  let create ~deps ~gen = { deps; gen; }

  let deps t = t.deps

  let gen t =
    t.gen() >>= fun rules ->
    return rules

end

module Rule_scheme = struct

  type t = {
    tag : string;
    body : (dir:Path.t -> Rule_generator.t) ref; (* ref for identity *)
  }
  with fields

  let create ~tag f = {tag; body = ref f;}

end


module Env = struct

  type t = {
    command_lookup_path : [`Replace of string list | `Extend of string list] option;
    action : Sexp.t -> unit Deferred.t;
    scan : Sexp.t -> Dep.t list Deferred.t;
    schemes : (Pattern.t * Rule_scheme.t option) list;
  }

  let k_assert_false = fun _ -> assert false

  let create ?command_lookup_path ?(action=k_assert_false) ?(scan=k_assert_false)
      schemes =
    {
      command_lookup_path;
      action;
      scan;
      schemes =
        List.map schemes ~f:(fun (string,scheme) ->
          Pattern.create_from_glob_string string, scheme
        );
    }

end
