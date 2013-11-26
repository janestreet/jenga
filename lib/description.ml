
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Digest = Fs.Digest
module Glob = Fs.Glob

module Alias  = struct

  (* name of an alias target *)
  (* an id which associates to a set of deps
     - bit like an omake phony, but with no action *)

  module T = struct
    type t = {
      dir : Path.t;
      name : string;
    } with sexp, bin_io, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make(T)

  let create ~dir name = { dir; name; }
  let split t = t.dir, t.name
  let default ~dir = create ~dir "DEFAULT"

  let to_string t =
    if Path.equal t.dir Path.the_root
    then sprintf ".%s" t.name
    else sprintf "%s/.%s" (Path.to_string t.dir) t.name

  let directory t = t.dir

end

module Goal = struct

  module T = struct
    type t = Path of Path.t | Alias of Alias.t with sexp, bin_io, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make(T)

  let to_string = function
    | Path path -> Path.to_string path
    | Alias alias -> Alias.to_string alias

  let directory = function
    | Path path -> Path.dirname path
    | Alias alias -> Alias.directory alias

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
      | Some (rel_dir_string,base) ->
        match rel_dir_string with
        | "." -> dir,string
        | _ -> Path.relative ~dir rel_dir_string, base
    in
    match String.chop_prefix base ~prefix:"." with
    | None -> Path (Path.relative ~dir base)
    | Some after_dot -> Alias (Alias.create ~dir after_dot)

end

module Need = struct

  module T = struct
    type t = Jengaroot | Goal of Goal.t
    with sexp, bin_io, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make_binable(T)
  include Comparable.Make_binable(T)

  let goal x = Goal x
  let jengaroot = Jengaroot

  let to_string = function
    | Jengaroot -> Misc.jenga_root_basename
    | Goal x -> Goal.to_string x

end

module Xaction = struct

  type t = {
    dir : Path.t;
    prog : string;
    args : string list;
  } with sexp, bin_io, compare

  let shell ~dir ~prog ~args = { dir ; prog; args; }

  let need_quoting x = String.contains x ' '
  let quote_arg x = if need_quoting x then sprintf "'%s'" x else x
  let concat_args_quoting_spaces xs = String.concat ~sep:" " (List.map xs ~f:quote_arg)

  let to_string t = sprintf "(cd %s; %s %s)"
    (Path.to_string t.dir) t.prog (concat_args_quoting_spaces t.args)

end

module Iaction = struct

  type t = {
    tag : Sexp.t;
    func : (unit -> unit Deferred.t);
  } with fields

  let create ~tag ~func = { tag; func; }

end

module Action = struct

  type t = X of Xaction.t | I of Iaction.t

  let case = function
    | X x -> `xaction x
    | I i -> `iaction i

  let shell ~dir ~prog ~args = X (Xaction.shell ~dir ~prog ~args)
  let internal ~tag ~func = I (Iaction.create ~tag ~func)

end

module Depends = struct

  type _ t =
  | Return : 'a -> 'a t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | All : 'a t list -> 'a list t
  | Deferred : (unit -> 'a Deferred.t) -> 'a t
  | Path : Path.t -> unit t
  | Absolute : Path.Abs.t -> unit t
  | Alias : Alias.t -> unit t
  | Glob : Glob.t -> Path.t list t
  | Contents : Path.t -> string t
  | Contents_abs : Path.Abs.t -> string t
  | Stdout : Action.t t -> string t (* special -- arg nested in t gives scoping *)

  let return x = Return x
  let bind t f = Bind (t,f)

  let all ts = All ts

  let path p = Path p
  let absolute ~path = Absolute (Path.Abs.create path)
  let alias x = Alias x

  let action_stdout t = Stdout t
  let glob t = Glob t
  let deferred t = Deferred t

  let contents p = Contents p
  let contents_absolute ~path = Contents_abs (Path.Abs.create path)

  (* non primitive... *)

  let map t f = bind t (fun x -> return (f x))
  let all_unit ts = map (all ts) (fun (_:unit list) -> ())

  let ( *>>| ) = map

  let both : ('a t -> 'b t -> ('a * 'b) t) =
    fun a b ->
      all [
        (a *>>| fun a -> `a a);
        (b *>>| fun b -> `b b);
      ] *>>| function
      | [`a a; `b b] -> (a,b)
      | _ -> assert false

  let action a = action_stdout a *>>| fun (_:string) -> ()

  let subdirs ~dir =
    glob (Glob.create ~dir ~kinds:(Some [`Directory]) ~glob_string:"*")

  let file_exists path =
    glob (Glob.create_from_path ~kinds:None path) (* any kind *)
    *>>| function | [] -> false | _::_ -> true

  let read_sexp p =
    contents p *>>| fun s ->
    Sexp.scan_sexp (Lexing.from_string s)

  let read_sexps p =
    contents p *>>| fun s ->
    Sexp.scan_sexps (Lexing.from_string s)

end

module Target_rule = struct

  type t = {
    targets : Path.t list;
    action_depends : Action.t Depends.t
  }
  with fields

  let create ~targets action_depends =
    (* Sort targets on construction.
       This allows for better target-rule keyed caching, regarding as equivalent rules
       which differ only in the order of their targets/deps.
    *)
    let targets = List.sort ~cmp:Path.compare targets in
    {
      targets;
      action_depends;
    }

  let head_target_and_rest t =
    match t.targets with
    (* It is possible to construct a rule with an empty list of targets, but once rules
       have been indexed (by target), and a rule obtained by lookup, then we can sure the
       returned rule will have at least one target! *)
    | [] -> assert false
    | x::xs -> x,xs

  let head_target t = fst (head_target_and_rest t)

end

module Rule  = struct

  type t =
  | Target of Target_rule.t
  | Alias of Alias.t * unit Depends.t

  let targets = function
    | Target tr -> Target_rule.targets tr
    | Alias _ -> []

end

module Gen_key = struct

  module T = struct
    type t = {
      tag : string;
      dir : Path.t;
    } with sexp, bin_io, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make_binable(T)

  let create ~tag ~dir = { tag; dir; }
  let to_string t = sprintf "%s:%s" t.tag (Path.to_string t.dir)
end

module Rule_generator = struct

  type t = { rules : Rule.t list Depends.t } with fields

  let create rules = { rules }

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
    putenv:(string * string) list;
    command_lookup_path : [`Replace of string list | `Extend of string list] option;
    schemes : (Pattern.t * Rule_scheme.t option) list;
    build_begin : (unit -> unit Deferred.t);
    build_end : (unit -> unit Deferred.t);
  }

  let create
      ?(putenv=[])
      ?command_lookup_path
      ?(build_begin=(fun () -> Deferred.return ()))
      ?(build_end=(fun () -> Deferred.return ()))
      schemes =
    {
      putenv;
      command_lookup_path;
      schemes =
        List.map schemes ~f:(fun (string,scheme) ->
          Pattern.create_from_glob_string string, scheme
        );
      build_begin;
      build_end;
    }

end
