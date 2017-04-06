
open Core
open Async

(* Dont open jenga API, but access via explicit [J.] *)
module J = Jenga_lib.Api
module Dep = J.Dep
let dirname = J.Path.dirname
let relative = J.Path.relative
let return = Dep.return
let ( *>>= ) t f = Dep.bind t ~f
let ( *>>| ) = Dep.map

let simple_default ~dir paths =
  J.Rule.default ~dir (
    List.map ~f:Dep.path paths
  )

let simple_rule ~dir ~targets ~deps action =
  let t_dir = (match targets with [] -> assert false | t1::_ -> dirname t1) in
  if dir = t_dir then [
    J.Rule.create ~targets (
      Dep.all_unit (List.map ~f:Dep.path deps) *>>| fun () ->
      action
    )
  ]
  else []

module Load_ninja (X : sig

  val dir : J.Path.t (* generating rules in this directory *)
  val build_dot_ninja : J.Path.t
  val contents : string

end) : sig

  val rules : J.Rule.t list

end = struct

  include X

  let ninja_root_dir = dirname build_dot_ninja

  let sh command_string =
    J.Action.process ~dir:ninja_root_dir ~prog:"sh" ~args:["-c"; command_string] ()

  module Pos : sig (* line/column number positions, for accurate error messages *)

    type t [@@deriving sexp]
    val start : t
    val next : char -> t -> t
    val column : t -> int
    val error : t -> string -> 'a

  end = struct

    type t = {
      line : int;
      col : int;
    } [@@deriving sexp]

    let column t = t.col

    let start = { line = 1; col = 0; }

    let to_string t = sprintf "[%d.%d]" t.line t.col

    let error t s =
      failwithf "%s %s : %s"
        (J.Path.to_string build_dot_ninja) (to_string t) s ()

    let next char t =
      let () =
        match char with
        | '\t' -> error t "unexpected tab char"
        | _ -> ()
      in
      match char with
      | '\n' -> { line = t.line+1; col = 0 }
      | _ -> { line = t.line; col = t.col+1 }

  end

  (* abstract-syntax tree of Ninja. Build by parser, consumed by evaluator *)
  module N = struct

    type var_name = string [@@deriving sexp]
    type rule_name = string [@@deriving sexp]

    type chunk =
    | Literal of string
    | Dollar_ref of Pos.t * var_name
    [@@deriving sexp]

    type text = { chunks : chunk list } [@@deriving sexp]

    module Bind = struct
      type t = {
        var_name: var_name;
        rhs: text;
      }
      [@@deriving sexp]
    end

    module Build_edge = struct
      type t = {
        outputs : text list;
        rule_name : rule_name;
        rule_name_pos : Pos.t;
        inputs : text list;
        implicit_deps : text list;
        order_only : text list;
        binds : Bind.t list;
      } [@@deriving sexp]
    end

    type dec_form =
    | Rule of rule_name * Bind.t list
    | Build_edge of Build_edge.t
    | Var_dec of Bind.t
    | Default of text list
    [@@deriving sexp]

    type dec = Pos.t * dec_form [@@deriving sexp]
    type decs = dec list [@@deriving sexp]

  end

  (* Lexer Location - The lexer supports: comments and escaped newlines *)
  module Loc : sig

    type t
    val position : t -> Pos.t
    val start : t
    val next : t -> (char * t) option
    val parse_error : t -> 'a

  end = struct

    let max = String.length X.contents

    type t = {
      index : int;
      pos : Pos.t;
    }

    let position t = t.pos
    let start = { index = 0; pos = Pos.start; }

    let consider_all_chars t =
      if t.index = max
      then None
      else
        let char = X.contents.[t.index] in
        let index = t.index + 1 in
        let pos = Pos.next char t.pos in
        let t2 = { index ; pos; } in
        Some (char, t2)

    let strip_comments ~next =
      let rec skip_until_eol t =
        match next t with
        | Some ('\n',t) -> Some ('\n',t)
        | None -> None
        | Some (_,t) -> skip_until_eol t
      in
      fun t ->
        match next t with
        | Some ('#',t) -> skip_until_eol t
        | x -> x

    let handle_escaped_newlines ~next =
      let rec skip_until_non_space t =
        match next t with
        | Some (' ',t) -> skip_until_non_space t
        | x -> x
      in
      fun t ->
        match next t with
        | Some ('$',t1) ->
          begin match next t1 with
          | Some ('\n',t2) -> skip_until_non_space t2
          | _ -> Some ('$',t1)
          end
        | x -> x

    let next =
      let next = consider_all_chars in
      (* This allows comments to be inserted between a continuation $ and NL *)
      let next = strip_comments ~next in
      let next = handle_escaped_newlines ~next in
      next

    let quote_char = function
      | '\n' -> sprintf "<newline>"
      |  c -> sprintf "'%c'" c

    let parse_error t =
      Pos.error t.pos (
        sprintf "parse error; unexpected %s"
          (match next t with | None -> "<eof>" | Some (c,_) -> quote_char c)
      )

  end

  module Par : sig (* simple parser combinators *)

    type 'a t
    val fail : 'a t
    val return : 'a -> 'a t
    val location : Loc.t t
    val satisfy : (char -> 'a option) -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val maybe : 'a t -> 'a option t
    val exec : 'a t -> 'a

  end = struct

    type 'a res =
    | Eps of 'a
    | Succ of 'a * Loc.t
    | Fail
    | Err of Loc.t

    type 'a t = Loc.t -> 'a res

    let fail = fun _loc -> Fail
    let return x = fun _loc -> Eps x
    let location = fun loc -> Eps loc

    let satisfy p =
      fun loc ->
        match Loc.next loc with
        | None -> Fail
        | Some (char,loc) ->
          match (p char) with
          | None -> Fail
          | Some res -> Succ (res, loc)

    let add_consume loc res =
      match res with
      | Fail -> Err loc
      | Err loc -> Err loc
      | Eps x -> Succ (x,loc)
      | Succ (x,loc) -> Succ (x,loc)

    let bind t f =
      fun loc ->
        match t loc with
        | Fail -> Fail
        | Err loc -> Err loc
        | Eps x -> (f x) loc
        | Succ (x,loc) -> add_consume loc ((f x) loc)

    let maybe t =
      fun loc ->
        match t loc with
        | Fail -> Eps None
        | Err loc -> Err loc
        | Eps x -> Eps (Some x)
        | Succ (x,loc) -> Succ (Some x, loc)

    let check_eof loc res =
      match Loc.next loc with
      | Some _ -> Loc.parse_error loc
      | None -> res

    let exec t =
      let loc = Loc.start in
      match t loc with
      | Fail            -> Loc.parse_error loc
      | Err loc         -> Loc.parse_error loc
      | Eps res         -> check_eof loc res
      | Succ (res,loc)  -> check_eof loc res

  end

  module Parser : sig (* parser for ninja.build files *)

    val ninja_build_file_syntax : N.dec list Par.t

  end = struct

    open Par
    let (>>=) = Par.bind
    let (>>|) p f = Par.bind p (fun x -> return (f x))

    let position = Par.location >>| Loc.position
    let column = position >>| Pos.column

    let sat pred =
      satisfy (fun x -> if pred x then Some x else None)

    let skip pred =
      satisfy (fun x -> if pred x then Some () else None)

    let char c = skip (fun c' -> Char.(c=c'))

    let many p =
      let rec loop acc =
        maybe p >>= function
        | None -> return (List.rev acc)
        | Some x -> loop (x::acc)
      in
      loop []

    let many1 p =
      p >>= fun x ->
      many p >>| fun xs ->
      x::xs

    let rec skipwhile p =
      maybe (skip p) >>= function
      | None -> return ()
      | Some () -> skipwhile p

    let whitespace = skipwhile (function | ' ' | '\n' -> true | _ -> false)
    let spaces = skipwhile (function | ' ' -> true | _ -> false)

    let nibble p =
      p >>= fun x ->
      spaces >>= fun () ->
      return x

    let is_simple_ident_char = function
      | '_' | '-' -> true
      | c ->
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')

    let is_ident_char = function
      | '.' -> true
      | c -> is_simple_ident_char c

    let simple_identifier = (* no '.' allowed *)
      many1 (sat is_simple_ident_char) >>= fun xs ->
      return (String.of_char_list xs)

    let identifier =
      many1 (sat is_ident_char) >>= fun xs ->
      return (String.of_char_list xs)

    let identifier_or_keyword =
      identifier >>= fun s ->
      return (
        match s with
        | "rule" -> `rule
        | "build" -> `build
        | "default" -> `default
        |  id -> `identifier id
      )

    let chunk pred =
      position >>= fun pos ->
      maybe (char '$') >>= function
      | Some () ->
        begin
          maybe (char '{') >>= function
          | Some () ->
            identifier >>= fun name ->
            char '}' >>= fun () ->
            return (N.Dollar_ref (pos,name))
          | None ->
            simple_identifier >>= fun name ->
            return (N.Dollar_ref (pos,name))
        end
      | None ->
        many1 (sat pred) >>= fun xs ->
        return (N.Literal (String.of_char_list xs))

    let not_newline_or_dollar = function | '$' | '\n' -> false | _ -> true
    let text_to_eol =
      many (chunk not_newline_or_dollar) >>| fun chunks -> {N.chunks}

    let is_path_char = function
      | '$' | '|' | ':' | ' ' | '\n' -> false | _ -> true

    let path =
      nibble (many1 (chunk is_path_char)) >>| fun chunks -> {N.chunks}

    let maybe_pipe_deps = (* TODO: Allow both "|" and "||" *)
      maybe (char '|') >>= function
      | None -> return ([],[])
      | Some () ->
        maybe (char '|') >>= function
        | Some () ->
          spaces >>= fun () ->
          many1 path >>= fun order_only ->
          return ([],order_only)
        | None ->
          spaces >>= fun () ->
          many1 path >>= fun implicit_deps ->
          return (implicit_deps,[])

    let binding =
      nibble identifier >>= fun var_name ->
      char '=' >>= fun () ->
      text_to_eol >>= fun rhs ->
      whitespace >>= fun () ->
      return {N.Bind. var_name; rhs}

    let bindings_indented_more_than n =
      whitespace >>= fun () ->
      many (
        column >>= fun i -> if i<=n then fail else
            binding
      )

    let dec_form =
      column >>= fun n ->
      nibble identifier_or_keyword >>= function
      | `default ->
        many1 path >>= fun targets ->
        return (N.Default targets)

      | `build ->
        many1 path >>= fun outputs ->
        nibble (char ':') >>= fun () ->
        position >>= fun rule_name_pos ->
        nibble identifier >>= fun rule_name ->
        many path >>= fun inputs ->
        maybe_pipe_deps >>= fun (implicit_deps,order_only) ->
        bindings_indented_more_than n >>= fun binds ->
        return (N.Build_edge {
          N.Build_edge.
          outputs;
          rule_name; rule_name_pos;
          inputs;
          implicit_deps;order_only;binds
        })

      | `rule ->
        identifier >>= fun rule_name ->
        bindings_indented_more_than n >>= fun binds ->
        return (N.Rule (rule_name,binds))

      | `identifier var_name->
        char '=' >>= fun () ->
        text_to_eol >>= fun rhs ->
        let bind = {N.Bind. var_name; rhs} in
        return (N.Var_dec bind)

    let dec =
      position >>= fun pos ->
      dec_form >>= fun form ->
      whitespace >>= fun () ->
      return (pos,form)

    let ninja_build_file_syntax =
      whitespace >>= fun () ->
      many dec

  end


  module Eval : sig

    val eval_file : N.dec list -> J.Rule.t list

  end = struct

    let to_jpath t =
      J.Path.relative ~dir:ninja_root_dir (String.strip t)

    module Env = struct
      type t = string String.Map.t
      let empty = String.Map.empty
      let single k v = String.Map.singleton k v
      let look pos t var_name =
        match String.Map.find t var_name with
        | Some v -> v
        | None -> Pos.error pos (sprintf "no binding for $%s" var_name)
    end

    module Rmacro = struct
      type t = (Env.t -> J.Action.t)
      let create f = f
      let apply t = t
    end

    module Renv = struct
      type t = Rmacro.t String.Map.t
      let empty = String.Map.empty
      let single k v = String.Map.singleton k v
      let look pos t rule_name =
        match String.Map.find t rule_name with
        | Some v -> v
        | None -> Pos.error pos (sprintf "no rule defined for `%s'" rule_name)

    end

    let ( ++ ) t1 t2 = (* right-biased environment merging *)
      String.Map.merge t1 t2
        ~f:(fun ~key:_ -> function | `Left x | `Right x | `Both (_,x) -> Some x)

    let eval_text : (Env.t -> N.text -> string) =
      fun env text ->
        String.concat (List.map text.N.chunks ~f:(function
        | N.Literal string -> string
        | N.Dollar_ref (pos,var_name) -> Env.look pos env var_name))

    let eval_texts : (Env.t -> N.text list -> string list) =
      fun env texts ->
        List.map texts ~f:(fun text -> eval_text env text)

    let eval_bind : (Env.t -> N.Bind.t -> Env.t) =
      fun env bind ->
        let {N.Bind.var_name;rhs} = bind in
        let atom = eval_text env rhs in
        Env.single var_name atom

    let rec eval_binds : (Env.t -> N.Bind.t list -> Env.t) =
      fun env -> function
      | [] -> env
      | bind::binds ->
        let env1 = eval_bind env bind in
        let env = env ++ env1 in
        eval_binds env binds

    let eval_dec : (Env.t -> Renv.t -> N.dec -> J.Rule.t list * Env.t * Renv.t) =
      fun env renv (dec_pos,dec_form) -> match dec_form with
      | N.Var_dec bind ->
        let env = eval_bind env bind in
        [], env, Renv.empty

      | N.Rule (rule_name,binds) ->
        let rmacro =
          Rmacro.create (fun env1 ->
            let env = env ++ env1 in
            let env = eval_binds env binds in
            let command = Env.look dec_pos env "command" in
            (* TODO: support scanner deps from file defined by $deps *)
            sh (String.strip command)

          )
        in
        let renv = Renv.single rule_name rmacro in
        [], Env.empty, renv

      | N.Build_edge {N.Build_edge.
                      outputs;
                      rule_name;rule_name_pos;
                      inputs;
                      binds;
                      implicit_deps;
                      order_only; (* TODO: how should these be treated? *)
                     } ->
        let rmacro = Renv.look rule_name_pos renv rule_name in
        let outputs = eval_texts env outputs in
        let inputs = eval_texts env inputs in
        let implicit_deps = eval_texts env implicit_deps in
        let order_only = eval_texts env order_only in
        let env =
          Env.single "out" (String.concat ~sep:" " outputs)
          ++ Env.single "in" (String.concat ~sep:" " inputs)
          ++ env
        in
        let env = eval_binds env binds in
        let action = Rmacro.apply rmacro env in
        let targets = List.map outputs ~f:to_jpath in
        let deps = List.map (inputs @ implicit_deps @ order_only) ~f:to_jpath in
        let jrules = simple_rule ~dir ~targets ~deps action in
        jrules, Env.empty, Renv.empty

      | N.Default targets ->
        let targets = eval_texts env targets in
        let paths = List.map targets ~f:to_jpath in
        let jrule = simple_default ~dir:ninja_root_dir paths in
        [jrule], Env.empty, Renv.empty

    let rec eval_decs :
        (J.Rule.t list -> Env.t -> Renv.t -> N.dec list -> J.Rule.t list) =
      fun jrules env renv -> function
      | [] -> jrules
      | dec::decs ->
        let jrules1,env1,renv1 = eval_dec env renv dec in
        let jrules = jrules1 @ jrules in (* order matters not *)
        let env = env ++ env1 in
        let renv = renv ++ renv1 in
        eval_decs jrules env renv decs

    let eval_file : (N.dec list -> J.Rule.t list) =
      fun decs ->
        let jrules = [] in
        let env = Env.single "configure_env" "CONFIGURE_ENV" in (*TODO*)
        let renv =
          Renv.single "phony" (*TODO*)
            (Rmacro.create (fun _env -> sh "phony-action"))
        in
        eval_decs jrules env renv decs

  end

  (* Sequence the parser; then the evaluation; resulting in a list of jenga rules*)
  let decs = Par.exec Parser.ninja_build_file_syntax
  let rules = Eval.eval_file decs

end

let find_build_dot_ninja_upwards_from =
  let rec loop ~dir =
    let candidate = relative ~dir "build.ninja" in
    Dep.file_exists candidate *>>= function
    | true -> return (Some candidate)
    | false ->
      if dir = J.Path.the_root
      then return None
      else loop ~dir:(relative ~dir "..")
  in
  loop

let scheme ~dir =
  J.Scheme.all
    [ J.Scheme.sources [relative ~dir "build.ninja"]
    ; J.Scheme.rules_dep (
        find_build_dot_ninja_upwards_from ~dir *>>= function
        | None -> return []
        | Some build_dot_ninja ->
          Dep.contents build_dot_ninja *>>| fun contents ->
          let module Loaded = Load_ninja (struct
            let dir = dir
            let build_dot_ninja = build_dot_ninja
            let contents = contents
          end) in
          Loaded.rules)
    ]

let env = J.Env.create (fun ~dir -> { scheme = scheme ~dir; directories_generated_from = None })
let setup () = Deferred.return env
