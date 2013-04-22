
open Core.Std
open Async.Std

val external_action_counter : Effort.Counter.t

module Alias : sig
  type t with sexp
  include Hashable with type t := t
  val create : dir: Path.t -> string -> t (* aliases are directory relative *)
  val split : t -> Path.t * string
  val default : dir:Path.t -> t
  val to_string : t -> string
end

module Scan_id : sig
  type t with sexp
  include Hashable with type t := t
  val of_sexp : Sexp.t -> t
  val to_sexp : t -> Sexp.t
  val to_string : t -> string
end

module Action_id : sig
  type t with sexp,compare
  val of_sexp : Sexp.t -> t
  val to_sexp : t -> Sexp.t
  val to_string : t -> string
end

module Goal : sig
  type t with sexp
  val case : t -> [ `path of Path.t | `alias of Alias.t ]
  val path : Path.t -> t
  val alias : Alias.t -> t
  val to_string : t -> string
  val directory : t -> Path.t
end

module Xaction : sig
  type t = {dir : Path.t; prog : string; args : string list;} with sexp,compare
  val shell : dir:Path.t -> prog:string -> args:string list -> t
  val to_string : t -> string
  val run_now : t -> Job_scheduler.t -> need:string ->
    (unit, [ `non_zero_status | `other_error of exn]) Result.t Deferred.t
  val run_now_stdout : t -> Job_scheduler.t -> need:string ->
    (string, [ `non_zero_status | `other_error of exn]) Result.t Deferred.t
end

module Action : sig
  type t with sexp,compare
  val case : t -> [ `xaction of Xaction.t | `id of Action_id.t ]
  val xaction : Xaction.t -> t
  val internal1 : Action_id.t -> t
  val internal : Sexp.t -> t
  val shell : dir:Path.t -> prog:string -> args:string list -> t
  val to_string : t -> string
end

module Scanner : sig
  type t = [
  | `old_internal of Scan_id.t
  | `local_deps of Path.t * Action.t
  ]
  include Hashable with type t := t
  val to_string : t -> string
  val old_internal : Sexp.t -> t
  val local_deps : dir:Path.t -> Action.t -> t
end

module Dep : sig
  type t with sexp
  include Hashable with type t := t

  val case : t -> [
  | `path of Path.t
  | `alias of Alias.t
  | `scan of t list * Scanner.t
  | `glob of Fs.Glob.t
  | `null
  ]

  val to_string : t -> string
  val path : Path.t -> t
  val glob : Fs.Glob.t -> t
  val scan1 : t list -> Scan_id.t -> t
  val scan : t list -> Sexp.t -> t
  val scanner : t list -> Scanner.t -> t
  val alias : Alias.t -> t
  val null : t
  val default : dir:Path.t -> t
  val parse_string : dir:Path.t -> string -> t
  val parse_string_as_deps : dir:Path.t -> string -> t list
  val compare : t -> t -> int
end

module Target_rule : sig
  type t with sexp
  include Hashable with type t := t
  val create : targets:Path.t list -> deps:Dep.t list -> action:Action.t -> t
  val triple : t -> Path.t list * Dep.t list * Action.t
  val targets : t -> Path.t list
  val head_and_rest_targets : t -> Path.t * Path.t list
  val to_string : t -> string
end

module Rule : sig
  type t with sexp
  val create : targets:Path.t list -> deps:Dep.t list -> action:Action.t -> t
  val alias : Alias.t -> Dep.t list -> t
  val default : dir:Path.t -> Dep.t list -> t
  val targets : t -> Path.t list
  val defines_alias_for : Alias.t -> t -> bool
  val case : t -> [
  | `target of Target_rule.t
  | `alias of Alias.t * Dep.t list
  ]
  val to_string : t -> string
end

module Gen_key : sig
  type t = {tag : string; dir : Path.t;} with sexp
  include Hashable with type t := t
  val create : tag:string -> dir:Path.t -> t
  val to_string : t -> string
end

module Rule_generator : sig
  type t
  val create : deps: Dep.t list -> gen:(unit -> Rule.t list Deferred.t) -> t
  val deps : t -> Dep.t list
  val gen : t -> Rule.t list Deferred.t
end

module Rule_scheme : sig
  type t
  val create : tag:string -> (dir:Path.t -> Rule_generator.t) -> t
  val tag : t -> string
  val body : t -> (dir:Path.t -> Rule_generator.t) ref
end

module Env : sig
  type t = {
    command_lookup_path : [`Replace of string list | `Extend of string list] option;
    action : Sexp.t -> unit Deferred.t;
    scan : Sexp.t -> Dep.t list Deferred.t;
    schemes : (Pattern.t * Rule_scheme.t option) list;
  }
  val create :
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?action : (Sexp.t -> unit Deferred.t) ->
    ?scan : (Sexp.t -> Dep.t list Deferred.t) ->
    (string * Rule_scheme.t option) list ->
    t
end
