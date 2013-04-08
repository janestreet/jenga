
open Core.Std
open Async.Std

(* This signature provides the interface between the `user-code' which
   describes the build rules etc for a specific instatnce of jenga,
   and the core jenga build system.
*)

module Path : sig
  type t with sexp
  val relative : dir:t -> string -> t
  val suffix : t -> string -> t
  val to_rrr_string : t -> string
  val dirname : t -> t
  val basename : t -> string
  val the_root : t
  val root_relative : string -> t
end

module Glob : sig
  type t with sexp
  val create : dir:Path.t -> string -> t
  val exec : t -> Path.t list Deferred.t
end

module Alias : sig
  type t with sexp
  val create : dir: Path.t -> string -> t
end

module Action : sig
  type t
  val internal : Sexp.t -> t
  val shell : dir:Path.t -> prog:string -> args:string list -> t
end

module Scanner : sig
  type t
  val old_internal : Sexp.t -> t
  val local_deps : dir:Path.t -> Action.t -> t
end

module Dep : sig
  type t
  val path : Path.t -> t
  val glob : Glob.t -> t
  val scan : t list -> Sexp.t -> t (* TODO: remove, in favour of more general scanner *)
  val scanner : t list -> Scanner.t -> t
  val alias : Alias.t -> t
  val null : t
  val parse_string : dir:Path.t -> string -> t
end

module Rule : sig
  type t
  val create : targets:Path.t list -> deps:Dep.t list -> action:Action.t -> t
  val alias : Alias.t -> Dep.t list -> t
  val default : dir:Path.t -> Dep.t list -> t
  val targets : t -> Path.t list
  val to_string : t -> string
end

module Rule_generator : sig
  type t
  val create : deps:Dep.t list -> gen:(unit -> Rule.t list Deferred.t) -> t
end

module Rule_scheme : sig
  type t
  val create : tag:string -> (dir:Path.t -> Rule_generator.t) -> t
end

module Env : sig
  type t = Description.Env.t
  val create :
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?action : (Sexp.t -> unit Deferred.t) ->
    ?scan : (Sexp.t -> Dep.t list Deferred.t) ->
    (string * Rule_scheme.t option) list ->
    t
end

val verbose : unit -> bool
val load_sexp_for_jenga : (Sexp.t -> 'a) -> Path.t -> 'a Deferred.t
val parse_rules_from_simple_makefile : Path.t -> Rule.t list Deferred.t
