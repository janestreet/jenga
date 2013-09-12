
open Core.Std
open Async.Std

(* This signature provides the interface between the `user-code' which
   describes the build rules etc for a specific instatnce of jenga,
   and the core jenga build system.
*)

module Path : sig (* repo-relative path *)
  type t with sexp
  val relative : dir:t -> string -> t
  val to_string : t -> string (* repo root-relative string *)
  val to_absolute_string : t -> string (* AVOID USING THIS *)
  val dirname : t -> t
  val basename : t -> string
  val the_root : t
  val root_relative : string -> t

  (* [dotdot ~dir path]
     compute relative ".."-based-path-string to reach [path] from [dir] *)
  val dotdot : dir:t -> t -> string

end

module Kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
end

module Glob : sig
  type t with sexp
  val create : dir:Path.t -> ?kinds: Kind.t list -> string -> t
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
  val bash : dir:Path.t -> string -> t
  val write_string : string -> target:Path.t -> t
end

module Scanner : sig
  type t
  val local_deps : dir:Path.t -> Action.t -> t
end

module Dep : sig (* old pre-monadic interface *)
  type t
  val path : Path.t -> t
  val absolute : path:string -> t
  val glob : Glob.t -> t
  val alias : Alias.t -> t
  val scan : t list -> Sexp.t -> t
  val scanner : t list -> Scanner.t -> t
  val parse_string : dir:Path.t -> string -> t
end

(* taster of API v2...*)
module Depends : sig (* The jenga monad *)
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  val path : Path.t -> unit t
  val absolute : path:string -> unit t
  val alias : Alias.t -> unit t
  val glob : Glob.t -> Path.t list t
  val action : Action.t t -> unit t
  val action_stdout : Action.t t -> string t
  val deferred : (unit -> 'a Deferred.t) -> 'a t
  val contents : Path.t -> string t
  val contents_absolute : path:string -> string t
  val subdirs : dir:Path.t -> Path.t list t
  val read_sexp : Path.t -> Sexp.t t
  val read_sexps : Path.t -> Sexp.t list t
end

module Rule : sig
  type t
  val create : targets:Path.t list -> deps:Dep.t list -> action:Action.t -> t
  val alias : Alias.t -> Dep.t list -> t
  val default : dir:Path.t -> Dep.t list -> t
  val targets : t -> Path.t list
  (* taster of API v2 create function... *)
  val create_api_v2 : targets:Path.t list -> Action.t Depends.t -> t
  val alias_api_v2 : Alias.t -> unit Depends.t -> t
end

module Rule_generator : sig
  type t
  val create : deps:Dep.t list -> gen:(unit -> Rule.t list Deferred.t) -> t
  val create_api_v2 : Rule.t list Depends.t -> t
end

module Rule_scheme : sig
  type t
  val create : tag:string -> (dir:Path.t -> Rule_generator.t) -> t
end

module Version : sig
   (* This Versioning pre-dates idea for versioning of entire API *)
  type t =
  | Pre_versioning
  | V_2013_07_09
end

module Env : sig
  type t = Description.Env.t
  val create :
    ?version:Version.t ->
    ?putenv:(string * string) list ->
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?action : (Sexp.t -> unit Deferred.t) ->
    ?scan : (Sexp.t -> Dep.t list Deferred.t) ->
    ?build_begin : (unit -> unit Deferred.t) ->
    ?build_end : (unit -> unit Deferred.t) ->
    (string * Rule_scheme.t option) list ->
    t
end

val verbose : unit -> bool

val run_action_now : Action.t -> unit Deferred.t
val run_action_now_stdout : Action.t -> string Deferred.t

val enqueue_file_access : (unit -> 'a Deferred.t) -> 'a Deferred.t

(* these all wrap with enqueue_file_access *)
val load_sexp_for_jenga : (Sexp.t -> 'a) -> Path.t -> 'a Deferred.t
val load_sexps_for_jenga : (Sexp.t -> 'a) -> Path.t -> 'a list Deferred.t
(*val parse_rules_from_simple_makefile : Path.t -> Rule.t list Deferred.t*)
