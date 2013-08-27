
(* Jenga API version 2 - Monadic Style *)

(* This signature provides the interface between the `user-code' which
   describes the build rules etc for a specific instatnce of jenga,
   and the core jenga build system.
*)

open Core.Std
open Async.Std

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
end

module Alias : sig
  type t with sexp
  val create : dir: Path.t -> string -> t
end

module Action : sig
  type t
  val shell : dir:Path.t -> prog:string -> args:string list -> t
  val bash : dir:Path.t -> string -> t
  val write_string : string -> target:Path.t -> t
end

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
end

module Rule : sig
  type t
  val create : targets:Path.t list -> Action.t Depends.t -> t
  val alias : Alias.t -> unit Depends.t -> t
  val default : dir:Path.t -> unit Depends.t -> t
  val targets : t -> Path.t list
end

module Generator : sig
  type t
  val create : Rule.t list Depends.t -> t
end

module Scheme : sig
  type t
  val create : tag:string -> (dir:Path.t -> Generator.t) -> t
end

module Env : sig
  type t = Description.Env.t
  val create :
    ?putenv:(string * string) list ->
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?build_begin : (unit -> unit Deferred.t) ->
    ?build_end : (unit -> unit Deferred.t) ->
    (string * Scheme.t option) list ->
    (* odd return type (instead of "t") - matches expected type of setup *)
    (unit -> t Deferred.t)

end

val verbose : unit -> bool

(* should these stay? *)
val load_sexp_for_jenga : (Sexp.t -> 'a) -> Path.t -> 'a Depends.t
val load_sexps_for_jenga : (Sexp.t -> 'a) -> Path.t -> 'a list Depends.t
