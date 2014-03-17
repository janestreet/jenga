
open Core.Std
open Async.Std

module Alias : sig
  type t with sexp, bin_io
  include Hashable with type t := t
  val create : dir:Path.t -> string -> t (* aliases are directory relative *)
  val split : t -> Path.t * string
  val default : dir:Path.t -> t
  val to_string : t -> string
end

module Goal : sig
  type t = Path of Path.t | Alias of Alias.t
  with sexp, bin_io
  include Hashable with type t := t
  val to_string : t -> string
  val directory : t -> Path.t
  val parse_string : dir:Path.t -> string -> t
end

module Need : sig
  type t
  val goal : Goal.t -> t
  val jengaroot : t
  include Hashable_binable with type t := t
  include Comparable_binable with type t := t
  val to_string : t -> string
end

module Xaction : sig
  type t = {dir : Path.t; prog : string; args : string list;} with sexp, bin_io, compare
  val to_string : t -> string
end

module Iaction : sig
  type t
  val create : tag:Sexp.t -> func:(unit -> unit Deferred.t) -> t
  val tag : t -> Sexp.t
  val func : t -> unit -> unit Deferred.t
end

module Action : sig
  type t with sexp_of
  val case : t -> [ `xaction of Xaction.t | `iaction of Iaction.t ]
  val shell : dir:Path.t -> prog:string -> args:string list -> t
  val internal : tag:Sexp.t -> func:(unit -> unit Deferred.t) -> t
end

module Depends : sig

  type _ t =
  | Return : 'a -> 'a t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Cutoff : ('a -> 'a -> bool) * 'a t -> 'a t
  | All : 'a t list -> 'a list t
  | Deferred : (unit -> 'a Deferred.t) -> 'a t
  | Path : Path.t -> unit t
  | Source_if_it_exists : Path.t -> unit t
  | Absolute : Path.Abs.t -> unit t
  | Alias : Alias.t -> unit t
  | Glob : Fs.Glob.t -> Path.t list t (*deprecated*)
  | Glob_listing : Fs.Glob.t -> Path.t list t
  | Glob_change : Fs.Glob.t -> unit t
  | Contents : Path.t -> string t
  | Contents_abs : Path.Abs.t -> string t
  | Stdout : Action.t t -> string t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  val path : Path.t -> unit t
  val source_if_it_exists : Path.t -> unit t
  val absolute : path:string -> unit t
  val alias : Alias.t -> unit t
  val glob : Fs.Glob.t -> Path.t list t (*deprecated*)
  val glob_listing : Fs.Glob.t -> Path.t list t
  val glob_change : Fs.Glob.t -> unit t
  val action : Action.t t -> unit t
  val action_stdout : Action.t t -> string t
  val deferred : (unit -> 'a Deferred.t) -> 'a t
  val contents : Path.t -> string t
  val contents_absolute : path:string -> string t

  val subdirs : dir:Path.t -> Path.t list t
  val file_exists : Path.t -> bool t

  val read_sexp : Path.t -> Sexp.t t
  val read_sexps : Path.t -> Sexp.t list t

end

module Target_rule : sig
  type t
  val create : targets:Path.t list -> Action.t Depends.t -> t
  val targets : t -> Path.t list
  val head_target : t -> Path.t
  val head_target_and_rest : t -> Path.t * Path.t list
  val action_depends : t -> Action.t Depends.t
end

module Rule : sig

  type t =
  | Target of Target_rule.t
  | Alias of Alias.t * unit Depends.t

  val targets : t -> Path.t list

end

module Gen_key : sig
  type t = {tag : string; dir : Path.t;} with sexp, bin_io
  include Hashable_binable with type t := t
  val create : tag:string -> dir:Path.t -> t
  val to_string : t -> string
end

module Rule_generator : sig
  type t
  val create : Rule.t list Depends.t -> t
  val rules : t -> Rule.t list Depends.t
end

module Rule_scheme : sig
  type t
  val create : tag:string -> (dir:Path.t -> Rule_generator.t) -> t
  val tag : t -> string
  val body : t -> (dir:Path.t -> Rule_generator.t) ref
end

module Env : sig
  type t = {
    putenv : (string * string) list;
    command_lookup_path : [`Replace of string list | `Extend of string list] option;
    schemes : (Pattern.t * Rule_scheme.t option) list;
    build_begin : (unit -> unit Deferred.t);
    build_end : (unit -> unit Deferred.t);
  }
  val create :
    ?putenv : (string * string) list ->
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?build_begin : (unit -> unit Deferred.t) ->
    ?build_end : (unit -> unit Deferred.t) ->
    (string * Rule_scheme.t option) list ->
    t
end
