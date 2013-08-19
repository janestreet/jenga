
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

module Scan_id : sig
  type t with sexp, bin_io
  include Hashable with type t := t
  val of_sexp : Sexp.t -> t
  val to_sexp : t -> Sexp.t
  val to_string : t -> string
end

module Action_id : sig
  type t with sexp, bin_io, compare
  val of_sexp : Sexp.t -> t
  val to_sexp : t -> Sexp.t
  val to_string : t -> string
end

module Goal : sig
  type t with sexp, bin_io
  val case : t -> [ `path of Path.t | `alias of Alias.t ]
  val path : Path.t -> t
  val alias : Alias.t -> t
  val to_string : t -> string
  val directory : t -> Path.t
end

module Xaction : sig
  type t = {dir : Path.t; prog : string; args : string list;} with sexp, bin_io, compare
  val shell : dir:Path.t -> prog:string -> args:string list -> t
  val to_string : t -> string
  val to_script : t -> string
end

module Action : sig
  type t with sexp, bin_io, compare
  include Hashable_binable with type t := t
  val case : t -> [ `xaction of Xaction.t | `id of Action_id.t ]
  val xaction : Xaction.t -> t
  val internal1 : Action_id.t -> t
  val internal : Sexp.t -> t
  val shell : dir:Path.t -> prog:string -> args:string list -> t
  val to_string : t -> string
end

module Dep1 : sig
  type t with sexp, bin_io
  include Hashable with type t := t

  val case : t -> [
  | `path of Path.t
  | `alias of Alias.t
  | `glob of Fs.Glob.t
  | `absolute of Path.Abs.t
  ]

  val to_string : t -> string
  val path : Path.t -> t
  val glob : Fs.Glob.t -> t
  val alias : Alias.t -> t
  val default : dir:Path.t -> t
  val parse_string : dir:Path.t -> string -> t
  val parse_string_as_deps : dir:Path.t -> string -> t list
  val absolute : path:string -> t

end

module Depends : sig

  type _ t =
  | Return : 'a -> 'a t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | All : 'a t list -> 'a list t
  | Need : Dep1.t list -> unit t
  | Stdout : Action.t t -> string t
  | Glob : Fs.Glob.t -> Path.t list t
  | Scan_id : Scan_id.t t -> unit t
  | Scan_local_deps : (Path.t * Action.t) t -> unit t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val all : 'a t list -> 'a list t
  val need : Dep1.t list -> unit t
  val stdout : Action.t t -> string t
  val glob : Fs.Glob.t -> Path.t list t

  val all_unit : unit t list -> unit t
  val file_contents : Path.t -> string t

  val scan_id : Scan_id.t t -> unit t
  val scan_local_deps : (Path.t * Action.t) t -> unit t

end

module Target_rule : sig
  type t
  val create_new : targets:Path.t list -> Action.t Depends.t -> t
  val targets : t -> Path.t list
  val head_target : t -> Path.t
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
  val create_new : unit Depends.t -> gen:(unit -> Rule.t list Deferred.t) -> t
  val depends : t -> unit Depends.t
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
    version : Version.t;
    putenv : (string * string) list;
    command_lookup_path : [`Replace of string list | `Extend of string list] option;
    action : Sexp.t -> unit Deferred.t;
    scan : Sexp.t -> unit Depends.t Deferred.t;
    schemes : (Pattern.t * Rule_scheme.t option) list;
    build_begin : (unit -> unit Deferred.t);
    build_end : (unit -> unit Deferred.t);
  }
  val create :
    ?version : Version.t ->
    ?putenv : (string * string) list ->
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?action : (Sexp.t -> unit Deferred.t) ->
    ?scan : (Sexp.t -> unit Depends.t Deferred.t) ->
    ?build_begin : (unit -> unit Deferred.t) ->
    ?build_end : (unit -> unit Deferred.t) ->
    (string * Rule_scheme.t option) list ->
    t
end
