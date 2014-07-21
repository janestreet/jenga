
open Async.Std

(* [Dep.t] is the type central to jenga's API, supporting the description of dependencies
   in monadic style *)

include module type of Dep_type

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : 'a t -> ('a -> 'b) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val all : 'a t list -> 'a list t
val all_unit : unit t list -> unit t
val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t
val deferred : (unit -> 'a Deferred.t) -> 'a t

val action : Action.t t -> unit t
val action_stdout : Action.t t -> string t
val alias : Alias.t -> unit t
val path : Path.t -> unit t

val source_if_it_exists : Path.t -> unit t

val contents : Path.t -> string t
val contents_cutoff : Path.t -> string t

val glob_listing : Fs.Glob.t -> Path.t list t
val glob_change : Fs.Glob.t -> unit t

val subdirs : dir:Path.t -> Path.t list t
val file_exists : Path.t -> bool t

module List : sig
  val concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t
end

val buildable_targets : dir:Path.t -> Path.t list t
