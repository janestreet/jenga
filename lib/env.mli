(** [Env.t] is the value that contains all the configuration related to build rules
    that a user of the jenga library can provide to jenga.
    See api.mli for documentation of the various fields. *)

open! Core
open! Async.Std

type t

type delete_predicate = (non_target:Path.t -> bool) Dep.t

val create :
  ?putenv : (string * string option) list ->
  ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
  ?build_begin : (unit -> unit Deferred.t) ->
  ?build_end : (unit -> unit Deferred.t) ->
  ?delete_eagerly:delete_predicate ->
  ?delete_if_depended_upon:delete_predicate ->
  (dir: Path.t -> Scheme.t) ->
  t

val putenv : t -> (string * string option) list
val build_begin : t -> (unit -> unit Deferred.t)
val build_end : t -> (unit -> unit Deferred.t)
val get_scheme : t -> dir:Path.t -> Scheme.t
val delete_eagerly : t -> delete_predicate option
val delete_if_depended_upon : t -> delete_predicate
