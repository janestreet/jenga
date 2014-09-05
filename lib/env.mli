
open Core.Std
open Async.Std

type t

val create :
  ?putenv : (string * string) list ->
  ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
  ?build_begin : (unit -> unit Deferred.t) ->
  ?build_end : (unit -> unit Deferred.t) ->
  (dir: Path.t -> Scheme.t) ->
  t

val putenv : t -> (string * string) list
val build_begin : t -> (unit -> unit Deferred.t)
val build_end : t -> (unit -> unit Deferred.t)

val get_scheme : t -> dir:Path.t -> Scheme.t
