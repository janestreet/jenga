
open Core.Std
open Async.Std

type t

val create :
  ?run_when_persist_format_has_changed:Action.t ->
  ?putenv : (string * string) list ->
  ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
  ?build_begin : (unit -> unit Deferred.t) ->
  ?build_end : (unit -> unit Deferred.t) ->
  ?artifacts: (dir:Path.t -> Path.t list Dep.t) ->
  (dir: Path.t -> Scheme.t) ->
  t

val run_when_persist_format_has_changed : t -> Action.t option

val putenv : t -> (string * string) list
val build_begin : t -> (unit -> unit Deferred.t)
val build_end : t -> (unit -> unit Deferred.t)
val artifacts_policy : t -> Artifact_policy.t
val get_scheme : t -> dir:Path.t -> Scheme.t
