(** This module limits the concurrency in jenga.

    Note that the various locking functions below should _not_ be called when inside
    of the [File_access] throttle, to avoid deadlocks.
*)

open! Core
open! Async

(** At any given time, you can either have any number of functions passed
    to [lock_directory_for_action] or any number of functions passed to
    [lock_directory_for_listing] running, but not both at the same time.

    This is presumably because an action running in a directory may create temporary
    files, so we prefer to avoid retriggering spuriously when we see them coming and
    going.
*)
val lock_directory_for_action :
  dir: Path.t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

val lock_directory_for_listing :
  dir: Path.t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

(** Prevent overlapping execution of actions on each target.

    Note that you can have multiple concurrent actions for a target
    even when you have only one [Rule.t]:
    If the value of [Action.t Dep.t] changes over time, jenga
    might decide to start the new [Action.t] before the old one is finished.
    This lock is there to prevent that.
*)
val lock_targets_for_action :
  targets: Path.Rel.t list -> (unit -> 'a Deferred.t) -> 'a Deferred.t

(* for target, 'is there an action writing this target?'
   for directory, 'is there any action running in this directory?' *)
val is_action_running_for_path : Path.Rel.t -> bool
