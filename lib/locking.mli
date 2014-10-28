
open Core.Std
open Async.Std

(* directory locking *)
val lock_directory_for_action :
  dir: Path.t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

val lock_directory_for_listing :
  dir: Path.t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

(* target locking *)
val lock_targets_for_action :
  targets: Path.Rel.t list -> (unit -> 'a Deferred.t) -> 'a Deferred.t

val is_action_running_for_target : Path.Rel.t -> bool
