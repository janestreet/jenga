
open! Core.Std
open! Async.Std

module Jr_spec : sig

  type t = In_root_dir | Path of Path.t | Env of (unit -> Env.t)

end

val build_forever :
  Config.t ->
  Progress.t ->
  jr_spec: Jr_spec.t ->
  top_level_demands : Goal.t list ->
  Fs.t ->
  Persist.t ->
  save_db_now:(unit -> unit Deferred.t) ->
  when_rebuilding:(unit -> unit Deferred.t) ->
  Nothing.t Deferred.t

val exit_code_upon_control_c : int ref
