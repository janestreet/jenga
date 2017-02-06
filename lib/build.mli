(** This module is the part of jenga that interprets the [Dep] monad, by running
    actions in topological order of dependency, rerunning as needed, creating the
    persistent state etc.
    This module is not meant to be used directly, except for the Jr_spec type. *)

open! Core
open! Async

module Jr_spec : sig

  (** A [t] represents the various ways to find the specifications of the rules:
      - [In_root_dir] means to look for [jenga.conf] or [jengaroot.ml] in the root
        directory (as specified in path.ml), and load it. [jengaroot.ml] is compiled
        directly, and is expected to have the interface described in
        [jenga_root_interface.ml]. [jenga.conf] is expected to contain an s-expression
        like "(modules (file1.ml file2.ml))", and then the given ml-files are loaded
        in that order and the last one is expected to have the same interface as the
        [jengaroot.ml].
      - [Path] is like In_root_dir, but the ml files are looked up in the given directory
        rather than at the root
      - [Env] allows to give a value directly, and so it can be used to link the
        rules statically into jenga, as opposed to dynamically in the previous
        two cases. *)
  type t = In_root_dir | Path of Path.t | Env of (unit -> Env.t Deferred.t)

end

val build_forever :
  Config.t ->
  Progress.t ->
  jr_spec: Jr_spec.t ->
  top_level_goals : Goal.t list ->
  Fs.t ->
  Persist.t ->
  save_db_now:(unit -> unit Deferred.t) ->
  when_rebuilding:(unit -> unit Deferred.t) ->
  Nothing.t Deferred.t

val exit_code_upon_control_c : int ref
