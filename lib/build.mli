
open Core.Std
open Async.Std

module Persist : sig
  type t with sexp, bin_io
  val create : unit -> t
  val equal : t -> t -> bool
  val copy : t -> t
end

module Progress : sig
  type t
  val create : Fs.t -> t
  val snap : t -> Mon.Progress.t
end

val build_forever :
  Config.t ->
  Progress.t ->
  jenga_root_path: Path.LR.t ->
  top_level_demands : Description.Dep.t list ->
  Fs.t ->
  Persist.t ->
  when_polling:(unit -> unit Deferred.t) ->
  when_rebuilding:(unit -> unit Deferred.t) ->
  unit Deferred.t

module Run_now : sig
  val run_action_now : Description.Action.t -> unit Deferred.t
  val run_action_now_stdout : Description.Action.t -> string Deferred.t
end

val persist_saves_done : Effort.Counter.t


