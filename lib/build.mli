
open Core.Std
open Async.Std

val snap_all_effort : unit -> Effort.Snapped.t

val run_effort : Effort.t
val work_effort : Effort.t
val intern_effort : Effort.t

module Persist : sig
  type t with sexp, bin_io
  val create : unit -> t
end

module Progress : sig
  type t
  val create : Fs.t -> t
  val snap : t -> Mon.Progress.t
end

val build_forever :
  Config.t ->
  Progress.t ->
  jenga_root_path: Path.X.t ->
  top_level_demands : Description.Dep.t list ->
  Fs.t ->
  Persist.t ->
  when_polling:(unit -> unit Deferred.t) ->
  when_rebuilding:(unit -> unit Deferred.t) ->
  unit Deferred.t

val persist_saves_done : Effort.Counter.t
