
open Core.Std
open Async.Std

val snap_all_effort : unit -> Effort.Snapped.t

val all_effort : Effort.t

module Persist : sig
  type t with sexp, bin_io
  val create : unit -> t
  val cat_build_script : t -> Path.t list -> unit
end

val build_forever :
  Config.t ->
  Progress.t ->
  jenga_root_path: Path.X.t ->
  top_level_demands : Description.Goal.t list ->
  Fs.t ->
  Persist.t ->
  when_polling:(unit -> unit Deferred.t) ->
  when_rebuilding:(unit -> unit Deferred.t) ->
  unit Deferred.t

val persist_saves_done : Effort.Counter.t

val exit_code_upon_control_c : int ref
