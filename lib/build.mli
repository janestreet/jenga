
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
  val create : unit -> t
  module Counts : sig
    type t with bin_io
    val to_string : t -> string
    val fraction : t -> (int*int)
  end
  val snap : t -> Counts.t
  val readme : string list
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
