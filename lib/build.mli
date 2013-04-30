
open Core.Std
open Async.Std

module Persist : sig
  type t with sexp
  val create : unit -> t
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
end

val build_forever :
  Config.t ->
  Progress.t ->
  jenga_root_path: Path.LR.t ->
  top_level_demands : Description.Dep.t list ->
  Fs.t ->
  Persist.t ->
  when_polling:(unit -> unit Deferred.t) ->
  unit Deferred.t
