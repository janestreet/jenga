
open Core.Std
open Async.Std

module Persist : sig
  type t with sexp
  val create : unit -> t
end

val build_forever :
  Config.t ->
  jenga_root_path: Path.LR.t ->
  top_level_demands : Description.Dep.t list ->
  Fs.t ->
  Persist.t ->
  when_polling:(unit -> unit Deferred.t) ->
  unit Deferred.t
