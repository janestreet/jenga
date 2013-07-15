
open Core.Std
open Async.Std

val init : Config.t -> unit (* just once *)

module Reply : sig
  type t = {
    stdout : string;
    stderr : string;
    outcome : [`success | `error of string];
  }
end

module Rel_path_semantics : sig
  type t = Old_wrt_repo_root | New_wrt_working_dir
end

module Request : sig
  type t
  val create :
    rel_path_semantics:Rel_path_semantics.t ->
    (* calls to putenv, to be done in parent, before the fork *)
    putenv:(string * string) list ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    t
end

val run : Request.t -> Reply.t Deferred.t
