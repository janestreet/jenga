
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

val run :
  (* calls to putenv, to be done in parent, before the fork *)
  putenv:(string * string) list ->
  dir:Path.t -> prog:string -> args:string list ->
  Reply.t Deferred.t
