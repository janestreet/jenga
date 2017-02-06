(** Because forking is so expensive (even if memory is copied lazily, the page table is
    copied eagerly, which makes it very costly when the forking process uses a lot of
    memory), this module is used to hold a number of processed forked off the main jenga
    process early on, when jenga uses little memory. Jenga can then ask these processes to
    cheaply spawn commands. *)

open! Core
open! Async

val init : Config.t -> args:string list -> unit Or_error.t Deferred.t (* just once *)

module Reply : sig
  type t = {
    stdout : string;
    stderr : string;
    outcome : [`success | `error of string];
    duration : Time.Span.t;
  }
end

module Request : sig
  type t
  val create :
    (* calls to putenv, to be done in parent, before the fork *)
    putenv:(string * string option) list ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    t
end

val run : Request.t -> Reply.t Deferred.t
val command : Command.t
