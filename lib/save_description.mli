
open Core.Std
open Async.Std

(* [Save_description.t] is a special kind of job which we can either run directly,
   or we can construct the equivalent [Job.t].. a call to bash/echo/redirect *)

type t [@@deriving sexp_of]
val create : contents:string -> target:Path.t -> chmod_x:bool -> t
val job : t -> Job.t
val run : t -> unit Deferred.t
