
open Core.Std
open Async.Std

(* Entry point after command line has been processed *)

val main : Config.t -> unit

module For_user : sig
  (* access to config from user rules *)
  val config : unit -> Config.t
end
