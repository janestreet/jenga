
open Core.Std
open Description

type t
val create : unit -> t

val set_status : t -> key:Goal.t -> data:Status.t -> unit
val message_errors : Config.t -> t -> unit

module Snapped : sig

  type t with bin_io

  val total : t -> int
  val todo : t -> int
  val fraction : t -> (int*int) (* good/total *)
  val completed : t -> bool (* good = total *)

  val to_string : todo_breakdown:bool -> good_breakdown:bool -> t -> string

end

val snap : t -> Snapped.t

val readme : unit -> string


