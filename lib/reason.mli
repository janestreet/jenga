
open Core.Std

include module type of Error_reason_type

val messages : tag:string -> t -> unit
val message_summary : Config.t -> Description.Goal.t -> t -> unit
