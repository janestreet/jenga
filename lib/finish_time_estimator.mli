open! Core.Std

type t
val create : decay_factor_per_second:float -> t
val push_todo : t -> int -> unit
val estimated_finish_time_string : t -> string
