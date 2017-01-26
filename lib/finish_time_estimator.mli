(** This module implements the guessing of when the build will finished based on how the
    amount of remaining work changes, which is shown in the output of [jenga monitor]
    and [jenga --progress]. *)

open! Core

type t
val create : decay_factor_per_second:float -> t
val push_todo : t -> int -> unit
val estimated_finish_time_string : t -> string
