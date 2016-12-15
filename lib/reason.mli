(** A type representing the various errors that can happen in jenga. Compared to simply
    using [Error.t], we have control over the display. *)

open! Core.Std

include module type of Error_reason_type

val filesystem_related : t -> bool

val to_string_one_line : t -> string
val to_extra_lines : t -> string list

val messages : need:string -> t -> unit
val message_summary : Config.t -> need:string -> t -> unit
