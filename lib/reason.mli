(** A type representing the various errors that can happen in jenga. Compared to simply
    using [Error.t], we have control over the display. *)

open! Core

include module type of Reason_type

val filesystem_related : t -> bool

val to_string_one_line : t -> string
val to_extra_lines : t -> dir:Path.Rel.t -> string list

val messages : need:Goal.t -> t -> unit
val message_summary : Config.t -> need:Goal.t -> t -> unit
