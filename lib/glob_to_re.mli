(** This module converts the syntax of the [Api.Glob.create ..] globs into the syntax of
    the [Str] library. *)
val convert_unanchored : string -> string
