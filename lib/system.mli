(** Module for OS specific configuration. *)

open! Core

val description : string
val num_cpus_if_known : int option
val has_inotify : bool
