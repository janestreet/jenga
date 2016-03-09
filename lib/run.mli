
open! Core.Std

(** Entry point after command line has been processed. *)

val main : Config.t -> unit

(** A way to run a custom-built jenga with hard-coded rules.
    [path_to_jenga_conf] configuration option gets ignored. *)
val main' : Build.Jr_spec.t -> root_dir:Path.Abs.t -> Config.t -> unit
