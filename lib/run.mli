(** The main entry points of jenga. *)

open! Core

(** Entry point after command line has been processed. [forker_args] is the command line
    arguments to pass to the current exe that will call [Forker.command]. *)
val main
  : Config.t
  -> forker_args:string list
  -> unit

(** A way to run a custom-built jenga with hard-coded rules.
    [path_to_jenga_conf] configuration option gets ignored. *)
val main'
   : Build.Jr_spec.t
  -> root_dir:Path.Abs.t
  -> forker_args:string list
  -> Config.t
  -> unit
