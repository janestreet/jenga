(** Command line interface entry point *)

val main
  :  ?argv:string list
  -> run:(Config.t -> forker_args:string list -> unit)
  -> unit
  -> unit
