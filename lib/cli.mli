(** Command line interface entry point *)

val main
  :  ?argv:string list
  -> run:(Config.t -> unit)
  -> unit
  -> unit
