
(* jem - jenga monitor *)

open Core.Std
let main = Jenga_lib.Jem_command_line.main
let () = Exn.handle_uncaught ~exit:true main
