
(* jem - jenga monitor *)

open Core.Std
let main = Jenga_lib.Jem.command_line
let () = Exn.handle_uncaught ~exit:true main
