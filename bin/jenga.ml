
open Core.Std

(* jenga -- Swahili 'to build' *)

let main = Jenga_lib.Command_line.main

let () = Exn.handle_uncaught ~exit:true main
