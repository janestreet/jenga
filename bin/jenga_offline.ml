
(* jenga_offline *)

open Core.Std
let main = Jenga_lib.Offline.command_line
let () = Exn.handle_uncaught ~exit:true main
