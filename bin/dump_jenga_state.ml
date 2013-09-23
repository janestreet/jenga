
open Core.Std
let main = Jenga_lib.Dump_jenga_state.command_line
let () = Exn.handle_uncaught ~exit:true main
