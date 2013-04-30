
open Core.Std
let main = Jenga_lib.Jenga_progress.command_line
let () = Exn.handle_uncaught ~exit:true main
