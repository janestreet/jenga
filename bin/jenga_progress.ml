
open Core.Std
let main = Jenga_lib.Jenga_progress.main
let () = Exn.handle_uncaught ~exit:true main
