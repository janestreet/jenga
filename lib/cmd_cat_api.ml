open Core.Std
open Command.Let_syntax

let command =
  Command.basic'
    ~summary:"Print the API supported by this version of jenga"
    [%map_open
      let () = return ()
      in fun () ->
        Printf.printf "%s\n%!" Cat_api.string
    ]
;;
