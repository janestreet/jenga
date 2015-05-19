open Core.Std

let command =
  Command.basic Command.Spec.empty
    ~summary:"Print the API supported by this version of jenga"
    (fun () -> Printf.printf "%s\n%!" Cat_api.string)
;;
