open Core.Std

let () =
  Command.run
    (Command.group
       ~summary:" Bench jenga, and see the results."
       [ "run", Run.command
       ; "report", Report.command
       ])
;;
