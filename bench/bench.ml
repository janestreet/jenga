open Core

let () =
  Command.run
    (Command.group
       ~summary:" Bench jenga, and see the results."
       [ "micro", Micro.command
       ; "run", Run.command
       ; "report", Report.command
       ; "analyze-debug", Analyze_debug.command
       ])
;;
