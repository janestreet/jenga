open Core
open! Int.Replace_polymorphic_compare

open Command.Let_syntax

let command =
  Command.basic
    ~summary:"prints the absolute path to the jenga root"
    [%map_open
      let () = return ()
      in fun () ->
        match Special_paths.discover_root () with
        | Error e -> prerr_endline (Error.to_string_hum e); exit 1
        | Ok path -> print_endline (Path.Abs.to_string path)
    ]
