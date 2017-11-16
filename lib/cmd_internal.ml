open Core
open! Int.Replace_polymorphic_compare

open Command.Let_syntax

let rpc_versions =
  Command.basic
    ~summary:" Show what rpc versions are supported by the servers of this executable"
    [%map_open
      let () = return ()
      in fun () ->
        Map.iteri Rpc_server.versions ~f:(fun ~key ~data ->
          printf "%s: %s\n" key
            (String.concat ~sep:" " (List.map ~f:Int.to_string (Set.to_list data))))
    ]
;;

let forker = "forker"

let command =
  Command.group
    ~summary:" various subcommands not meant for casual users"
    [ "rpc-versions", rpc_versions
    ; forker, Forker.command
    ]
;;
