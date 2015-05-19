
open Core.Std

(* jenga -- Swahili 'to build' *)

let toplevel_group =
  [ "build", Jenga_lib.Cmd_build.command ~toplevel:false
  ; "cat-api", Jenga_lib.Cmd_cat_api.command
  ; "monitor", Jenga_lib.Cmd_monitor.command
  ; "db", Jenga_lib.Cmd_db.command
  ]
;;

let toplevel_group_names =
  "help" :: "version" :: List.map toplevel_group ~f:fst
;;

let () =
  let arg1 = if Array.length Sys.argv > 1 then Some Sys.argv.(1) else None in
  match arg1 with
  | Some s when List.mem toplevel_group_names s ->
    Command.run (Command.group ~summary:"Generic build system" toplevel_group)
  | _ ->
    (* When completing the first argument we would like to ask for the completion of
       both the group names and the flags/arguments of the command below. Unfortunately,
       Command wants to exit instead of returning even when completing. So we create the
       completion ourselves, which is easy enough, even though it's a bit ugly. *)
    begin match arg1 with
    | Some s when Sys.getenv "COMP_CWORD" = Some "1" ->
      List.iter toplevel_group_names ~f:(fun group_name ->
        if String.is_prefix ~prefix:s group_name then print_endline group_name)
    | _ -> ()
    end;
    Command.run (Jenga_lib.Cmd_build.command ~toplevel:true)
;;
