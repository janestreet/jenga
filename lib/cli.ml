open Core
open! Int.Replace_polymorphic_compare

(* jenga -- Swahili 'to build' *)

let main ?(argv=Array.to_list Sys.argv) ~run () =

  let run config = run config ~forker_args:["internal"; Cmd_internal.forker] in

  let toplevel_group =
    [ "build"         , Cmd_build.command ~toplevel:false ~run ()
    ; "cat-api"       , Cmd_cat_api.command
    ; "db"            , Cmd_db.command
    ; "diagnostics"   , Cmd_diagnostics.command
    ; "env"           , Cmd_env.command
    ; "errors"        , Cmd_errors.command
    ; "internal"      , Cmd_internal.command
    ; "monitor"       , Cmd_monitor.command
    ; "root"          , Cmd_root.command
    ; "stop"          , Cmd_stop.command
    ]
  in

  let toplevel_group_names =
    "help" :: "version" :: List.map toplevel_group ~f:fst
  in

  match argv with
  | _ :: s :: _ when List.mem ~equal:String.(=) toplevel_group_names s ->
    Command.run (Command.group ~summary:"Generic build system" toplevel_group)
      ~argv
  | _ ->
    (* When completing the first argument we would like to ask for the completion of
       both the group names and the flags/arguments of the command below. Unfortunately,
       Command wants to exit instead of returning even when completing. So we create the
       completion ourselves, which is easy enough, even though it's a bit ugly. *)
    begin match argv with
    | _ :: s :: _
      when (match Sys.getenv "COMP_CWORD" with Some "1" -> true | _ -> false) ->
      List.iter toplevel_group_names ~f:(fun group_name ->
        if String.is_prefix ~prefix:s group_name then print_endline group_name)
    | _ -> ()
    end;
    Command.run (Cmd_build.command ~toplevel:true ~run ()) ~argv
;;
