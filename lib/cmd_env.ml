open Core
open Async
open! Int.Replace_polymorphic_compare
open Command.Let_syntax
let return = Async.return

let with_menu_connection ~f =
  return (Special_paths.discover_root ()) >>=? fun root_dir ->
  Jenga_client.with_menu_connection ~root_dir ~f
  >>| Or_error.join

let print_info_line ~as_sexp info =
  if as_sexp then
    printf !"%{sexp:Var.Info.t}\n" info
  else begin
    let {Var.Info.name;default;choices;peeked;value} = info in
    let                            resolved , used_default , overridden =
      match default,value with
      | None, None              -> None     , false        , false
      | None, Some v            -> Some v   , false        , false
      | Some d, None            -> Some d   , true         , false
      | Some _, Some v          -> Some v   , false        , true
    in
    let displayed =
      match resolved with
      | Some s -> Api.Shell.escape s
      | None -> "<unset>"
    in
    assert (not (used_default && overridden));
    printf "%s = %s%s%s%s%s\n" name displayed
      (if used_default      then " (default)"       else "")
      (if overridden        then " (set by user)"   else "")
      (if peeked            then " (peeked)"        else "")
      (match choices with
      | [] -> ""
      | _ :: _ -> sprintf " # choices: %s" (String.concat ~sep:" " choices))
  end

let get =
  Command.async_or_error ~summary:"show the value of a registered environment variable"
    [%map_open
      let name = anon ("NAME" %: string)
      and as_sexp = flag "sexp" no_arg ~doc:" print in sexp format" in
      fun () ->
        with_menu_connection ~f:(fun cwm ->
          Rpc_intf.Getenv.dispatch_multi cwm (Var.Getenv.query name)
        )
        >>| Or_error.join
        >>|? print_info_line ~as_sexp
    ]

let anon_strings name =
  [%map_open
    let args1 = anon (sequence (name %: string))
    and args2 =
      map ~f:(function None -> [] | Some l -> l)
        (flag "--" ~doc:(" collect remaining arguments into " ^ name) escape)
    in
    args1 @ args2]

let set =
  Command.async_or_error
    ~summary:"set the value of a registered environment variable"
    [%map_open
      let name = anon ("NAME" %: string)
      and args = anon_strings "VALUE"
      in fun () ->
        let value = String.concat ~sep:" " args in
        with_menu_connection ~f:(fun cwm ->
          Rpc_intf.Setenv.dispatch_multi cwm
            (Var.Setenv.query name ~value:(Some value))
        )
        >>| Or_error.join]

let unset =
  Command.async_or_error ~summary:"unset a registered environment variable"
    [%map_open
      let name = anon ("NAME" %: string)
      in fun () ->
        with_menu_connection ~f:(fun cwm ->
          Rpc_intf.Setenv.dispatch_multi cwm
            (Var.Setenv.query name ~value:None)
        )
        >>| Or_error.join
    ]

let print =
  Command.async_or_error ~summary:"show the values of all registered environment variables"
    [%map_open
      let as_sexp = flag "sexp" no_arg ~doc:" print in sexp format"
      in fun () ->
        with_menu_connection ~f:(fun cwm ->
          Rpc_intf.Env_info.dispatch_multi cwm ()
        )
        >>|? fun infos -> List.iter infos ~f:(print_info_line ~as_sexp)
    ]

let command =
  Command.group
    ~summary:" Manipulation of the server's registered environment variables"
    [ "set", set
    ; "unset", unset
    ; "print", print
    ; "get", get
    ]
;;
