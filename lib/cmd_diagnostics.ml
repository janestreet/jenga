open Core
open Async
open! Int.Replace_polymorphic_compare

open Tenacious_lib
open Command.Let_syntax
let return = Async.return

module Tenacious_graph = struct
  let colored color s =
    let color_code = match color with
      | `red -> 1
      | `green -> 2
      | `yellow -> 3
      | `magenta -> 5
    in
    sprintf "\x1b[4%dm%s\x1b[49m" color_code s

  module Dump = Graph.Dump

  let print writer =
    let rec go writer indent  { Dump. id; name; children; age; } =
      let status =
        match children with
        | See_above -> colored `yellow "See_above"
        | Here [] -> colored `red "Working"
        | Here (_ :: _) -> colored `green "Waiting"
      in
      Writer.writef writer "%s| %6s %s %s %s\n"
        (String.make (indent * 2) ' ')
        (Int63.to_string id)
        name
        status
        (Time.Span.to_string age);
      List.iter (
        match children with
        | See_above -> []
        | Here l -> l)
        ~f:(go writer (indent+1))
    in
    go writer 0

  let command =
    Command.async_or_error
      ~summary:(sprintf !"dump the dependency graph of a running server to stdout")
      [%map_open
        let sexp = flag "sexp" no_arg ~doc:" print in sexp format" in
        fun () ->
          match Special_paths.discover_root () with
          | Error e -> return (Error e)
          | Ok root_dir ->
            Writer.behave_nicely_in_pipeline ();
            Jenga_client.with_menu_connection ~root_dir ~f:(fun cwm ->
              Rpc_intf.Dump_tenacious_graph.dispatch_multi cwm ())
            >>| Or_error.join
            >>|? (fun dump ->
              if sexp
              then Writer.write_sexp (Lazy.force Writer.stdout) (Dump.sexp_of_t dump)
              else print (Lazy.force Writer.stdout) dump)
      ]
end

let command =
  Command.group ~summary:"Jenga diagnostics"
    [ "dump-tenacious-graph", Tenacious_graph.command ]
