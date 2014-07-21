
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Spec = Command.Spec
let (+>) = Spec.(+>)
let (++) = Spec.(++)
let (%:) = Spec.(%:)

let e_message fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let cat_sexp =
  Command.basic Spec.empty
    ~summary:"cat .jenga.db in sexp format (big!)"
    (fun () ->
      match Path.Root.discover() with | `cant_find_root ->
        e_message "Cant find '%s' or '%s' in start-dir or any ancestor dir"
          Misc.jenga_conf_basename
          Misc.jenga_root_basename
      | `ok ->
        let root_dir = Path.Rel.to_absolute_string Path.Rel.the_root in
        Misc.in_async ~f:(fun () ->
          e_message "cat_sexp...";
          let db_filename = root_dir ^/ Misc.db_basename in
          e_message "cat_sexp, loading...";
          Persist.State.load_db ~db_filename >>= fun db ->
          e_message "cat_sexp, converting...";
          let sexp = Persist.State.sexp_of_t db in
          e_message "cat_sexp, output...";
          let stdout = force Writer.stdout in
          Writer.write_sexp ~hum:true stdout sexp;
          e_message "cat_sexp, waiting...";
          Writer.flushed stdout >>= fun () ->
          e_message "cat_sexp, done...";
          return 0;
        )
    )

let offline_commands =
  Command.group
    ~summary:"Jenga offline - query/dump info from jenga's persistent database"
    ~readme:(fun () -> "\
Query/dump info from jenga's persistent database (.jenga.db)
The jenga server does not need to be running.

REMOVED: `cat-build-script'
Superseded by Makefile extraction, via new API reflection functions.
")
    [
      "cat-sexp" , cat_sexp;
    ]

let command_line () =
  Command.run offline_commands
