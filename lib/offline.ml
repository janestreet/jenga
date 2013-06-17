
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let message fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let cat_sexp =
  Command.basic Command.Spec.empty
    ~summary:"cat .jenga.db in sexp format (big!)"
    (fun () ->
      match Path.Root.discover() with | `cant_find_root ->
        message "Cant find '%s' in start-dir or any ancestor dir" Init.jenga_root_basename
      | `ok ->
        let root_dir = Path.to_absolute_string Path.the_root in
        Init.in_async ~f:(fun () ->
          message "cat_sexp...";
          let db_filename = root_dir ^/ Path.db_basename in
          message "cat_sexp, loading...";
          Persist.State.load_db ~db_filename >>= fun db ->
          message "cat_sexp, converting...";
          let sexp = Persist.State.sexp_of_t db in
          message "cat_sexp, output...";
          let stdout = force Writer.stdout in
          Writer.write_sexp ~hum:true stdout sexp;
          message "cat_sexp, waiting...";
          Writer.flushed stdout >>= fun () ->
          message "cat_sexp, done...";
          return 0;
        )
    )

let offline_commands =
  Command.group
    ~summary:"Jenga offline - query/dump info from jenga's persistent database"
    ~readme:(fun () -> "\
Query/dump info from jenga's persistent database (.jenga.db)
The jenga server does not need to be running.")
    [
      "cat-sexp" , cat_sexp
    ]

let command_line () =
  Command.run offline_commands
