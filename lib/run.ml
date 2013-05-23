
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Build_state = Build.Persist

let trace fmt =
  ksprintf (fun string ->
    Message.trace "Run: %s" string
  ) fmt

let jenga_root_basename =
  match Core.Std.Sys.getenv "JENGA_ROOT" with
  | None -> "JengaRoot.ml"
  | Some x -> x

let discover_root() =
  match Repo_root.discover ~marker:jenga_root_basename with
  | `cant_find_root ->
    failwithf "Cant find '%s' in start-dir or any ancestor dir"
      jenga_root_basename ()
  | `ok root -> root

let db_save_span = sec 15.0

let run config =

  let root_dir,jenga_root_path =
    match Config.external_jenga_root config with
    | None ->
      let dir = discover_root() in
      dir,Path.LR.local (Path.root_relative jenga_root_basename)

    | Some jenga_root ->
      let dir = Core.Std.Sys.getcwd () in
      Repo_root.set ~dir;
      dir, Path.LR.remote jenga_root

  in

  let start_dir = Path.cwd() in

  let log_filename = root_dir ^/ Path.log_basename in

  Message.init_logging config ~log_filename;

  trace "----------------------------------------------------------------------";
  trace "Root: %s" root_dir;
  trace "Start: %s" (Path.to_rrr_string start_dir);

  let progress = Build.Progress.create () in

  Rpc_server.go ~root_dir progress >>= fun () ->

  (* remember original CWD as start_dir, the chdir to ROOT
     This is necessary for internal actions, which assume this! *)
  Sys.chdir root_dir >>= fun () ->

  Persist.create_saving_periodically ~root_dir db_save_span
  >>= fun persist ->

  let fs_persist = Persist.fs_persist persist in
  Fs.create fs_persist >>= fun fs ->

  For_user.install_fs_for_user_rules fs;

  let top_level_demands =
    match Config.demands config with
    | [] -> [ Description.Dep.default ~dir:start_dir ]
    | demands -> List.map demands ~f:(Description.Dep.parse_string ~dir:start_dir)
  in

  let when_polling () =
    Persist.disable_periodic_saving_and_save_now persist
  in

  let when_rebuilding () =
    return (Persist.re_enable_periodic_saving persist)
  in

  let bs = Persist.build_persist persist in

  Build.build_forever config progress
    ~jenga_root_path ~top_level_demands fs bs ~when_polling ~when_rebuilding
  >>= fun () ->

  Message.flushed () >>= fun () ->
  return 0


let install_signal_handlers () =
  trace "install_signal_handlers..";
  Signal.handle Signal.terminating ~f:(fun s ->
    Message.message "handling signal: %s, calling shutdown" (Signal.to_string s);
    Heart.shutdown();
    Shutdown.shutdown 1;
  )

let main config =
  For_user.install_config_for_user_rules config;
  Deferred.unit >>> (fun () ->
    run config >>> (fun n ->
      Shutdown.shutdown n
    )
  );
  (
    match config.Config.report_long_cycle_times with
    | None -> ()
    | Some cutoff -> Scheduler.report_long_cycle_times ~cutoff ()
  );
  install_signal_handlers();
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())
