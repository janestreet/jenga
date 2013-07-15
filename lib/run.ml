
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Build_state = Build.Persist

let trace fmt = ksprintf (fun string -> Message.trace "Run: %s" string) fmt

let db_save_span = sec 15.0

let run_once_async_is_started config ~start_dir ~root_dir ~jenga_root_path =
  trace "----------------------------------------------------------------------";
  trace "Root: %s" root_dir;
  trace "Start: %s" (Path.to_string start_dir);
  Persist.create_saving_periodically ~root_dir db_save_span
  >>= fun persist ->
  let fs_persist = Persist.fs_persist persist in
  Fs.create fs_persist >>= fun fs ->
  let progress = Build.Progress.create fs in
  Rpc_server.go ~root_dir progress >>= fun () ->
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

(* for pre-init_logging errors *)
let error fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

let main config =
  For_user.install_config_for_user_rules config;

  (* The jenga root discovery must run before we call Parallel.init *)
  let root_dir,jenga_root_path =
    match Config.external_jenga_root config with
    | Some jenga_root ->
      let dir = Core.Std.Sys.getcwd () in
      Path.Root.set ~dir;
      dir, Path.X.of_absolute (Path.Abs.create jenga_root)
    | None ->
      begin
        match Path.Root.discover() with
        | `ok ->
          let dir = Path.to_absolute_string Path.the_root in
          dir, Path.X.of_relative (Path.root_relative Misc.jenga_root_basename)
        | `cant_find_root ->
          error "Cant find '%s' in start-dir or any ancestor dir"
            Misc.jenga_root_basename;
          Pervasives.exit 1
      end
  in

  let log_filename = root_dir ^/ Path.log_basename in
  Message.init_logging config ~log_filename;

  (* Remember the original start_dir, but then chdir to root_dir. This way jenga behaves
     the same regardless of what subdir it is started in. *)

  let start_dir =
    match (Path.create_from_absolute (Core.Std.Sys.getcwd ())) with
    | None -> failwith "start_dir, not under root_dir - impossible"
    | Some x -> x
  in

  Core.Std.Sys.chdir root_dir;

  (* Must do the chdir before Parallel.init is called, so that we have the same cwd when
     using parallel forkers or not *)
  Parallel.Std.Parallel.init();
  Forker.init config;

  (* Only after Parallel.init is called may we start async *)
  Deferred.unit >>> (fun () ->
    run_once_async_is_started config ~start_dir ~root_dir ~jenga_root_path >>> (fun n ->
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
