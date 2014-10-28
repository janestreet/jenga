
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Build_state = Build.Persist

let max_num_threads = 50

let max_num_threads =
  match Core.Std.Sys.getenv "JENGA_MAX_NUM_THREADS" with
  | None -> max_num_threads
  | Some s ->
    let n = Int.of_string s in
    Printf.eprintf "max_num_threads = %d\n%!" n;
    n

let trace fmt = ksprintf (fun string -> Message.trace "Run: %s" string) fmt

let db_save_span = sec 60.0

let db_save_span =
  match Core.Std.Sys.getenv "JENGA_DB_SAVE_SPAN" with
  | None -> db_save_span
  | Some s ->
    let int = Int.of_string s in
    Printf.eprintf "db_save_span = %d sec\n%!" int;
    sec (float int)

let run_once_async_is_started config ~start_dir ~root_dir ~jr_spec =
  Forker.init config;
  Fs.Digester.init config;
  Persist.create_saving_periodically ~root_dir db_save_span >>= fun persist ->
  Fs.create config (Persist.fs_persist persist) >>= fun fs ->
  let progress = Progress.create config in
  Rpc_server.go config ~root_dir progress >>= fun () ->
  let top_level_demands =
    match Config.demands config with
    | [] -> [ Goal.Alias (Alias.default ~dir:(Path.of_relative start_dir)) ]
    | demands -> List.map demands ~f:(Goal.parse_string ~dir:start_dir)
  in
  let save_db_now () =
    Persist.disable_periodic_saving_and_save_now persist
  in
  let when_rebuilding () =
    return (Persist.re_enable_periodic_saving persist)
  in
  let bs = Persist.build_persist persist in
  let pq = Persist.quality persist in
  Build.build_forever config progress
    ~jr_spec ~top_level_demands fs bs pq ~save_db_now ~when_rebuilding

let install_signal_handlers () =
  trace "install_signal_handlers..";
  Signal.handle Signal.terminating ~f:(fun s ->
    Message.message "handling: %s; quitting" (Signal.to_string s);
    let exit_code = ! Build.exit_code_upon_control_c in
    Quit.quit exit_code
  )

(* for pre-init_logging errors *)
let error fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt

module For_user = struct

  let the_installed_config = ref None
  let install_config_for_user_rules config = the_installed_config := Some config

  let config() =
    match !the_installed_config with
    | Some config -> config
    | None -> assert false

end

let main config =
  For_user.install_config_for_user_rules config;

  (* The jenga root discovery must run before we call Parallel.init *)
  let root_dir,jr_spec =
    match Config.path_to_jenga_conf config with
    | Some jenga_root ->
      let dir = Core.Std.Sys.getcwd () in
      Path.Root.set ~dir;
      dir, Build.Jr_spec.path (Path.of_absolute (Path.Abs.create jenga_root))
    | None ->
      begin
        match Path.Root.discover() with
        | `ok ->
          let dir = Path.Rel.to_absolute_string Path.Rel.the_root in
          dir, Build.Jr_spec.in_root_dir
        | `cant_find_root ->
          error "Cant find '%s' or '%s' in start-dir or any ancestor dir"
            Misc.jenga_conf_basename
            Misc.jenga_root_basename;
          Pervasives.exit Exit_code.cant_start
      end
  in

  let log_filename = root_dir ^/ Misc.log_basename in
  Message.init_logging config ~log_filename;

  (* Remember the original start_dir, but then chdir to root_dir. This way jenga behaves
     the same regardless of what subdir it is started in. *)

  let start_dir =
    match (Path.Rel.create_from_absolute (Core.Std.Sys.getcwd ())) with
    | None -> failwith "start_dir, not under root_dir - impossible"
    | Some x -> x
  in

  trace "----------------------------------------------------------------------";
  trace "Root: %s" root_dir;
  trace "Start: %s" (Path.Rel.to_string start_dir);

  Core.Std.Sys.chdir root_dir;

  let pid_string () = Pid.to_string (Unix.getpid ()) in

  Message.message "[%s] root=%s, sys=%s, j=%d, f=%d"
    (pid_string()) root_dir
    System.description
    (Config.j_number config) (Config.f_number config);

  (* Must do the chdir before Parallel.init is called, so that we have the same cwd when
     using parallel forkers or not *)

  let config =
    if Int.(Config.f_number config >= 0) then config else (
      Message.error "Ignoring negative value to -f flag; treating as 0";
      {config with Config. f_number = 0}
    )
  in

  let config =
    try
      if Int.(Config.f_number config > 0) then (
        Async_parallel.Std.Parallel.init();
      );
      config
    with | exn ->
      Message.error "Parallel.init (needed for forkers) threw exception:\n%s"
        (Exn.to_string exn);
      Message.message "INFO: Avoid separate forker processes with: '-f 0'";
      {config with Config. f_number = 0}
  in

  (* Only after Parallel.init is called may we start async *)
  let main () =
    don't_wait_for (
      Deferred.unit >>= fun () ->
      Quit.ignore_exn_while_quitting (fun () ->
        run_once_async_is_started config ~start_dir ~root_dir ~jr_spec;
      )
    );
    (
      match config.Config.report_long_cycle_times with
      | None -> ()
      | Some cutoff -> Scheduler.report_long_cycle_times ~cutoff ()
    );
    install_signal_handlers()
  in


  never_returns (
    Scheduler.go_main ~max_num_threads ~raise_unhandled_exn:true ~main ()
  )
