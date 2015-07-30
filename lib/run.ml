
open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

let max_num_threads = 50

let max_num_threads =
  match Core.Std.Sys.getenv "JENGA_MAX_NUM_THREADS" with
  | None -> max_num_threads
  | Some s ->
    let n = Int.of_string s in
    Core.Std.Printf.eprintf "max_num_threads = %d\n%!" n;
    n

let trace fmt = ksprintf (fun string -> Message.trace "Run: %s" string) fmt

let db_save_span = sec 60.0

let db_save_span =
  match Core.Std.Sys.getenv "JENGA_DB_SAVE_SPAN" with
  | None -> db_save_span
  | Some s ->
    let int = Int.of_string s in
    Core.Std.Printf.eprintf "db_save_span = %d sec\n%!" int;
    sec (float int)

let run_once_async_is_started config ~start_dir ~root_dir ~jr_spec =
  Forker.init config;
  Fs.Ocaml_digest.init config;
  Persist.create_saving_periodically db_save_span >>= function
  | Error e ->
    Message.error "can't load persistent database: %s" (Error.to_string_hum e);
    Quit.quit Exit_code.persist_bad;
    Deferred.never()
  | Ok persist ->
  Fs.create config persist >>= fun fs ->
  let progress = Progress.create config in
  Rpc_server.go config ~root_dir progress
  >>= fun () ->
  (match Config.demands config with
   | [] -> return [ Goal.Alias (Alias.default ~dir:(Path.of_relative start_dir)) ]
   | demands -> Deferred.List.map demands ~f:(Goal.parse_string ~dir:start_dir))
  >>= fun top_level_demands ->
  let save_db_now () =
    Persist.disable_periodic_saving_and_save_now persist
  in
  let when_rebuilding () =
    return (Persist.re_enable_periodic_saving persist)
  in
  Build.build_forever config progress
    ~jr_spec ~top_level_demands fs persist ~save_db_now ~when_rebuilding

let install_signal_handlers () =
  trace "install_signal_handlers..";
  Signal.handle Signal.terminating ~f:(fun s ->
    Message.message "handling: %s; quitting" (Signal.to_string s);
    let exit_code = ! Build.exit_code_upon_control_c in
    Quit.quit exit_code
  )

(* for pre-init_logging errors *)
let error fmt = ksprintf (fun s -> Core.Std.Printf.eprintf "%s\n%!" s) fmt

module For_user = struct

  let the_installed_config = ref None
  let install_config_for_user_rules config = the_installed_config := Some config

  let config() =
    match !the_installed_config with
    | Some config -> config
    | None -> assert false

end

let configure_scheduler ~report_long_cycle_times =
  Option.iter report_long_cycle_times ~f:(fun cutoff ->
    Scheduler.report_long_cycle_times ~cutoff ()
  );
  Async.Std.Scheduler.handle_thread_pool_stuck (fun ~stuck_for ->
    let sched = Async.Std.Scheduler.t () in
    let completed_before = Async_unix.Thread_pool.num_work_completed sched.thread_pool in
    (* A 1ms pause shouldn't matter since this is called at most once per second, but it
       should be enough to convince the kernel to give other threads a chance to grab the
       ocaml lock. If threads are stuck for a different reason, then we will get the
       standard warning because of the call to the default handler below.

       This assumes async threads only grab the lock once, just before finishing their async job.
    *)
    let time_to_wait_for = ref 0.001 in
    while !time_to_wait_for >. 0.; do
      time_to_wait_for := Core.Std.Unix.nanosleep !time_to_wait_for;
    done;
    let completed_after = Async_unix.Thread_pool.num_work_completed sched.thread_pool in
    if completed_before = completed_after then
      Async.Std.Scheduler.default_handle_thread_pool_stuck ~stuck_for;
    if Jenga_options.t.sigstop_on_thread_pool_stuck && Time.Span.(stuck_for > sec 5.) then
      Signal.send_exn Signal.stop (`Pid (Unix.getpid ()));
  )
;;

let main config =
  For_user.install_config_for_user_rules config;

  (* The jenga root discovery must run before we call Parallel.init *)
  let root_dir,jr_spec =
    match Config.path_to_jenga_conf config with
    | Some jenga_root ->
      let dir = Core.Std.Sys.getcwd () in
      Path.Repo.set_root ~dir:(Path.Abs.create dir);
      dir, Build.Jr_spec.path (Path.of_absolute (Path.Abs.create jenga_root))
    | None ->
      begin
        match Special_paths.discover_root () with
        | Ok () ->
          let dir = Path.Abs.to_string (Path.Repo.root ()) in
          dir, Build.Jr_spec.in_root_dir
        | Error e ->
          error "%s" (Error.to_string_hum e);
          Pervasives.exit Exit_code.cant_start
      end
  in

  Special_paths.Dot_jenga.prepare ();

  let log_filename =
    Path.to_absolute_string (Path.of_relative Special_paths.Dot_jenga.log)
  in
  Message.init_logging config ~log_filename;

  (* Remember the original start_dir, but then chdir to root_dir. This way jenga behaves
     the same regardless of what subdir it is started in. *)

  let start_dir =
    match
      Path.case (Path.of_absolute_string (Core.Std.Sys.getcwd ()))
    with
    | `absolute _ ->
      failwith "start_dir, not under root_dir - impossible"
    | `relative s -> s
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
        Async_parallel_deprecated.Std.Parallel.init();
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
      ) >>= never_returns
    );
    configure_scheduler ~report_long_cycle_times:config.report_long_cycle_times;
    install_signal_handlers()
  in


  never_returns (
    Scheduler.go_main ~max_num_threads ~raise_unhandled_exn:true ~main ()
  )
