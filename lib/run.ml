open Core
open! Int.Replace_polymorphic_compare
open Async

let max_num_threads = 50

let max_num_threads =
  match Core.Sys.getenv "JENGA_MAX_NUM_THREADS" with
  | None -> max_num_threads
  | Some s ->
    let n = Int.of_string s in
    Core.Printf.eprintf "max_num_threads = %d\n%!" n;
    n

let trace fmt = ksprintf (fun string -> Message.trace "Run: %s" string) fmt

let db_save_span = sec 60.0

let db_save_span =
  match Core.Sys.getenv "JENGA_DB_SAVE_SPAN" with
  | None -> db_save_span
  | Some s ->
    let int = Int.of_string s in
    Core.Printf.eprintf "db_save_span = %d sec\n%!" int;
    sec (float int)

let run_once_async_is_started config ~start_dir ~root_dir ~jr_spec ~forker_args =
  Forker.init config ~args:forker_args
  >>| ok_exn
  >>= fun () ->
  Tenacious.init
    ~concurrency:(Option.value config.tenacious_concurrency
                    ~default:(config.j_number + 5));
  Fs.Ocaml_digest.init config;
  let progress = Progress.create config in
  Rpc_server.go config ~root_dir progress
  >>= fun () ->
  Persist.create_saving_periodically db_save_span >>= function
  | Error e ->
    Message.error "can't load persistent database: %s" (Error.to_string_hum e);
    Quit.quit Exit_code.persist_bad;
    Deferred.never()
  | Ok persist ->
    Fs.create config persist >>= fun fs ->
    (match Config.goals config with
     | [] -> return [ Goal.Alias (Alias.default ~dir:(Path.of_relative start_dir)) ]
     | goals -> Deferred.List.map goals ~f:(Goal.parse_string ~dir:start_dir))
    >>= fun top_level_goals ->
    let save_db_now () =
      Persist.disable_periodic_saving_and_save_now persist
    in
    let when_rebuilding () =
      return (Persist.re_enable_periodic_saving persist)
    in
    Build.build_forever config progress
      ~jr_spec ~top_level_goals fs persist ~save_db_now ~when_rebuilding

let install_signal_handlers () =
  trace "install_signal_handlers..";
  Signal.handle Signal.terminating ~f:(fun s ->
    Message.message "handling: %s; quitting" (Signal.to_string s);
    let exit_code = ! Build.exit_code_upon_control_c in
    Quit.quit exit_code
  )

(* for pre-init_logging errors *)
let error fmt = ksprintf (fun s -> Core.Printf.eprintf "%s\n%!" s) fmt

let configure_scheduler ~report_long_cycle_times =
  Option.iter report_long_cycle_times ~f:(fun cutoff ->
    Scheduler.report_long_cycle_times ~cutoff ()
  );
  Async.Scheduler.handle_thread_pool_stuck (fun ~stuck_for ->
    let sched = Async.Scheduler.t () in
    let completed_before =
      Async_unix.Async_unix_private.Thread_pool.num_work_completed sched.thread_pool in
    (* A 1ms pause shouldn't matter since this is called at most once per second, but it
       should be enough to convince the kernel to give other threads a chance to grab the
       ocaml lock. If threads are stuck for a different reason, then we will get the
       standard warning because of the call to the default handler below.

       This assumes async threads only grab the lock once, just before finishing their
       async job.  *)
    let time_to_wait_for = ref 0.001 in
    while !time_to_wait_for >. 0.; do
      time_to_wait_for := Core.Unix.nanosleep !time_to_wait_for;
    done;
    let completed_after =
      Async_unix.Async_unix_private.Thread_pool.num_work_completed sched.thread_pool in
    if completed_before = completed_after then
      Async.Scheduler.default_handle_thread_pool_stuck ~stuck_for;
    if Jenga_options.t.sigstop_on_thread_pool_stuck && Time_ns.Span.(stuck_for > of_sec 5.) then
      Signal.send_exn Signal.stop (`Pid (Unix.getpid ()));
  )
;;

let tune_gc config =
  let words_per_mb = 1024 * 1024 / 8 in
  let open Config.Gc in
  Gc.tune
    ~minor_heap_size: (words_per_mb * config.minor_heap_size)
    ~major_heap_increment: (words_per_mb * config.major_heap_increment)
    ~space_overhead: config.space_overhead
    ()

let main' jr_spec ~root_dir ~forker_args config =

  Path.Repo.set_root root_dir;
  tune_gc (Config.gc config);

  Special_paths.Dot_jenga.prepare ();

  let log_filename =
    Path.to_absolute_string (Path.of_relative Special_paths.Dot_jenga.debug)
  in
  Message.init_logging config ~log_filename;

  (* Remember the original start_dir, but then chdir to root_dir. This way jenga behaves
     the same regardless of what subdir it is started in. *)

  let start_dir =
    match
      Path.case (Path.of_absolute_string (Core.Sys.getcwd ()))
    with
    | `absolute _ ->
      failwith "start_dir, not under root_dir - impossible"
    | `relative s -> s
  in

  trace "----------------------------------------------------------------------";
  trace !"Root: %{Path.Abs}" root_dir;
  trace !"Start: %{Path.Rel}" start_dir;

  (* chdir before Forker.init so that we have the same cwd when using parallel forkers or
     not *)
  Core.Sys.chdir (Path.Abs.to_string root_dir);

  let pid_string () = Pid.to_string (Unix.getpid ()) in

  Message.message !"[%s] root=%{Path.Abs}, sys=%s, j=%d, f=%d (%s)"
    (pid_string()) root_dir
    System.description
    (Config.j_number config) (Config.f_number config)
    Version_util.version;

  let main () =
    don't_wait_for (
      Deferred.unit >>= fun () ->
      Quit.ignore_exn_while_quitting (fun () ->
        run_once_async_is_started config ~start_dir ~root_dir ~jr_spec ~forker_args;
      ) >>= never_returns
    );
    configure_scheduler ~report_long_cycle_times:config.report_long_cycle_times;
    install_signal_handlers()
  in

  never_returns (
    Scheduler.go_main ~max_num_threads ~raise_unhandled_exn:true ~main ()
  )

let main config =
  (* The jenga root discovery must run before we call Parallel.init *)
  match Config.path_to_jenga_conf config with
  | Some jenga_root ->
    let root_dir = Path.Abs.create (Core.Sys.getcwd ()) in
    let jenga_root = Path.relative_or_absolute ~dir:Path.the_root jenga_root in
    main' ~root_dir (Build.Jr_spec.Path jenga_root) config
  | None ->
    match Special_paths.discover_root () with
    | Ok root_dir ->
      main' ~root_dir Build.Jr_spec.In_root_dir config
    | Error e ->
      error "%s" (Error.to_string_hum e);
      Pervasives.exit Exit_code.cant_start
