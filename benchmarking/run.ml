open Core.Std
open Async.Std

let save_metrics ~jenga_jengaroot_version ~bench_name ~dst =
  let started_at = Time.now () in
  Deferred.repeat_until_finished ()
    (fun () ->
       Deferred.Or_error.try_with (fun () -> Reader.file_contents ".jenga/metrics")
       >>= fun result ->
       let result =
         (* Let's avoid reading the file between its creation and jenga filling it *)
         match result with
         | Error _ as e -> e
         | Ok contents as ok ->
           if String.mem contents '\n'
           then ok
           else Or_error.errorf "the metrics file only contains %S" contents
       in
       match result with
       | Error e ->
         if Time.diff (Time.now ()) started_at > Time.Span.of_min 45.
         then Error.raise e
         else after (sec 0.1) >>| fun () -> `Repeat ()
       | Ok contents -> return (`Finished contents))
  >>= fun contents ->
  Writer.with_file dst ~append:true ~f:(fun w ->
    Writer.write w (String.concat ~sep:"%" [jenga_jengaroot_version; bench_name; contents]);
    return ())
;;

let start = Time.now ()
let eprintf_progress =
  fun fmt ->
    ksprintf (fun str ->
      let from_start = Time.diff (Time.now ()) start in
      let decimals =
        match Time.Span.to_unit_of_time from_start with
        | Nanosecond | Microsecond | Millisecond -> 0
        | Second -> 1
        | Minute | Hour | Day -> 2
      in
      Writer.writef (force (Writer.stderr)) "%5s %s"
        (Time.Span.to_string_hum from_start ~decimals) str)
      fmt
;;

let wait_for_compact_and_db_save () =
  eprintf_progress "waiting for compact/db save...%!";
  Clock.after (Time.Span.of_sec 80.)
  >>| fun () ->
  eprintf " done\n";
;;

let delete_metrics_file () =
  Process.run_expect_no_output_exn ~prog:"rm" ~args:["-f"; ".jenga/metrics"] ()
;;

let make_change ~file ~what_to_append =
  delete_metrics_file ()
  >>= fun () ->
  Process.run_expect_no_output_exn ~prog:"truncate" ~args:["--size"; "0"; ".jenga/debug"] ()
  >>= fun () ->
  Writer.with_file file ~append:true ~f:(fun w -> Writer.write w what_to_append; return ())
;;

let make_fake_change ~file =
  make_change ~file ~what_to_append:"\n"
;;

let make_actual_change ~file =
  make_change ~file
    ~what_to_append:"let () = if int_of_string \"0\" <> 0 then raise Exit"
;;


let dump_reader_in_writer reader writer =
  Writer.transfer writer (Reader.pipe reader)
    (Writer.write writer)
;;

let run_jenga ~exe ~args ~env ~log =
  let env =
    [ "VERSION_UTIL_SUPPORT", "false"
    ; "STABLE_BUILD_INFO", "true"
    ; "TERM", "dumb"
    ; "NODYNLINK", "true"
    ; "DYNLINKABLE_CODE", "false"
    ; "BUILD_PROFILE", "fast-exe"
    ; "OCAML_VERSION", "default"
    ; "WITH_FRAME_POINTERS", "false"
    ; "WITH_NO_NAKED_POINTERS", "false"
    ; "WITH_ARCH_32_BIT", "false"
    ; "WITH_FLAMBDA", "false"
    ; "WITH_SPACETIME", "false"
    ; "LINK_EXECUTABLES", "true"
    ] @ env
  in
  let args = "-show-mem" :: args in
  let prog = exe in
  Process.create_exn ~env:(`Extend env) ~prog ~args ()
  >>= fun p ->
  Deferred.both
    (Deferred.all_unit
       [ dump_reader_in_writer (Process.stdout p) log
       ; dump_reader_in_writer (Process.stderr p) log
       ])
    (Process.wait p)
  >>| fun ((), wait_result) ->
  match wait_result with
  | Ok () -> Ok ()
  | Error err ->
    error_s [%sexp
      { prog = (prog : string)
      ; args = (args : string list)
      ; exit_status = (err : Unix.Exit_or_signal.error)
      ; log = (Fd.info (Writer.fd log) : Info.t)
      }]
;;

let run_jenga_and_save_metrics ~jenga_jengaroot_version ~bench_name ~exe ~targets ~env ~dst ~log =
  delete_metrics_file ()
  >>= fun () ->
  run_jenga ~exe ~args:targets ~env ~log
  >>= function
  | Ok () -> save_metrics ~jenga_jengaroot_version ~bench_name ~dst
  | Error e ->
    eprintf_progress "skipping failed build: %s\n" (Error.to_string_hum e);
    delete_metrics_file ()

;;

let run_polling_jenga_and_wait ~exe ~targets ~env ~log =
  delete_metrics_file ()
  >>= fun () ->
  let def = run_jenga ~exe ~args:("-P" :: targets) ~env ~log in
  save_metrics ~jenga_jengaroot_version:"" ~bench_name:"" ~dst:"/dev/null"
  >>| fun () ->
  (def >>| ok_exn) (* we don't care about this error, as it doesn't say
                      whether we successfully reached the polling build *)
;;

let setup_limits () =
  In_thread.run (fun () ->
    let open Core.Std in
    Unix.RLimit.set Unix.RLimit.stack
      { cur = Limit (Int64.of_int (20_000 * 1024)); max = Infinity })
;;

let setup_repository ~rev ~jenga_exe ~log =
  eprintf_progress "setting up repository at %s\n" rev;
  Deferred.all_unit
    [ Process.run_exn ~prog:"hg" ~args:["revert"; "--all"] ()
      >>| Writer.write log
    ; Process.run_exn ~prog:"hg" ~args:["pull"] ()
      >>| Writer.write log
    ]
  >>= fun () ->
  Process.run_exn ~prog:"hg" ~args:["up"; "-r"; rev] ()
  >>| Writer.write log
  >>= fun () ->
  eprintf_progress "figuring out jenga version\n";
  let jenga_exe =
    match jenga_exe with
    | `This path -> path
    | `From_rev -> "/j/office/app/jenga/prod/bin/jenga"
  in
  Process.run_exn ~prog:jenga_exe ~args:["-version"] ()
  >>= fun version ->
  let _, version = String.lsplit2_exn version ~on:'_' in
  let version = String.chop_suffix_exn version ~suffix:"\n" in
  let jenga_jengaroot_version = String.prefix rev 12 ^ "/" ^ version in
  eprintf_progress "setup done\n";
  return (jenga_jengaroot_version, jenga_exe)
;;

let from_scrach () =
  Process.run_expect_no_output_exn ~prog:"hg" ~args:["clean"; "--all"] ()
;;

let repeat n f =
  let rec loop i f =
    if i >= n then return ()
    else f i >>= fun () -> loop (i + 1) f
  in
  loop 0 f
;;

let non_polling_builds ~rev ~jenga_exe ~dst ~log =
  setup_repository ~rev ~jenga_exe ~log
  >>= fun (jenga_jengaroot_version, jenga_exe) ->
  Deferred.List.iter
    [ "core_kernel", ["lib/core_kernel/src/core_kernel.cmxa"], []
    ; "lib", ["lib"], []
    ; "full tree", [".DEFAULT"], ["X_LIBRARY_INLINING", "true"]
    ]
    ~f:(fun (bench_name, targets, env) ->
      let total = 2 in
      repeat total (fun i ->
        eprintf_progress "bench of non-polling mode %s %d/%d\n" bench_name (i + 1) total;
        from_scrach ()
        >>= fun () ->
        run_jenga_and_save_metrics ~exe:jenga_exe ~jenga_jengaroot_version ~env ~dst ~log
          ~bench_name:(bench_name ^ " from scratch") ~targets
        >>= fun () ->
        repeat 2 (fun _ ->
          run_jenga_and_save_metrics ~exe:jenga_exe ~jenga_jengaroot_version ~env ~dst ~log
            ~bench_name:(bench_name ^ " null build") ~targets)))
;;

let polling_builds ~rev ~jenga_exe ~dst ~log =
  setup_repository ~rev ~jenga_exe ~log
  >>= fun (jenga_jengaroot_version, jenga_exe) ->
  from_scrach ()
  >>= fun () ->
  Deferred.List.iter
    [ "core_kernel",
      ["lib/core_kernel/src/core_kernel.cmxa"],
      ["lib/core_kernel/src/core_list.ml"],
      []
    ; "lib",
      ["lib"],
      ["lib/nyse_utp_protocol/src/message.ml"; "lib/core_kernel/src/core_list.ml"],
      []
    ; "full tree",
      [".DEFAULT"],
      ["lib/core_kernel/src/core_list.ml"; "app/hydra/bin/hydra.ml"],
      ["X_LIBRARY_INLINING", "true"]
    ] ~f:(fun (bench_name, targets, files, env) ->
      eprintf_progress "bench of polling mode %s\n" bench_name;
      run_polling_jenga_and_wait ~exe:jenga_exe ~targets ~env ~log
      >>= fun jenga_is_dead ->
      Deferred.List.iter files ~f:(fun file ->
        let total = 4 in
        repeat total (fun i ->
          eprintf_progress "bench %s %d/%d\n" file (i + 1) total;
          Deferred.List.iter
            [ make_fake_change, "fake change"
            ; make_actual_change, "actual change"
            ] ~f:(fun (make_change, descr) ->
              wait_for_compact_and_db_save ()
              >>= fun () ->
              make_change ~file
              >>= fun () ->
              save_metrics
                ~jenga_jengaroot_version
                ~bench_name:(bench_name ^ " " ^ descr ^ " " ^ file)
                ~dst)))
      >>= fun () ->
      Process.run_expect_no_output_exn ~prog:jenga_exe ~args:["stop"] ()
      >>= fun () ->
      jenga_is_dead
      >>= fun () ->
      Process.run_exn ~prog:"hg" ~args:["revert"; "--all"] ()
      >>| Writer.write log
    )
;;

type revision =
  { revision : string
  ; exe : string sexp_option
  } [@@deriving of_sexp]

type config =
  { feature : string sexp_option
  ; base : revision sexp_option
  ; tip : revision sexp_option
  ; mode : [ `polling | `non_polling ]
  ; exe : string sexp_option
  } [@@deriving of_sexp]

let feature_and_exe feature exe =
  Deferred.both
    (Process.run_exn ~prog:"fe" ~args:["show"; feature; "-base"] ())
    (Process.run_exn ~prog:"fe" ~args:["show"; feature; "-tip"] ())
  >>| fun (base, tip) ->
  let exe =
    match exe with
    | None -> `From_rev
    | Some path -> `This path
  in
  String.strip base, exe, String.strip tip, exe
;;

let base_tip_and_exe (base : revision) (tip : revision) exe =
  let default_exe =
    match exe with
    | None -> `From_rev
    | Some path -> `This path
  in
  let base_exe =
    match base.exe with
    | Some path -> `This path
    | None -> default_exe
  in
  let tip_exe =
    match tip.exe with
    | Some path -> `This path
    | None -> default_exe
  in
  return (base.revision, base_exe, tip.revision, tip_exe)
;;

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Runs benches and outputs the result on stdout"
    [%map_open
      let can_delete_everything =
        flag "-can-delete-everything" no_arg
          ~doc:" to acknowledge that this command can blow your files away"
      and config =
        anon ("((feature ..)(mode (polling|non_polling))(exe (some a.exe)))" %: string)
      in fun () ->
        let config = Sexp.of_string_conv_exn config [%of_sexp: config] in
        let log = "/tmp/jenga-bench.log" in
        Sys.getcwd ()
        >>= fun cwd ->
        Sys.file_exists_exn ".hg"
        >>= function
        | false ->
          failwithf "%s expects to run at the root of a repository" Sys.argv.(0) ()
        | true ->
          if not can_delete_everything
          then failwith "This exe is going to run [hg clean --all] in the current \
                         repository, so make sure that's fine and rerun it \
                         -can-delete-everything";
          Writer.with_file ~append:true log ~f:(fun log ->
            Writer.write log (sprintf !">>>>>>>>>>>> %{Sexp}\n"
                                [%sexp { time = (Time.now () : Time.t)
                                       ; cwd = (cwd : string)
                                       ; cmdline = (Sys.argv : string array) }]);
            setup_limits ()
            >>= fun () ->
            let dst = "/dev/stdout" in
            let base_and_tip =
              match config.base, config.tip with
              | None, None -> None
              | Some _, None -> failwith "base option provided but tip option is missing"
              | None, Some _ -> failwith "tip option provided but base option is missing"
              | Some base, Some tip -> Some (base, tip)
            in
            begin match config.feature, base_and_tip with
            | Some _, Some _ ->
              failwith "feature option is not compatible with the base and tip options"
            | Some feature, None ->
              feature_and_exe feature config.exe
            | None, None ->
              failwith "missing feature option"
            | None, Some (base, tip) ->
              base_tip_and_exe base tip config.exe
            end
            >>= fun (base, base_exe, tip, tip_exe) ->
            let builds =
              match config.mode with
              | `polling -> polling_builds
              | `non_polling -> non_polling_builds
            in
            builds ~dst ~log ~rev:base ~jenga_exe:base_exe
            >>= fun () ->
            builds ~dst ~log ~rev:tip ~jenga_exe:tip_exe
          )
    ]
;;
