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
  List.iter (* override a few of the env or jengaroot defaults *)
    [ "VERSION_UTIL_SUPPORT", "false"
    ; "STABLE_BUILD_INFO", "true"
    ; "TERM", "dumb"
    ] ~f:(fun (var, value) -> Unix.putenv ~key:var ~data:value);
  List.iter (* Avoid propagating user defaults into the benches *)
    [ "NODYNLINK"
    ; "DYNLINKABLE_CODE"
    ; "BUILD_PROFILE"
    ; "OCAML_VERSION"
    ; "WITH_FRAME_POINTERS"
    ; "WITH_NO_NAKED_POINTERS"
    ; "WITH_ARCH_32_BIT"
    ; "WITH_FLAMBDA"
    ; "WITH_SPACETIME"
    ; "LINK_EXECUTABLES"
    ; "X_LIBRARY_INLINING"
    ] ~f:Unix.unsetenv;
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

let run_jenga_and_save_metrics ~jenga_jengaroot_version ~bench_name ~exe ~targets ~env ~flags ~dst ~log =
  delete_metrics_file ()
  >>= fun () ->
  run_jenga ~exe ~args:(targets @ flags) ~env ~log
  >>= function
  | Ok () -> save_metrics ~jenga_jengaroot_version ~bench_name ~dst
  | Error e ->
    eprintf_progress "skipping failed build: %s\n" (Error.to_string_hum e);
    delete_metrics_file ()
;;

let run_polling_jenga_and_wait ~exe ~targets ~env ~flags ~log =
  delete_metrics_file ()
  >>= fun () ->
  let def = run_jenga ~exe ~args:("-P" :: targets @ flags) ~env ~log in
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

let clean_all () =
  Process.run_expect_no_output_exn ~prog:"hg" ~args:["clean"; "--all"] ()
;;

let clean_and_update ~rev ~log =
  eprintf_progress "setting up repository at %s\n" rev;
  Deferred.all_unit
    [ Process.run_exn ~prog:"hg" ~args:["revert"; "--all"] ()
      >>| Writer.write log
    ; Process.run_exn ~prog:"hg" ~args:["pull"] ()
      >>| Writer.write log
    ]
  >>= fun () ->
  clean_all ()
  >>= fun () ->
  Process.run_exn ~prog:"hg" ~args:["up"; "-r"; rev] ()
  >>| Writer.write log
  >>= fun () ->
  clean_all ()
;;

let setup_repository ~rev ~jenga_exe ~log =
  clean_and_update ~rev ~log
  >>= fun () ->
  eprintf_progress "figuring out jenga version\n";
  let jenga_exe =
    match jenga_exe with
    | `Path path -> path
    | `From_rev -> "/j/office/app/jenga/prod/bin/jenga"
    | `Build_at rev -> sprintf "../%s.exe" rev
  in
  Process.run_exn ~prog:jenga_exe ~args:["-version"] ()
  >>= fun version ->
  let _, version = String.lsplit2_exn version ~on:'_' in
  let version = String.chop_suffix_exn version ~suffix:"\n" in
  let jenga_jengaroot_version = String.prefix rev 12 ^ "/" ^ version in
  eprintf_progress "setup done\n";
  return (jenga_jengaroot_version, jenga_exe)
;;

let repeat n f =
  let rec loop i f =
    if i >= n then return ()
    else f i >>= fun () -> loop (i + 1) f
  in
  loop 0 f
;;

let non_polling_builds ~rev ~jenga_exe ~env ~flags ~dst ~log =
  setup_repository ~rev ~jenga_exe ~log
  >>= fun (jenga_jengaroot_version, jenga_exe) ->
  let jenga_jengaroot_version =
    jenga_jengaroot_version
    ^ (String.concat (List.map env ~f:(fun (a, b) -> sprintf " %s=%s" a b)))
    ^ (String.concat (List.map flags ~f:(sprintf " %s")))
  in
  Deferred.List.iter
    [ "core_kernel", ["lib/core_kernel/src/core_kernel.cmxa"], []
    ; "lib", ["lib"], []
    ; "full tree", [".DEFAULT"], [("X_LIBRARY_INLINING", "true")]
    ]
    ~f:(fun (bench_name, targets, bench_env) ->
      let env = env @ bench_env in
      let total = 2 in
      repeat total (fun i ->
        eprintf_progress "bench of non-polling mode %s %d/%d\n" bench_name (i + 1) total;
        clean_all ()
        >>= fun () ->
        run_jenga_and_save_metrics ~exe:jenga_exe ~jenga_jengaroot_version ~env ~flags ~dst ~log
          ~bench_name:(bench_name ^ " from scratch") ~targets
        >>= fun () ->
        repeat 2 (fun _ ->
          run_jenga_and_save_metrics ~exe:jenga_exe ~jenga_jengaroot_version ~env ~flags ~dst ~log
            ~bench_name:(bench_name ^ " null build") ~targets)))
;;

let polling_builds ~rev ~jenga_exe ~env ~flags ~dst ~log =
  setup_repository ~rev ~jenga_exe ~log
  >>= fun (jenga_jengaroot_version, jenga_exe) ->
  let jenga_jengaroot_version =
    jenga_jengaroot_version
    ^ (String.concat (List.map env ~f:(fun (a, b) -> sprintf " %s=%s" a b)))
    ^ (String.concat (List.map flags ~f:(sprintf " %s")))
  in
  clean_all ()
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
      [("X_LIBRARY_INLINING", "true")]
    ] ~f:(fun (bench_name, targets, files, bench_env) ->
      let env = env @ bench_env in
      eprintf_progress "bench of polling mode %s\n" bench_name;
      run_polling_jenga_and_wait ~exe:jenga_exe ~targets ~env ~flags ~log
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

type jenga_invocation_syntax =
  { revision : string
  ; exe : string sexp_option
  ; env : (string * string) sexp_list
  ; flags : string sexp_list
  } [@@deriving of_sexp]

type jenga_invocation =
  { revision : string
  ; exe : [ `From_rev | `Path of string | `Build_at of string ]
  ; env : (string * string) sexp_list
  ; flags : string sexp_list
  }

type config =
  { feature : string sexp_option
  ; jengas : jenga_invocation_syntax sexp_list
  ; mode : [ `polling | `non_polling ]
  ; exe : string sexp_option
  } [@@deriving of_sexp]

let exe_of_syntax str =
  if String.mem str '/'
  then `Path str
  else `Build_at str
;;

let feature_and_exe feature exe =
  Deferred.both
    (Process.run_exn ~prog:"fe" ~args:["show"; feature; "-base"] ())
    (Process.run_exn ~prog:"fe" ~args:["show"; feature; "-tip"] ())
  >>| fun (base, tip) ->
  let exe =
    match exe with
    | None -> `From_rev
    | Some str -> exe_of_syntax str
  in
  { revision = String.strip base; exe; env = []; flags = [] },
  { revision = String.strip tip; exe; env = []; flags = [] }
;;

let jenga_invocation (jenga_invocation_syntax : jenga_invocation_syntax) exe =
  let default_exe =
    match exe with
    | None -> `From_rev
    | Some str -> exe_of_syntax str
  in
  { revision = jenga_invocation_syntax.revision
  ; exe = (match jenga_invocation_syntax.exe with
           | None -> default_exe
           | Some str -> exe_of_syntax str)
  ; env = jenga_invocation_syntax.env
  ; flags = jenga_invocation_syntax.flags
  }
;;

let build_all_needed_jengas jengas ~log =
  let revs =
    List.filter_map jengas ~f:(fun (invocation : jenga_invocation) ->
      match invocation.exe with
      | `Build_at rev -> Some rev
      | `Path _ | `From_rev -> None)
    |> List.dedup ~compare:String.compare
  in
  Deferred.List.iter revs ~f:(fun rev ->
    eprintf_progress "building jenga at rev %s\n" rev;
    clean_and_update ~rev ~log
    >>= fun () ->
    run_jenga ~exe:"/j/office/app/jenga/prod/bin/jenga"
      ~args:["app/jenga/bin/jenga.exe"]
      ~env:[ "VERSION_UTIL_SUPPORT", "true"
           ; "BUILD_PROFILE", "fast-exe"]
      ~log
    >>| ok_exn
    >>= fun () ->
    Process.run_expect_no_output_exn ~prog:"cp"
      ~args:["app/jenga/bin/jenga.exe"; sprintf "../%s.exe" rev] ()
  )
;;

let command =
  let open Command.Let_syntax in
  let return = Async.Std.return in
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
        Unix.getlogin ()
        >>= fun user ->
        let log = sprintf "/tmp/jenga-bench-%s.log" user in
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
            begin match config.feature, config.jengas with
            | Some _, _ :: _ ->
              failwith "feature option is not compatible with the jengas options"
            | Some feature, [] ->
              (feature_and_exe feature config.exe
               >>| fun (jenga1, jenga2) -> [ jenga1; jenga2 ])
            | None, [] ->
              failwith "missing feature option"
            | None, (_ :: _ as jengas) ->
              return (List.map jengas ~f:(fun x -> jenga_invocation x config.exe))
            end
            >>= fun jengas ->
            build_all_needed_jengas jengas ~log
            >>= fun () ->
            let builds =
              match config.mode with
              | `polling -> polling_builds
              | `non_polling -> non_polling_builds
            in
            Deferred.List.iter jengas ~f:(fun { revision; exe; env; flags } ->
              builds ~dst ~log ~rev:revision ~env ~flags ~jenga_exe:exe
            )
          )
    ]
;;
