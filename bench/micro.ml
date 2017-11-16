open Core
open Async
open String.Replace_polymorphic_compare

let empty_file file = Writer.with_file ~f:(fun _ -> return ()) file

let range n1 n2 = List.init (n2 - n1 + 1) ~f:((+) n1)

let cleanup_files () =
  let rec loop ~dir =
    Sys.ls_dir dir
    >>= fun files ->
    Deferred.List.iter files ~f:(fun file ->
      (* leave .jenga alone *)
      if not (String.is_prefix file ~prefix:".") && String.(<>) file "jengaroot.ml"
      then
        Unix.stat (dir ^/ file)
        >>= fun st ->
        match st.kind with
        | `Directory ->
          loop ~dir:(dir ^/ file)
          >>= fun () ->
          Unix.rmdir (dir ^/ file)
        | _ -> Unix.unlink (dir ^/ file)
      else return ())
  in
  loop ~dir:"."
;;

let all_benches = Queue.create ()
let all_results = Queue.create ()
let register_bench name f =
  let wrapped_f ~jenga ~log =
    cleanup_files ()
    >>= fun () ->
    Deferred.Or_error.try_with (fun () -> f ~name ~jenga ~log)
    >>| fun result ->
    ok_exn (Or_error.tag result ~tag:(sprintf "in bench %s" name))
  in
  Queue.enqueue all_benches (name, wrapped_f)

let bench ~jenga ~log name n targets =
  Sys.ls_dir ".jenga"
  >>= fun files ->
  Deferred.List.iter files ~f:(fun file ->
    if String.is_prefix file ~prefix:"db-v" || file = "metrics"
    then Unix.unlink (".jenga" ^/ file)
    else return ())
  >>= fun () ->
  Run.run_jenga ~exe:jenga ~args:targets ~env:[] ~log
  >>| ok_exn
  >>= fun () ->
  Reader.file_lines ".jenga/metrics"
  >>| List.last_exn
  >>= fun metrics ->
  Queue.enqueue all_results (name, n, metrics);
  return ()
;;

let copy_and_precompile_jengaroot ~jenga ~initial_dir =
  Unix.mkdir ".jenga"
  >>= fun () ->
  Process.run_expect_no_output_exn
    ~prog:"cp" ~args:[initial_dir ^/ "../tests/shared-root.ml"; "jengaroot.ml" ] ()
  >>= fun () ->
  empty_file "foo"
  >>= fun () ->
  Process.run_exn ~prog:jenga ~args:[ "foo" ] ()
  >>= fun (_ : string) ->
  return ()
;;

let () = register_bench "one-source-n-others" (fun ~name ~jenga ~log ->
  empty_file "rule"
  >>= fun () ->
  Deferred.List.iter [ 1; 2; 3; 4; 50 ] ~f:(fun n ->
    Deferred.List.iter (range 1 n) ~f:(ksprintf empty_file "in%d")
    >>= fun () ->
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      bench ~jenga ~log name n [ "in1" ])
  )
)

let () = register_bench "n-sources" (fun ~name ~jenga ~log ->
  empty_file "rule"
  >>= fun () ->
  Deferred.List.iter (range 1 50) ~f:(ksprintf empty_file "in%d")
  >>= fun () ->
  Deferred.List.iter [ 1; 2; 3; 4; 50 ] ~f:(fun n ->
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      bench ~jenga ~log name n
        (List.map (range 1 n) ~f:(sprintf "in%d")))
  )
)

let () = register_bench "n-one-target-one-source" (fun ~name ~jenga ~log ->
  Deferred.List.iter (range 1 50) ~f:(ksprintf empty_file "in%d")
  >>= fun () ->
  Deferred.List.iter [ 1; 2; 3; 4; 50 ] ~f:(fun n ->
    Writer.save_sexps "rule"
      (List.map (range 1 n) ~f:(fun i ->
         [%sexp { targets = [ (sprintf "out%d" i : string) ]
                ; deps = [ (sprintf "in%d" i : string) ]
                ; command = (sprintf "cp in%d out%d" i i : string)
                }]))
    >>= fun () ->
    let targets = List.map (range 1 n) ~f:(sprintf "out%d") in
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      bench ~jenga ~log name n targets
      >>= fun () ->
      Deferred.List.iter targets ~f:Unix.remove))
)

let () = register_bench "n-one-target-same-source" (fun ~name ~jenga ~log ->
  empty_file "in1"
  >>= fun () ->
  Deferred.List.iter [ 1; 2; 3; 4; 50 ] ~f:(fun n ->
    Writer.save_sexps "rule"
      (List.map (range 1 n) ~f:(fun i ->
         [%sexp { targets = [ (sprintf "out%d" i : string) ]
                ; deps = [ (sprintf "in1" : string) ]
                ; command = (sprintf "cp in1 out%d" i : string)
                }]))
    >>= fun () ->
    let targets = List.map (range 1 n) ~f:(sprintf "out%d") in
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      bench ~jenga ~log name n targets
      >>= fun () ->
      Deferred.List.iter targets ~f:Unix.remove))
)

let () = register_bench "chain-of-n-rules" (fun ~name ~jenga ~log ->
  empty_file "in1"
  >>= fun () ->
  Deferred.List.iter [ 1; 2; 3; 4; 50 ] ~f:(fun n ->
    Writer.save_sexps "rule"
      (List.map (range 1 n) ~f:(fun i ->
         let in_ = if Int.(=) i 1 then "in1" else sprintf "out%d" (i - 1) in
         let out = sprintf "out%d" i in
         [%sexp { targets = [ (out : string) ]
                ; deps = [ (in_ : string) ]
                ; command = (sprintf "cp %s %s" in_ out : string)
                }]))
    >>= fun () ->
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      let targets = List.map (range 1 n) ~f:(sprintf "out%d") in
      bench ~jenga ~log name n
        [ List.last_exn targets ]
      >>= fun () ->
      Deferred.List.iter targets ~f:Unix.remove))
)

let () = register_bench "chain-of-n-scheme-and-rules" (fun ~name ~jenga ~log ->
  Unix.mkdir "din1" >>= fun () ->
  empty_file "din1/f" >>= fun () ->
  Deferred.List.iter [ 1; 2; 3; 4; 50 ] ~f:(fun n ->
    Deferred.List.iter (range 1 n) ~f:(fun i ->
      let in_ = if Int.(=) i 1 then "din1" else sprintf "dout%d" (i - 1) in
      let out = sprintf "dout%d" i in
      Unix.mkdir ~p:() out >>= fun () ->
      Writer.save_sexps (out ^/ "rule")
        [[%sexp { targets = [ "f" ]
                ; deps = [ (sprintf "../%s/f" in_ : string) ]
                ; command = (sprintf "cp ../%s/f f" in_ : string)
                }]])
    >>= fun () ->
    let targets = List.map (range 1 n) ~f:(sprintf "dout%d/f") in
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      bench ~jenga ~log name n [ List.last_exn targets ]
      >>= fun () ->
      Deferred.List.iter targets ~f:Unix.remove))
)

let () = register_bench "complete-dag-of-size-n" (fun ~name ~jenga ~log ->
  Deferred.List.iter [ 1; 2; 3; 9; 20; 50 ] ~f:(fun n ->
    Writer.save_sexps "rule"
      (List.map (range 1 n) ~f:(fun i ->
         let target = sprintf "out%d" i in
         let deps = List.map (range 1 (i - 1)) ~f:(sprintf "out%d") in
         [%sexp { targets = [ (target : string) ]
                ; deps : string list
                ; command = (sprintf "touch %s" target : string) }]))
    >>= fun () ->
    let targets = List.map (range 1 n) ~f:(sprintf "out%d") in
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      bench ~jenga ~log name (n * (n + 1) / 2) [ List.last_exn targets ]
      >>= fun () ->
      Deferred.List.iter targets ~f:Unix.remove))
)

let () = register_bench "n-deps-on-the-same-100-files" (fun ~name ~jenga ~log ->
  let deps = List.map (range 1 100) ~f:(sprintf "in%d") in
  let n_deps = List.length deps in
  Deferred.List.iter deps ~f:empty_file
  >>= fun () ->
  Deferred.List.iter [ 1; 2; 3; 4; 50; 70; 100 ] ~f:(fun n ->
    Writer.save_sexps "rule"
      ([%sexp { targets = [ "out1" ]
              ; deps : string list
              ; command = "touch out1" } ]
       :: List.map (range 2 (1 + n)) ~f:(fun i ->
         [%sexp { targets = [ (sprintf "out%d" i : string) ]
                ; deps : string list = ("out1" :: deps)
                ; command = (sprintf "touch out%d" i : string) } ]))
    >>= fun () ->
    let targets = List.map (range 1 (1 + n)) ~f:(sprintf "out%d") in
    Deferred.List.iter (range 1 5) ~f:(fun _ ->
      bench ~jenga ~log name (n * n_deps) targets
      >>= fun () ->
      Deferred.List.iter targets ~f:Unix.remove))
)

let tmp_dir () =
  Process.run_expect_no_output_exn ~prog:"rm" ~args:[ "-rf"; ".tmp" ] ()
  >>= fun () ->
  Unix.mkdir ~p:() ".tmp" (* ignored by hg and jenga *)
  >>| fun () ->
  ".tmp"
;;

let selected_benches =
  Command.Param.map
    (List.fold (Queue.to_list all_benches) ~init:(Command.Param.return [])
       ~f:(fun acc (name, f) ->
         Command.Param.map2 (Command.Param.flag name ~doc:"" Command.Param.no_arg) acc
           ~f:(fun b acc -> if b then f :: acc else acc)))
    ~f:(function
      | [] -> List.map (Queue.to_list all_benches) ~f:snd
      | _ :: _ as l -> List.rev l)
;;

let command =
  let open Command.Let_syntax in
  let return = Async.return in
  Command.async
    ~summary:"Runs micro-benchmarks on the given jenga executable"
    ~readme:(fun () ->
      "Outputs debugging information in .tmp/logs, and the result of the benches \
       in .tmp/metrics, which can be viewed using the report subcommand.")
    [%map_open
      let jenga = anon ("JENGA-EXE" %: file)
      and selected_benches = selected_benches
      in fun () ->
        Sys.getcwd ()
        >>= fun initial_dir ->
        let jenga =
          if Filename.is_relative jenga
          then initial_dir ^/ jenga
          else jenga
        in
        tmp_dir ()
        >>= fun tmp_dir ->
        Writer.with_file (tmp_dir ^/ "log") ~f:(fun log ->
          Unix.mkdir ~p:() (tmp_dir ^/ "build")
          >>= fun () ->
          Unix.chdir (tmp_dir ^/ "build")
          >>= fun () ->
          copy_and_precompile_jengaroot ~jenga ~initial_dir
          >>= fun () ->
          Deferred.List.iter selected_benches ~f:(fun f -> f ~jenga ~log)
        )
        >>= fun () ->
        Writer.with_file "../metrics" ~f:(fun writer ->
          Queue.iter all_results ~f:(fun (name, n, metrics) ->
            Writer.writef writer "%d%%%s%%%s\n" n name metrics);
          return ()
        )
    ]
