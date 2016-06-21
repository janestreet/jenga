open Core.Std

type t =
  { version : string
  ; bench : string
  ; metrics : Jenga_lib.Metrics.Disk_format.t
  }

let display_metric v (unit : Jenga_lib.Metrics.Unit.t) =
  match unit with
  | Second -> Time.Span.to_string_hum (Time.Span.of_float v)
  | Byte -> Byte_units.to_string_hum (Byte_units.create `Bytes v)
  | Dimensionless -> Printf.sprintf "%.0f" v
;;

let relative_change ~from:v1 ~to_:v2 =
  Textutils.Text_block.text
    (let f = v2 /. v1 -. 1. in
     if v1 =. v2 || Float.abs f <. 0.001
     then "="
     else Printf.sprintf "%+.1f%%" (f *. 100.))
;;

let same_unit u1 u2 =
  [%test_eq: Jenga_lib.Metrics.Unit.t] u1 u2;
  u1
;;

let display (inputs : t list) =
  let all_metrics =
    List.stable_dedup
      ([ "time"; "top_heap"; "minor"; "major"; "promoted"; "major_collections" ] @
       (List.concat_map inputs ~f:(fun t ->
          Map.keys (Map.remove t.metrics.metrics "heap"))))
  in
  let first_version, all_versions =
    match List.stable_dedup (List.map inputs ~f:(fun t -> t.version)) with
    | [] -> failwith "expected at least one sample in the input"
    | hd :: tl -> hd, tl
  in
  let by_bench =
    let all_keys = List.stable_dedup (List.map inputs ~f:(fun t -> t.bench)) in
    let map_by_bench =
      String.Map.of_alist_multi
        (List.map inputs ~f:(fun t -> t.bench, (t.version, t.metrics.metrics)))
    in
    List.map all_keys ~f:(fun key -> key, Map.find_exn map_by_bench key)
  in
  let module T = Textutils.Text_block in
  let list_of_columns =
    List.map by_bench ~f:(fun (bench, version_and_metrics) ->
      let count_by_version =
        String.Map.of_alist_fold version_and_metrics ~init:0 ~f:(fun acc _ -> acc + 1)
      in
      let metrics_by_version =
        String.Map.of_alist_fold version_and_metrics
          ~init:String.Map.empty
          ~f:(fun acc_metrics metrics ->
            Map.merge acc_metrics metrics ~f:(fun ~key:_ -> function
              | `Left (values, unit) -> Some (values, unit)
              | `Right (value, unit) -> Some ([value], unit)
              | `Both ((xs, unit1), (x, unit2)) ->
                Some (x :: xs, same_unit unit1 unit2)))
        |> Map.map ~f:(Map.map ~f:(fun (values, unit) ->
          let mean = List.sum (module Float) values ~f:Fn.id /. Int.to_float (List.length values) in
          mean, unit))
      in
      let versions_to_show = List.filter all_versions ~f:(Map.mem metrics_by_version) in
      let left_column =
        T.vcat ~align:`Right
          (List.map ~f:T.text (bench :: first_version :: versions_to_show))
      in
      let sample_size =
        T.vcat ~align:`Right
          (T.text "#" ::
           List.map (first_version :: versions_to_show)
             ~f:(fun v -> T.text (Int.to_string (Map.find_exn count_by_version v))))
      in
      let columns_of_data =
        List.map all_metrics ~f:(fun metric_name ->
          let first_metric, unit =
            Map.find_exn (Map.find_exn metrics_by_version first_version) metric_name
          in
          T.vcat ~align:`Right
            (T.text metric_name ::
             T.text (display_metric first_metric unit) ::
             List.map versions_to_show ~f:(fun version ->
               let this_metric, unit' =
                 Map.find_exn (Map.find_exn metrics_by_version version) metric_name
               in
               let _ = same_unit unit unit' in
               relative_change ~from:first_metric ~to_:this_metric)))
      in
      left_column :: sample_size :: columns_of_data)
  in
  let list_of_rows = List.transpose_exn list_of_columns in
  let table_as_string =
    List.map list_of_rows ~f:(T.vcat ~align:`Right)
    |> List.intersperse ~sep:(T.hstrut 1)
    |> T.hcat
    |> T.render
  in
  let table_as_string =
    if Unix.isatty Unix.stdout then
      (* Can't stick ansi escapes on the values directly, as Text_block assumes that all
         characters are the same width. *)
      let module Re2 = Re2.Std.Re2 in
      let worse_metrics = Re2.create_exn {|\+[0-9.]+%|} in
      let better_metrics = Re2.create_exn {|-[0-9.]+%|} in
      table_as_string
      |> Re2.replace_exn worse_metrics ~f:(fun m ->
        sprintf "[31;01m%s[39;22m" (Re2.Match.get_exn m ~sub:(`Index 0)))
      |> Re2.replace_exn better_metrics ~f:(fun m ->
        sprintf "[32;01m%s[39;22m" (Re2.Match.get_exn m ~sub:(`Index 0)))
    else
      table_as_string
  in
  print_string table_as_string
;;

open Command.Let_syntax

let () =
  Command.run
    (Command.basic'
       ~summary:" report bench result by analysing .jenga/metrics files"
       ~readme:(fun () ->
         "Takes input files with lines of the form VERSION%BENCH%METRICS, where\n\
          - VERSION is the version of jenga you're testing (rev of jenga + rev of \
            jengaroot for instance)\n\
          - BENCH is what you're benching (\"core_kernel from scratch\" for instance)\n\
          - METRICS is the line from .jenga/metrics resulting from building BENCH at \
            VERSION. If several lines are given for the same VERSION/BENCH, they are \
            consolidated by averaging them.\n\n\
           The report shows the metrics for all the versions relative to the \
           first version in the file (ie the first version is expected to be the \
           baseline).\n\n\
           There is no accompanying executable to create input files matching this \
           format.")
       [%map_open
         let metrics_files = anon (sequence ("METRICS" %: file))
         in fun () ->
           let lines =
             match metrics_files with
             | [] -> String.split_lines (In_channel.input_all In_channel.stdin)
             | _ :: _ as l ->
               List.concat_map l ~f:(fun file ->
                 String.split_lines (In_channel.read_all file))
           in
           let inputs =
             List.map lines ~f:(fun line ->
               match String.split line ~on:'%' with
               | version :: bench :: rest ->
                 let metrics =
                   Sexp.of_string_conv_exn (String.concat rest ~sep:"%")
                     [%of_sexp: Jenga_lib.Metrics.Disk_format.t]
                 in
                 { version; bench; metrics }
               | _ -> failwithf "parse error on line: %s" line ())
           in
           display inputs
       ]
    )
;;
