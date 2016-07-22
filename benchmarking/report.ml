open Core.Std
module T = Textutils.Text_block

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

let use_colors = lazy (Unix.isatty Unix.stdout)

let relative_change ~from:v1 ~to_:v2 =
  let f = v2 /. v1 -. 1. in
  if v1 =. v2 || Float.abs f <. 0.001
  then T.text "="
  else
    let s = T.text (Printf.sprintf "%+.1f%%" (f *. 100.)) in
    let prefix, suffix =
      if not (force use_colors)
      then "", ""
      else
        if f >. 0.
        then "[01;31m", "[22;39m"
        else "[01;32m", "[22;39m"
    in
    T.ansi_escape ~prefix ~suffix s
;;

let same_unit u1 u2 =
  [%test_eq: Jenga_lib.Metrics.Unit.t] u1 u2;
  u1
;;

let mean a ~f =
  List.sum (module Float) a ~f /. Int.to_float (List.length a)

let display (inputs : t list) ~show_dispersion =
  let all_metrics =
    List.stable_dedup
      ([ "time"; "top_heap"; "heap"; "minor"; "major"; "promoted"; "major_collections" ] @
       (List.concat_map inputs ~f:(fun t -> Map.keys t.metrics.metrics)))
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
          let mean_v = mean values ~f:Fn.id in
          let dispersion =
            mean values ~f:(fun v ->
              if v =. 0. && mean_v =. 0.
              then 0.
              else Float.abs (v /. mean_v -. 1.))
          in
          mean_v, dispersion, unit))
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
      let and_dispersion block dispersion =
        let prefix, suffix =
          if not (force use_colors)
          then "", ""
          else if dispersion >. 1.
          then "[45m", "[49m"
          else if dispersion >. 0.3
          then "[41m", "[49m"
          else if dispersion >. 0.05
          then "[43m", "[49m"
          else "", ""
        in
        T.hcat
          [ T.ansi_escape ~prefix ~suffix block
          ; if show_dispersion
            then T.text (Printf.sprintf " %.0f%%" (dispersion *. 100.))
            else T.nil
          ]
      in
      let columns_of_data =
        List.map all_metrics ~f:(fun metric_name ->
          let first_metric, first_dispersion, unit =
            Map.find_exn (Map.find_exn metrics_by_version first_version) metric_name
          in
          T.vcat ~align:`Right
            (T.text metric_name ::
             and_dispersion
               (T.text (display_metric first_metric unit))
               first_dispersion ::
             List.map versions_to_show ~f:(fun version ->
               let this_metric, this_dispersion, unit' =
                 Map.find_exn (Map.find_exn metrics_by_version version) metric_name
               in
               ignore (same_unit unit unit');
               and_dispersion
                 (relative_change ~from:first_metric ~to_:this_metric)
                 this_dispersion
             )))
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
           Background color represents variance:\n\
           - no background color: little variance\n\
           - yellow: slightly high variance\n\
           - red: really high variance\n\
           - purple: so much variance the data is nonsensical\n\n\
           There is no accompanying executable to create input files matching this \
           format.\
       ")
       [%map_open
         let metrics_files = anon (sequence ("METRICS" %: file))
         and show_dispersion =
           flag "-dispersion" no_arg ~doc: " show the average relative distance between the values and their mean"
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
           display ~show_dispersion inputs
       ]
    )
;;
