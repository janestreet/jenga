open Core
open Int.Replace_polymorphic_compare
module T = Textutils.Text_block

type t =
  { version : string
  ; bench : string
  ; metrics : Jenga_lib.Metrics.Disk_format.t
  }

let display_span ~show_sign v =
  let parts = Time.Span.to_parts v in
  let part_and_units =
    List.drop_while ~f:(fun (n, _) -> n = 0)
      [ parts.hr, "h"
      ; parts.min, "min"
      ; parts.sec, "s"
      ; parts.ms, "ms"
      ]
  in
  let body =
    match List.take part_and_units 2 with
    | [ (sec, unit); (ms, "ms") ] ->
      let denominator = if sec < 10 then 10 else 100 in
      sprintf "%d.%d%s" sec (ms / denominator) unit
    | [ (v1, unit1); (v2, _) ] ->
      sprintf "%d%s%02d" v1 unit1 v2
    | [ (v1, unit1) ] ->
      sprintf "%d%s" v1 unit1
    | [] -> "0"
    | _ -> assert false
  in
  let sign =
    match parts.sign with
    | Neg -> "-"
    | Pos -> if show_sign then "+" else ""
    | Zero -> ""
  in
  sign ^ body

let chop_trailing_zero s =
  Option.value ~default:s (String.chop_suffix s ~suffix:"0")

let display_float ~show_sign scaled_and_units =
  let f, unit =
    List.find scaled_and_units ~f:(fun (f, _) -> Float.(>=) (Float.abs f) 1.)
    |> Option.value ~default:(List.last_exn scaled_and_units)
  in
  let s =
    if show_sign
    then sprintf "%+.2f" f
    else sprintf "%.2f" f
  in
  let s =
    match String.lsplit2 s ~on:'.' with
    | None -> s
    | Some (left, right) ->
      let right = chop_trailing_zero (chop_trailing_zero right) in
      let concat l r = if String.is_empty r then l else l ^ "." ^ r in
      let whole_part_length =
        String.length left - (match left.[0] with '+' | '-' -> 1 | _ -> 0)
      in
      concat left
        (if whole_part_length >= 3
         then ""
         else if whole_part_length = 2
         then String.prefix right 1
         else right)
  in
  s ^ unit
;;

let display_byte_unit ~show_sign v =
  display_float ~show_sign
    [ Byte_units.gigabytes v, "GB"
    ; Byte_units.megabytes v, "MB"
    ; Byte_units.kilobytes v, "kB"
    ; Byte_units.bytes v, "B" ]

let display_dimensionless_number ~show_sign v =
  display_float ~show_sign
    [ v *. 1e-9, "G"
    ; v *. 1e-6, "M"
    ; v *. 1e-3, "k"
    ; v, ""
    ]

let display_metric ~show_sign v (unit : Jenga_lib.Metrics.Unit.t) =
  match unit with
  | Second -> display_span ~show_sign (Time.Span.of_sec v)
  | Byte -> display_byte_unit ~show_sign (Byte_units.create `Bytes v)
  | Dimensionless -> display_dimensionless_number ~show_sign v
;;

let use_colors = lazy (Unix.isatty Unix.stdout)

let rough_compare v1 v2 =
  let f = v2 /. v1 -. 1. in
  if v1 =. v2 || Float.abs f <. 0.001
  then `Eq
  else if Float.(>) f 0.
  then `Not_eq (`Lt, f)
  else `Not_eq (`Gt, f)
;;

let ansi_escape ~cmp s =
  let prefix, suffix =
    if not (force use_colors)
    then "", ""
    else
      match cmp with
      | `Lt -> "[01;31m", "[22;39m"
      | `Gt -> "[01;32m", "[22;39m"
  in
  T.ansi_escape ~prefix ~suffix s
;;

let absolute_change ?(prefix = "") ~unit ~from:v1 ~to_:v2 =
  match rough_compare v1 v2 with
  | `Eq -> T.text (prefix ^ "=")
  | `Not_eq (cmp, _) ->
    ansi_escape ~cmp
      (T.text (prefix ^ display_metric ~show_sign:true (v2 -. v1) unit))
;;

let relative_change ~unit ~from:v1 ~to_:v2 =
  if Float.(=) v1 0. && Float.(<>) v2 0.
  then absolute_change ~unit ~prefix:"(abs) " ~from:v1 ~to_:v2
  else
    match rough_compare v1 v2 with
    | `Eq -> T.text "="
    | `Not_eq (cmp, f) ->
      ansi_escape ~cmp
        (if f >. 3. || f <. -0.75
         then
           match cmp with
           | `Gt -> T.text (sprintf "/%.1f" (v1 /. v2))
           | `Lt -> T.text (sprintf "x%.1f" (v2 /. v1))
         else T.text (sprintf "%+.1f%%" (f *. 100.)))
;;

let same_unit u1 u2 =
  [%test_eq: Jenga_lib.Metrics.Unit.t] u1 u2;
  u1
;;

let mean a ~f =
  List.sum (module Float) a ~f /. Int.to_float (List.length a)

let display_metric_name = function
  | "major_collections" -> "gc cycle"
  | s -> s

let display (inputs : t list) ~show_dispersion ~display_non_baseline =
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
             ~f:(fun v ->
               let count = Option.value (Map.find count_by_version v) ~default:0 in
               T.text (Int.to_string count)))
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
          let find_version version =
            match Map.find metrics_by_version version with
            | None -> None
            | Some by_metric -> Map.find by_metric metric_name
          in
          let cells_of_all_versions =
            List.map (first_version :: versions_to_show) ~f:(fun version ->
              match find_version version with
              | None -> T.text "?"
              | Some (this_metric, this_dispersion, unit') ->
                let baseline =
                  if String.(=) version first_version
                  then None
                  else find_version first_version
                in
                let display_metric =
                  match baseline with
                  | None -> T.text (display_metric ~show_sign:false this_metric unit')
                  | Some (first_metric, _, unit) ->
                    let unit = same_unit unit unit' in
                    display_non_baseline ~unit ~from:first_metric ~to_:this_metric
                in
                and_dispersion display_metric this_dispersion)
          in
          T.vcat ~align:`Right
            (T.text (display_metric_name metric_name) :: cells_of_all_versions))
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

let command =
  Command.basic
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
        Such input files can be created by the [run] subcommand.\
    ")
    [%map_open
      let metrics_files = anon (sequence ("METRICS" %: file))
      and show_dispersion =
        flag "-dispersion" no_arg ~doc: " show the average relative distance between the values and their mean"
      and show_absolute_change =
        flag "-absolute-change" no_arg ~doc:" compare to the baseline using absolute values"
      in fun () ->
        let lines =
          match metrics_files with
          | [] ->
            List.mapi (String.split_lines (In_channel.input_all In_channel.stdin))
              ~f:(fun line_number line -> "-", 1 + line_number, line)
          | _ :: _ as l ->
            List.concat_map l ~f:(fun file ->
              List.mapi (String.split_lines (In_channel.read_all file))
                ~f:(fun line_number line -> file, 1 + line_number, line))
        in
        let inputs =
          List.filter_map lines ~f:(fun (file, line_number, line) ->
            let parse_metrics str =
              match
                Sexp.of_string_conv_exn str
                  [%of_sexp: Jenga_lib.Metrics.Disk_format.t]
              with
              | exception e ->
                printf !"Ignoring line %d in %s due to parse error:\n%{Sexp#hum}\n"
                  line_number file [%sexp ~~(line : string), (e : exn)];
                None
              | metrics -> Some metrics
            in
            match String.split line ~on:'%' with
            | version :: bench :: rest ->
              Option.map (parse_metrics (String.concat rest ~sep:"%"))
                ~f:(fun metrics -> { version; bench; metrics })
            | _ ->
              Option.map (parse_metrics line)
                ~f:(fun metrics -> { version = file; bench = "?"; metrics })
          ) |> List.map ~f:(fun { version; bench; metrics } ->
            let more_version =
              let map = [%of_sexp: Sexp.t String.Map.t] metrics.build_info in
              if [%of_sexp: bool] (Map.find_exn map "x_library_inlining")
              then ""
              else " (no x_lib)"
            in
            { version = version ^ more_version; bench; metrics })
        in
        display ~show_dispersion inputs
          ~display_non_baseline:
            (if show_absolute_change
             then absolute_change ?prefix:None
             else relative_change)
    ]
;;
