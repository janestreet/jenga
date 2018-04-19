open Core
open Async
open! Int.Replace_polymorphic_compare

module Job_summary = struct
  module Start = struct
    type t = {
      uid : int;
      need : string;
      where : string;
      prog : string;
      args : string list;
      sandboxed : bool;
    } [@@deriving sexp]
  end

  module Finish = struct
    type t = {
      outcome : [`success | `error of string];
      duration : Time.Span.t;
    } [@@deriving sexp]
  end

  module Output = struct
    type t = {
      stdout : string list;
      stderr : string list;
    } [@@deriving sexp]
  end

  type t = Start.t * Finish.t * Output.t
  [@@deriving sexp]
end

type event =
  | Job_started of Job_summary.Start.t
  | Job_completed of Job_summary.t
[@@deriving sexp]

let iter_over_events debug_file ~f =
  Reader.with_file debug_file ~f:(fun reader ->
    Pipe.iter (Reader.lines reader) ~f:(fun line ->
      match String.lsplit2 line ~on:' ' with
      | Some (date, rest) ->
        begin match String.lsplit2 rest ~on:' ' with
        | Some (time_of_day, sexp) ->
          if String.is_prefix ~prefix:"(Job_started" sexp
          || String.is_prefix ~prefix:"(Job_completed" sexp
          then
            let time = Time.of_string (date ^ " " ^ time_of_day) in
            f time (Sexp.of_string_conv_exn sexp event_of_sexp)
          else return ()
        | None -> return ()
        end
      | None -> return ()
    )
  )
;;


let map_with_next l ~f =
  match l with
  | [] -> []
  | h :: t ->
    let rec loop acc prev t =
      match t with
      | [] -> List.rev (f prev None :: acc)
      | h :: t -> loop (f prev (Some h) :: acc) h t
    in
    loop [] h t
;;

(* Given a step function, this returns a new step function which has about the same values
   but only one step every span [step]. The value of the steps of the output function is
   the average of the value of the input function for the range of x values considered. *)
let reduce_resolution ~step ~scalar_mul ~add time_a_list =
  let combine this_start next_start this_acc =
    let this_acc_combined =
      map_with_next (List.rev this_acc) ~f:(fun (v_time, v) next ->
        let v_time = Time.max v_time this_start in
        let next_time = match next with None -> next_start | Some (t, _) -> t in
        [%test_pred: Time.t * Time.t] (fun (a, b) -> Time.(>=) a b) (next_time, this_start);
        Time.diff next_time v_time, v)
      |> List.map ~f:(fun (span, v) -> scalar_mul (Time.Span.(//) span step) v)
      |> List.reduce_exn ~f:add
    in
    let time_combined = Time.add this_start (Time.Span.scale step 0.5) in
    time_combined, this_acc_combined
  in
  let rec loop this_start this_acc acc time_a_list =
    assert (not (List.is_empty this_acc));
    let next_start = Time.add this_start step in
    match time_a_list with
    | [] -> List.rev (combine this_start next_start this_acc :: acc)
    | (time, a) :: rest ->
      if Time.(>) time next_start
      then loop next_start [List.hd_exn this_acc]
             (combine this_start next_start this_acc :: acc) time_a_list
      else loop this_start ((time, a) :: this_acc) acc rest
  in
  match time_a_list with
  | [] -> []
  | (time, a) :: rest -> loop time [(time, a)] [] rest
;;

let parse_dtimings_output_4_06 (output : Job_summary.Output.t) =
  let parsed_lines =
    List.filter_map output.stdout ~f:(fun line ->
      let stripped_line = String.lstrip line in
      let indentation = String.length line - String.length stripped_line in
      match String.lsplit2 stripped_line ~on:' ' with
      | None -> None
      | Some (time, category) ->
        Some (indentation, Time.Span.of_string time, category))
  in
  let all =
    List.sum (module Time.Span) parsed_lines
      ~f:(fun (indentation, span, _) ->
        if indentation = 0 then span else Time.Span.zero)
  in
  let major_categories_and_durations =
    List.filter_map parsed_lines ~f:(fun (indentation, span, category) ->
      if indentation <> 2 || String.(=) category "other" then None
      else
        let category =
          match category with
          | "transl" | "generate" -> "backend"
          | s -> s
        in
        Some ("ocaml " ^ category, span))
  in
  Some (major_categories_and_durations, all)
;;

let parse_dtimings_output_4_03 start (output : Job_summary.Output.t) =
  let categories_and_durations =
    List.filter_map output.stdout ~f:(fun line ->
      Option.map (String.lsplit2 line ~on:':') ~f:(fun (timing_category, duration) ->
        let timing_category =
          match String.lsplit2 timing_category ~on:'(' with
          | None -> timing_category
          | Some (r, _) -> r
        in
        String.strip timing_category, Time.Span.of_string duration))
  in
  match categories_and_durations with
  | [ ("parsing", _); ("all", _); ("preprocessing", _) ]
  | [ ("all", _); ("preprocessing", _) ] ->
    None (* no timings, which includes at least compilation of mli's *)
  | _ ->
    let all = ref None in
    let major_categories_and_durations =
      List.filter_map categories_and_durations ~f:(fun (timing_category, duration) ->
        match timing_category with
         (* included in generate I think *)
        | "assemble" | "clambda" | "cmm" | "compile_phrases" | "comballoc"
        | "cse" | "liveness" | "deadcode" | "spill" | "regalloc" | "linearize"
        | "scheduling" | "emit" | "flambda_pass" | "split" | "selection"
        (* included in parsing *)
        | "-pp" | "parser" | "-ppx"
          -> None
        | "all" -> assert (Option.is_none !all); all := Some duration; None
        | "transl" | "generate" -> Some ("ocaml backend", duration)
        | "preprocessing" -> None (* included in parsing with my change, and useless
                                     without my change, as it uses Sys.time to measure
                                     time spent in a subprocess *)
        | "parsing"  | "typing" as s -> Some ("ocaml " ^ s, duration)
        | s -> failwithf "unknown -dtiming category: %s" s ())
    in
    match !all with
    | None -> raise_s [%sexp (start : Job_summary.Start.t)]
    | Some all -> Some (major_categories_and_durations, all)
;;

let categorize_ocaml_passes ((start, finish, output) : Job_summary.t) =
  if List.mem start.args "-dtimings" ~equal:String.equal
  && (* timings for linking is bad before 4.05, and even after we'd need to classify
        linker vs everything else, because the details inside the compiler are probably
        not relevant *)
  not (String.is_substring start.prog ~substring:"link-quietly")
  && (match finish.outcome with `success -> true | `error _ -> false)
  then
    if String.is_substring start.prog ~substring:"/4.03"
    || String.is_substring start.prog ~substring:"/4.04"
    || String.is_substring start.prog ~substring:"/4.05"
    then parse_dtimings_output_4_03 start output
    else parse_dtimings_output_4_06 output
  else None

let source =
  let is_strict_suffix x ~suffix =
    String.is_suffix x ~suffix && String.length x > String.length suffix
  in
  fun args ->
    List.find args ~f:(fun x ->
      is_strict_suffix x ~suffix:".ml"
      || is_strict_suffix x ~suffix:".ml-gen"
      || is_strict_suffix x ~suffix:".mli")

let simple_category ((start, _, _) : Job_summary.t) =
  if String.is_substring start.prog ~substring:"ocamldep"
  then "ocamldep", source start.args
  else if String.is_substring start.prog ~substring:"ocamlopt"
       && List.mem start.args "-c"    ~equal:String.equal
       && List.mem start.args "-impl" ~equal:String.equal
  then ".ml", source start.args
  else if String.is_substring start.prog ~substring:"ocamlc"
       && List.mem start.args "-c"    ~equal:String.equal
       && List.mem start.args "-impl" ~equal:String.equal
  then ".ml byte", source start.args
  else if (String.is_substring start.prog ~substring:"ocamlc"
           || String.is_substring start.prog ~substring:"ocamlopt")
       && List.mem start.args "-c"    ~equal:String.equal
       && List.mem start.args "-intf" ~equal:String.equal
  then ".mli", source start.args
  else if (String.is_substring start.prog ~substring:"ocamlc"
           || String.is_substring start.prog ~substring:"ocamlopt")
       && match List.rev (List.take (List.rev start.args) 2) with
         [ "-o"; name ] ->
           (match String.rsplit2 name ~on:'.' with
           | None | Some (_, "exe") -> true
           | _ -> false)
       | _ -> false
  then "link", None
  else if String.is_substring start.need ~substring:"fgrep"
  then "test/bench grep", None
  else if List.exists (start.prog :: start.args)
            ~f:(String.is_substring ~substring:"ocamlobjinfo")
  then "ocamlobjinfo", None
  else if String.(=) start.prog "gcc"
       || String.(=) start.prog "g++"
       || (String.is_substring start.prog ~substring:"ocamlc"
           && Option.exists (List.hd start.args) ~f:(String.is_suffix ~suffix:".c"))
  then "c/c++", None
  else if String.is_prefix ~prefix:".liblinks" start.where
  then "liblinks", None
  else if List.exists start.args ~f:(String.(=) "./inline_tests_runner")
       || String.is_substring start.prog ~substring:"enforce-style"
       || String.(=) start.need ".runtest"
  then "test", None
  else if String.(=) start.prog "hg"
       || List.exists start.args ~f:(String.is_substring ~substring:"hg showconfig")
  then "hg", None
  else if String.is_substring start.prog ~substring:"ocamlyacc"
       || String.is_substring start.prog ~substring:"ocamllex"
       || String.is_substring start.prog ~substring:"menhir"
  then "codegen", None
  else if (String.is_substring start.prog ~substring:"ocamlopt"
           && Option.exists (List.last start.args) ~f:(String.is_suffix ~suffix:".cmxa"))
  then "archive", None
  else if List.exists start.args ~f:(String.is_substring ~substring:"ocaml_embed_compiler")
  then "ocaml-plugin", None
  else if String.(=) start.prog "bash"
  then "bash", None
  else "other", None (* raise_s [%sexp (start : Job_summary.Start.t)] *)

let category job_summary =
  match categorize_ocaml_passes job_summary with
  | Some v -> `Detailed v
  | None -> `Simple (fst (simple_category job_summary))
;;

let categories_and_durations ((_, finish, _) as job_summary : Job_summary.t) =
  let duration = finish.duration in
  match category job_summary with
  | `Simple category -> [ category, duration ]
  | `Detailed (categories_and_durations, all) ->
    let sum_of_durations = List.sum (module Time.Span) categories_and_durations ~f:snd in
    let measured_but_undetailed = Time.Span.(-) all sum_of_durations in
    let categories_and_durations = categories_and_durations @ [ "measured by compiler without detail", measured_but_undetailed ] in
    let unaccounted_for = Time.Span.(-) duration all in
    (* if Time.Span.(>.) unaccounted_for (Time.Span.of_sec 0.5)
     * then eprintf !"%{Sexp#hum}\n"
     *        [%sexp ~~(all : Time.Span.t), ~~(duration : Time.Span.t), ~~(unaccounted_for : Time.Span.t),
     *               ~~(measured_but_undetailed : Time.Span.t), ~~(start.prog : string), ~~(start.args : string list)]; *)
    if Time.Span.(<.) measured_but_undetailed Time.Span.zero
    then eprintf !"Sum of subcategories is more than total (%{Time.Span})\n"
           measured_but_undetailed;
    if Time.Span.(<.) unaccounted_for Time.Span.zero
    then (eprintf !"real time is less than user/sys time (%{Time.Span} overaccounted for)\n"
            (Time.Span.neg unaccounted_for);
          categories_and_durations)
    else if Time.Span.(>.) unaccounted_for Time.Span.zero
    then categories_and_durations @ [ "padding between ocaml and jenga measurement", unaccounted_for ]
    else categories_and_durations
;;

let report_totals ~writer:w all_categories =
  fprintf w "category,total time\n";
  List.iter all_categories ~f:(fun (span, category) ->
    fprintf w !"%s,%f\n" category (Time.Span.to_min span))
;;

module M = Core_extended.Std.Interval_map.Make(Time)

let report_schedule ~writer:w ~all_categories ~interval_map ~first_time =
  fprintf w "time";
  List.iter all_categories ~f:(fprintf w ",%s");
  fprintf w "\n";
  Sequence.to_list (M.construct_preimage interval_map)
  |> List.filter_map ~f:(fun (v, interval) ->
    match interval with
    | `Always -> assert false
    | `Until _ -> None
    | `From k | `Between (k, _) -> Some (k, v))
  |> reduce_resolution
       ~step:(Time.Span.of_sec 5.)
       ~scalar_mul:(fun x y -> Map.map y ~f:(fun y -> x *. Float.of_int y))
       ~add:(fun m1 m2 -> Map.merge m1 m2 ~f:(fun ~key:_ ->
         function `Left x
                | `Right x -> Some x
                | `Both (x, y) -> Some (x +. y)))
  |> List.iter ~f:(fun (k, m) ->
    (* We could output Time.Span.to_string, but htmlplot shows spans as number of seconds
       apparently, which is not nice. *)
    fprintf w "%f" (Time.Span.to_min (Time.diff k first_time));
    List.iter all_categories ~f:(fun category ->
      let v = Option.value (Map.find m category) ~default:0. in
      fprintf w ",%f" v);
    fprintf w "\n")
;;

let to_csv debug_file ~line_columns =
  let count_lines file =
    In_channel.with_file file ~f:(fun ch ->
      In_channel.fold_lines ch ~init:0 ~f:(fun acc _ -> acc + 1))
  in
  printf "end_time,duration,category,dir,source";
  if line_columns then printf ",line_count,line_per_second";
  printf "\n";
  iter_over_events debug_file ~f:(fun time event ->
    match event with
    | Job_started _ -> return ()
    | Job_completed (start, finish, _ as job_summary) ->
      let category, source = simple_category job_summary in
      let dir = start.where in
      printf !"%{Time},%{Time.Span},%s,%s,%s"
        time finish.duration category dir (Option.value source ~default:"");
      if line_columns then begin
        match source with
        | None -> printf ",,"
        | Some source ->
          let line_count = count_lines (dir ^/ source) in
          printf ",%d,%.0f"
            line_count (Float.of_int line_count /. Time.Span.to_sec finish.duration);
      end;
      printf "\n";
      return ())
;;

let main debug_file ~totals ~graph ~csv ~csv_line_counts =
  let unfinished = Int.Table.create () in
  let state = ref (M.create ~left_of_leftmost:String.Map.empty ~value_right_of:Time.Map.empty) in
  let first_time = ref Time.epoch in
  let all_categories = ref String.Map.empty in
  iter_over_events debug_file ~f:(fun time event ->
    if Time.(=) Time.epoch !first_time then first_time := time;
    begin match event with
    | Job_started start ->
      Hashtbl.add_exn unfinished ~key:start.uid ~data:time
    | Job_completed (start, finish, _ as job_summary) ->
      let start_time = Option.value_exn (Hashtbl.find_and_remove unfinished start.uid) in
      ignore start_time;
      let categories_and_durations = categories_and_durations job_summary in
      let start = ref (Time.sub time finish.duration) in
      List.iter categories_and_durations ~f:(fun (category, duration) ->
        all_categories :=
          Map.update !all_categories category
            ~f:(function None -> duration
                       | Some prev -> Time.Span.(+) prev duration);
        let end_ = Time.add !start duration in
        state := M.map_within !state
                   (`Between (!start, end_))
                   ~f:(fun x -> Map.update x category ~f:(function None -> 1 | Some v -> v + 1));
        start := end_;
      );
    end;
    return ();
  ) >>= fun () ->
  let all_categories =
    Map.to_alist !all_categories
    |> List.map ~f:(fun (a, b) -> (b, a))
    |> List.sort ~compare:[%compare: Time.Span.t * string]
  in
  let report_totals writer = report_totals all_categories ~writer in
  let report_schedule writer =
    report_schedule
      ~all_categories:(List.map all_categories ~f:snd)
      ~interval_map:!state
      ~first_time:!first_time
      ~writer
  in
  if csv
  then to_csv debug_file ~line_columns:csv_line_counts
  else
    match graph with
    | None ->
      (if totals
       then report_totals (force Writer.stdout)
       else report_schedule (force Writer.stdout));
      return ()
    | Some output ->
      Unix.mkdir ~p:() output
      >>= fun () ->
      let totals_csv = output ^/ "totals.csv" in
      let totals_sexp = output ^/ "totals.sexp" in
      Writer.with_file totals_csv ~f:(fun w -> report_totals w; return ())
      >>= fun () ->
      let schedule_csv = output ^/ "schedule.csv" in
      let schedule_sexp = output ^/ "schedule.sexp" in
      Writer.with_file schedule_csv ~f:(fun w -> report_schedule w; return ())
      >>= fun () ->
      Process.run_exn
        ~prog:"htmlplot"
        ~args:[ "csv"; "hi"; totals_csv; "-sexp"; "-output"; totals_sexp
              ; "-kind"; "pie"; "-colors"; "spinner" ]
        ()
      >>| print_string
      >>= fun () ->
      Process.run_exn
        ~prog:"htmlplot"
        ~args:[ "csv"; "hi"; schedule_csv; "-sexp"; "-output"; schedule_sexp
              ; "-kind"; "area"; "-colors"; "spinner"; "-height"; "600"; "-width"; "1000" ]
        ()
      >>| print_string
      >>= fun () ->
      (* htmlplot doesn't support stacked area charts, so we fiddle with javascript
         directly *)
      Process.run_expect_no_output_exn
        ~prog:"sed"
        ~args:["-ie"; "s/'plotOptions':{/'plotOptions':{'area':{'stacking':'normal'},/"
              ; schedule_sexp ]
        ()
      >>= fun () ->
      Process.run_exn
        ~prog:"htmlplot"
        ~args:[ "csv"; "of-sexps"; schedule_sexp; totals_sexp; "-output"; output ^/ "graph.html" ]
        ()
      >>| print_string
;;

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:" report bench result by analysing .jenga/debug files"
    [%map_open
      let debug_file = anon (".jenga/debug" %: file)
      and totals = flag "totals" no_arg  ~doc:""
      and graph = flag "graph" (optional file) ~doc:"DIR where the graph will be written"
      and csv = flag "csv" no_arg ~doc:" output a simplified version of the input as a csv"
      and csv_line_counts = flag "csv-line-counts" no_arg ~doc:" add information to -csv output"
      in fun () ->
        Writer.behave_nicely_in_pipeline ();
        main debug_file ~totals ~graph ~csv ~csv_line_counts
    ]
;;
