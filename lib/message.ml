open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std
module Log = Async.Std.Log

let pretend_to_be_omake =
  match Core.Std.Sys.getenv "JENGA_PRETEND_TO_BE_OMAKE" with
  | None -> false
  | Some _ -> true

let build_system_message_tag =
  if pretend_to_be_omake then "*** omake" else "*** jenga"

module Q : sig

  val shell_escape : string -> string
  val shell_escape_list : string list -> string

end = struct

  let is_special_char_to_bash = function
    | '\\' | '\'' | '"' | '`' | '<' | '>' | '|' | ';' | ' ' | '\t' | '\n'
    | '(' | ')' | '[' | ']' | '?' | '#' | '$' | '^' | '&' | '*' | '=' | '!' | '~'
      -> true
    | _
      -> false

  let vanilla_shell_escape s =
    "'" ^ String.concat_map s ~f:(function
    | '\'' -> "'\\''"
    | c -> String.make 1 c
    ) ^ "'"

  let needs_quoting = function
    | "" -> true
    | s -> String.exists s ~f:is_special_char_to_bash

  let shell_escape s =
    (* quote a string (if necessary) to prevent interpretation of any chars which have a
       special meaning to bash *)
    if needs_quoting s
    then
      if String.contains s '\''
      (* already contains single-quotes; quote using backslash escaping *)
      then vanilla_shell_escape s
      else
        (* no embedded single quotes; just wrap with single quotes;
           same behavior as [shell_escape], but perhaps more efficient *)
        sprintf "'%s'" s
    else
      (* does not need quoting *)
      s

  let shell_escape_list l =
    String.concat ~sep:" " (List.map l ~f:(fun x -> shell_escape x))

end

let split_string_into_lines s =
  match s with
  | "" -> []
  | "\n" -> [""]
  | _ ->
    let s =
      match String.chop_suffix s ~suffix:"\n" with
      | None -> s
      | Some s -> s
    in
    String.split s ~on:'\n'


let pretty_span span =
  let { Time.Span.Parts.sign = _; hr; min; sec; ms; us = _ } = Time.Span.to_parts span in
  let mins = 60 * hr + min in
  if mins > 0     then sprintf "%dm %02ds" mins sec
  else if sec > 0 then sprintf "%d.%03ds"  sec ms
  else                 sprintf "%dms"      ms
;;

let parse_pretty_span span =
  match String.lsplit2 ~on:' ' span with
  | None -> Time.Span.of_string span
  | Some (minutes, seconds) -> Time.Span.(of_string minutes + of_string seconds)
;;

let%test_unit _ =
  List.iter ~f:(fun str -> [%test_result: string] ~expect:str
                             (pretty_span (parse_pretty_span str)))
    [ "1m 44s"
    ; "23.123s"
    ; "55ms"
    ]
;;

module Job_start = struct
  type t = {
    uid : int;
    need : string;
    where : string;
    prog : string;
    args : string list;
  } [@@deriving bin_io, fields, sexp_of]
end

module Job_finish = struct
  type t = {
    outcome : [`success | `error of string];
    duration : Time.Span.t;
  } [@@deriving bin_io, fields, sexp_of]
end

module Job_output = struct
  type t = {
    stdout : string list;
    stderr : string list;
  } [@@deriving bin_io, fields, sexp_of]
end

module Job_summary = struct

  type t = Job_start.t * Job_finish.t * Job_output.t
  [@@deriving bin_io, sexp_of]

  let output_with ~put (
    {Job_start. where; need; prog; args; uid=_},
    {Job_finish. outcome; duration},
    {Job_output. stdout; stderr}
  ) =
    put (sprintf "- build %s %s" where need);
    (* print out the command in a format suitable for cut&pasting into bash
       (except for the leading "+")
    *)
    let args = List.map args ~f:(fun arg -> Q.shell_escape arg) in
    put (sprintf "+ %s %s" prog (String.concat ~sep:" " args));
    List.iter stdout ~f:put;
    List.iter stderr ~f:put;
    let duration_string = pretty_span duration in
    let status_string =
      match outcome with
      | `success -> "code 0"
      | `error status_string -> status_string
    in
    put (sprintf "- exit %s %s, %s, %s" where need duration_string status_string)

end

module Tag = struct
  (* Error, Message(info), Verbose, Trace, Unlogged *)
  type t = E | M | V | T | U [@@deriving sexp_of]
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Err = struct
  type t = {
    line : int;
    col : int;
    short : string;
    extra : string option;
  } [@@deriving sexp]
  let create ?(pos=(1,1)) ?extra short =
    let line,col = pos in
    {line;col;short;extra}
end

module Event = struct
  type t =
  | Tagged_message of Tag.t * string
  | Transient of string
  | Load_jenga_root of Path.t * string list
  | Load_jenga_root_done of Path.t * Time.Span.t
  | Errors_for_omake_server of Path.t * Err.t list
  | Job_started of Job_start.t
  | Job_completed of Job_summary.t
  | Job_summary of Job_summary.t
  | Build_done of Time.Span.t * [`u of int] * int * string
  | Build_failed of Time.Span.t * [`u of int] * (int*int) * string
  | Polling
  | Sensitized_on of string
  | File_changed of string
  | Rebuilding

  [@@deriving sexp_of]
end

module T = struct

  module Logger = struct
    type t = {
      f : Event.t -> unit;
      flushed : unit -> unit Deferred.t;
    }
    let dispatch t event = t.f event
    let flushed t = t.flushed ()
  end

  type t = {
    mutable loggers : Logger.t list;
  }

  let install_logger t ~f ~flushed =
    t.loggers <-  t.loggers @ [ { Logger.f = f; flushed } ]

  let dispatch t event =
    List.iter t.loggers ~f:(fun x -> Logger.dispatch x event)

  let flushed t () =
    Deferred.all_unit (List.map t.loggers ~f:Logger.flushed )

end

let the_log = { T. loggers = [] }

let tagged_message tag fmt =
  ksprintf (fun string ->
    let event = Event.Tagged_message (tag,string) in
    T.dispatch the_log event
  ) fmt

let error fmt   = tagged_message Tag.E fmt
let message fmt = tagged_message Tag.M fmt
let verbose fmt = tagged_message Tag.V fmt
let trace fmt   = tagged_message Tag.T fmt
let unlogged fmt= tagged_message Tag.U fmt


let last_transient_message = ref None

let transient fmt =
  ksprintf (fun string ->
    last_transient_message := Some string;
    let event = Event.Transient string in
    T.dispatch the_log event
  ) fmt

let clear_transient () = last_transient_message := None


let load_jenga_root path ~modules =
  T.dispatch the_log (Event.Load_jenga_root (path,modules))

let load_jenga_root_done path span =
  T.dispatch the_log (Event.Load_jenga_root_done (path,span))

let errors_for_omake_server path errs =
  T.dispatch the_log (Event.Errors_for_omake_server (path,errs))

let job_started =
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun ~need ~where ~prog ~args ->
  let uid = genU() in
  let started = { Job_start. uid; need; where; prog; args } in
  let event = Event.Job_started started in
  T.dispatch the_log event;
  started

let job_finished start ~outcome ~duration ~stdout ~stderr =
  let finish = { Job_finish. outcome; duration } in
  let stdout = split_string_into_lines stdout in
  let stderr = split_string_into_lines stderr in
  let output = { Job_output. stdout; stderr } in
  let summary = (start,finish,output) in
  let event = Event.Job_completed summary  in
  T.dispatch the_log event;
  summary

let build_done ~duration ~u ~total s =
  T.dispatch the_log (Event.Build_done (duration, `u u, total, s))

let build_failed ~duration ~u ~fraction s =
  T.dispatch the_log (Event.Build_failed (duration, `u u,fraction,s))

let polling () =
  clear_transient();
  T.dispatch the_log (Event.Polling)

let sensitized_on ~desc =
  T.dispatch the_log (Event.Sensitized_on desc)

let file_changed ~desc =
  T.dispatch the_log (Event.File_changed desc)

let repeat_job_summary summary =
  let event = Event.Job_summary summary  in
  T.dispatch the_log event

let base_time = ref (Time.now())

let rebuilding () =
  (* reset base_time spans since build-started reported by -time *)
  base_time := Time.now();
  T.dispatch the_log (Event.Rebuilding)

let fixed_span_for_message_prefix span =
  (* fixed width (upto 100min) format for span for -time flag -- mm:ss.xxx *)
  let parts = Time.Span.to_parts span in
  let module P = Time.Span.Parts in
  sprintf "%02d:%02d.%03d" (60 * parts.P.hr + parts.P.min) parts.P.sec parts.P.ms

let stat_since_last_checked get_current zero (-) =
  let last = ref zero in
  fun stat ->
    let current = get_current stat in
    let diff = current - !last in
    last := current;
    diff
;;

let stat_since_last_checked_int get_current =
  stat_since_last_checked get_current 0 (-)
let stat_since_last_checked_float get_current =
  stat_since_last_checked get_current 0. (-.)

let pretty_mem_usage =
  let float_words x = Byte_units.create `Words x in
  let int_words x = float_words (Float.of_int x) in
  let major_collections = stat_since_last_checked_int Gc.Stat.major_collections in
  let minor_words = stat_since_last_checked_float Gc.Stat.minor_words in
  let major_words = stat_since_last_checked_float Gc.Stat.major_words in
  let promoted_words = stat_since_last_checked_float Gc.Stat.promoted_words in
  fun (config : Config.t) ->
    let stat = Gc.quick_stat () in
    let heap = int_words stat.heap_words in
    let gc_details =
      if config.show_memory_allocations
      then sprintf !", m=%{Byte_units}, M=%{Byte_units}, p=%{Byte_units}, major=%d"
             (float_words (minor_words stat))
             (float_words (major_words stat))
             (float_words (promoted_words stat))
             (major_collections stat)
      else ""
    in
    let top_heap =
      let top_heap = int_words stat.top_heap_words in
      if Byte_units.(=) heap top_heap
      then ""
      else sprintf !", top heap=%{Byte_units}" top_heap
    in
    sprintf !"heap=%{Byte_units}%s%s" heap top_heap gc_details
;;

let omake_style_logger config event =

  let verbose = Config.verbose config in
  let show_trace_messages = Config.show_trace_messages config in

  let elapsed =
    if not (Config.prefix_time config) then "" else
      let duration = Time.diff (Time.now()) (!base_time) in
      sprintf "%s " (fixed_span_for_message_prefix duration)
  in

  let dont_emit_kill_line () =
    Config.dont_emit_kill_line config
  in

  let put_trans s =
    if dont_emit_kill_line()
    then Core.Std.Printf.printf "%s%s\r%!" elapsed s
    else Core.Std.Printf.printf "\027[K%s%s\r%!" elapsed s
  in

  let redisplay_transient() =
    match (!last_transient_message) with
    | None -> ()
    | Some s -> put_trans s
  in

  let put s =
    if dont_emit_kill_line()
    then Core.Std.Printf.printf "%s%s\n%!" elapsed s
    else Core.Std.Printf.printf "\027[K%s%s\n%!" elapsed s
  in
  let jput s =
    (if dont_emit_kill_line()
    then Core.Std.Printf.printf "%s%s: %s\n%!" elapsed build_system_message_tag s
    else Core.Std.Printf.printf "\027[K%s%s: %s\n%!" elapsed build_system_message_tag s);
    redisplay_transient()
  in

  match event with
  (* jput -- with leading triple stars *)
  | Event.Tagged_message (Tag.E,s) -> jput (sprintf "ERROR: %s" s)
  | Event.Tagged_message (Tag.M,s) -> jput s
  | Event.Tagged_message (Tag.V,s) -> if verbose then jput s
  | Event.Tagged_message (Tag.T,s) -> if show_trace_messages then jput s
  (*put*)
  | Event.Tagged_message (Tag.U,s) -> put s

  (* progress style message - wll be overwritten by next transient or normal message *)
  | Event.Transient s -> put_trans s

  | Event.Load_jenga_root (path,modules) ->
    if verbose then (
      let where = Path.to_string (Path.dirname path) in
      let need = Path.basename path in
      put (sprintf "- build %s %s" where need);
    );
    let modules_string =
      match modules with
      | [] -> ""
      | _ -> sprintf " (%s)" (String.concat ~sep:" " modules)
    in
    jput (sprintf "reading %s%s" (Path.to_string path) modules_string)

  | Event.Load_jenga_root_done (path,duration) ->
    jput (sprintf "finished reading %s (%s)"
      (Path.to_string path)
      (pretty_span duration))

  | Event.Errors_for_omake_server (path,errs) ->
    let where = Path.to_string (Path.dirname path) in
    let file = Path.basename path in
    if verbose then (
      put (sprintf "- build %s %s" where file);
    );
    List.iter errs ~f:(fun err ->
      let {Err. line;col;short;extra} = err in
      put (sprintf "File \"%s\", line %d, characters %d-%d:" file line col col);
      put (sprintf "Error: %s" short);
      (match extra with | None -> () | Some text -> put text);
    );
    if verbose then (
      put (sprintf "- exit %s %s" where file);
    );
    redisplay_transient()

  | Event.Job_started _ -> () (* used to print the "- build" line here *)

  | Event.Job_completed summary ->
    let (
      {Job_start. where=_; need=_; prog=_; args=_; uid=_},
      {Job_finish. outcome; duration=_},
      {Job_output. stdout=_; stderr=_}
    ) = summary in
    let job_failed =
      match outcome with | `success -> false | `error _ -> true
    in
    let show_something = job_failed || verbose in
    if show_something then (
      Job_summary.output_with summary ~put
    )

  | Event.Job_summary summary ->
    Job_summary.output_with summary
      ~put:(fun line -> put (sprintf "      %s" line)) (* six spaces matches omake *)

  | Event.Build_done (duration,`u u,total,s) ->
    jput (sprintf "%d/%d targets are up to date" total total);
    jput (sprintf "done (#%d, %s, %s, %s) -- HURRAH"
            u (pretty_span duration) (pretty_mem_usage config) s)

  | Event.Build_failed (duration, `u u,(num,den),s) -> (
    jput (sprintf "%d/%d targets are up to date" num den);
    jput (sprintf "failed (#%d, %s, %s, %s)"
            u (pretty_span duration) (pretty_mem_usage config) s);
  )

  | Event.Polling ->
    jput "polling for filesystem changes"

  | Event.File_changed desc ->
    jput (sprintf "%s changed" desc)

  | Event.Sensitized_on desc ->
    jput (sprintf "- sensitized to: %s" desc)

  | Event.Rebuilding ->
    jput "rebuilding--------------------------------------------------"
;;

let parse_build_measures_assoc_list =
  let parse =
    let open Re2.Std.Parser in
    Staged.unstage (compile (
      or_ [string "done"; string "failed"]
      *> string " ("
      *> capture (
        string "#"
        *> ignore Decimal.int
        *> string ", "
        *> repeat (ignore Char.any)
      )
      <* string ")"
      <* ignore (optional (string " -- HURRAH"))))
  in
  (fun str ->
     Option.map (parse str)
       ~f:(fun csv ->
         List.map (String.split ~on:',' csv) ~f:(fun value ->
           let value = String.strip value in
           match String.lsplit2 ~on:'=' value with
           | Some (key, value) -> (key, value)
           | None -> ("", value)
         ))
  )
;;

let%test_unit _ =
  let test str expect =
    [%test_result: (string * string) list option] (parse_build_measures_assoc_list str)
      ~expect
  in
  test "\
*** jenga: done (#1, 30.199s, heap=1.17188g, m=8.25294g, M=1.54731g, p=1.43634g, \
major=11, stat=95340, digest=16, ls=4570, db-save=0) -- HURRAH
"
    (Some [ ""        , "#1"
          ; ""        , "30.199s"
          ; "heap"    , "1.17188g"
          ; "m"       , "8.25294g"
          ; "M"       , "1.54731g"
          ; "p"       , "1.43634g"
          ; "major"   , "11"
          ; "stat"    , "95340"
          ; "digest"  , "16"
          ; "ls"      , "4570"
          ; "db-save" , "0"
          ]);
  test "blah" None
;;

let install_logger ~f ~flushed = T.install_logger the_log ~f ~flushed

let string_of_event event =
  match event with
  | Event.Tagged_message (tag,s) -> sprintf "%s: %s" (Tag.to_string tag) s
  | _  -> Sexp.to_string (Event.sexp_of_t event)

let to_log_full_logger log event =
  match event with
  | Event.Tagged_message (Tag.U,_) -> ()
  | _ ->
    Log.raw log "%s" (string_of_event event)

let make_log ~log_filename =
  let output = [Log.Output.file `Text ~filename:log_filename] in
  let log = Log.create ~level:`Debug ~output ~on_error:`Raise in
  log


let init_logging config ~log_filename =
  let log = make_log ~log_filename in
  install_logger ~f:(to_log_full_logger log) ~flushed:(fun () -> Log.flushed log);
  install_logger ~f:(omake_style_logger config) ~flushed:(fun () -> Deferred.unit)


let flushed = T.flushed the_log
