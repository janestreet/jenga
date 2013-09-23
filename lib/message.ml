
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std
module Log = Async.Std.Log

let pretend_to_be_omake =
  match Core.Std.Sys.getenv "JENGA_PRETEND_TO_BE_OMAKE" with
  | None -> false
  | Some _ -> true

let build_system_message_tag =
  if pretend_to_be_omake then "*** omake" else "*** jenga"

module Job_start = struct
  type t = {
    uid : int;
    need : string;
    stdout_expected : bool;
    where : string;
    prog : string;
    args : string list;
  } with fields, sexp_of
end

module Job_finish = struct
  type t = {
    outcome : [`success | `error of string];
    duration : Time.Span.t;
  } with fields, sexp_of
end

module Job_output = struct
  type t = {
    stdout : string;
    stderr : string;
  } with fields, sexp_of
end

module Tag = struct
  (* Error, Message(info), Verbose, Trace, Unlogged *)
  type t = E | M | V | T | U with sexp_of
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Event = struct
  type t =
  | Tagged_message of Tag.t * string
  | Load_jenga_root of Path.X.t
  | Load_jenga_root_done of Path.X.t * Time.Span.t
  | Load_sexp_error of Path.t * [`loc of int * int] * exn
  | Job_started of Job_start.t
  | Job_completed of Job_start.t * Job_finish.t * Job_output.t
  | Build_done of Time.Span.t * [`u of int] * int * string
  | Build_failed of Time.Span.t * [`u of int] * (int*int) * string
  | Progress of (int*int)
  | Polling
  | Sensitized_on of string
  | File_changed of string
  | Rebuilding

  with sexp_of
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

let load_jenga_root path =
  T.dispatch the_log (Event.Load_jenga_root path)

let load_jenga_root_done path span =
  T.dispatch the_log (Event.Load_jenga_root_done (path,span))

let load_sexp_error path ~loc exn =
  T.dispatch the_log (Event.Load_sexp_error (path,`loc loc, exn))

let job_started =
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun ~need ~stdout_expected ~where ~prog ~args ->
  let uid = genU() in
  let started = { Job_start. uid; need; stdout_expected; where; prog; args } in
  let event = Event.Job_started started in
  T.dispatch the_log event;
  started

let job_finished start ~outcome ~duration ~stdout ~stderr =
  let finish = { Job_finish. outcome; duration } in
  let output = { Job_output. stdout; stderr } in
  let event = Event.Job_completed (start,finish,output) in
  T.dispatch the_log event

let build_done ~duration ~u ~total s =
  T.dispatch the_log (Event.Build_done (duration, `u u, total, s))

let build_failed ~duration ~u ~fraction s =
  T.dispatch the_log (Event.Build_failed (duration, `u u,fraction,s))

let progress ~fraction =
  T.dispatch the_log (Event.Progress fraction)

let polling () =
  T.dispatch the_log (Event.Polling)

let sensitized_on ~desc =
  T.dispatch the_log (Event.Sensitized_on desc)

let file_changed ~desc =
  T.dispatch the_log (Event.File_changed desc)


let base_time = ref (Time.now())

let rebuilding () =
  (* reset base_time spans since build-started reported by -time *)
  base_time := Time.now();
  T.dispatch the_log (Event.Rebuilding)


module Q = struct

  let is_special_char_to_bash = function
    | '\\' | '\'' | '"' | '`' | '<' | '>' | '|' | ';' | ' ' | '\t' | '\n'
    | '(' | ')' | '[' | ']' | '?' | '#' | '$' | '^' | '&' | '*' | '='
      -> true
    | _
      -> false

  (*let escape_special_chars s =
    String.concat_map s ~f:(fun c ->
      if is_special_char_to_bash c then sprintf "\\%c" c else Char.to_string c
    )*)

  let shell_escape s =
    "'" ^ String.concat_map s ~f:(function
    | '\'' -> "'\\''"
    | c -> String.make 1 c
    ) ^ "'"

  let quote_arg_prevent_bash_interpretation s =
    (* quote a string (if necessary) to prevent interpretation of any chars which have a
       special meaning to bash *)
    if String.exists s ~f:is_special_char_to_bash
    then
      if String.contains s '\''
      (* already contains single-quotes; quote using backslash escaping *)
      then shell_escape s
      (* no embedded single quotes; just wrap with single quotes *)
      else sprintf "'%s'" s
    else
      (* does not need quoting *)
      s

end

let output_lines s =
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


let fixed_span_for_message_prefix span =
  (* fixed width (upto 100min) format for span for -time flag -- mm:ss.xxx *)
  let parts = Time.Span.to_parts span in
  let module P = Time.Span.Parts in
  sprintf "%02d:%02d.%03d" (60 * parts.P.hr + parts.P.min) parts.P.sec parts.P.ms


let pretty_span span =
  let parts = Time.Span.to_parts span in
  let module P = Time.Span.Parts in
  let mins = 60 * parts.P.hr + parts.P.min in
  if Int.(mins > 0) then              sprintf "%dm %02ds" mins parts.P.sec
  else if Int.(parts.P.sec > 0) then  sprintf     "%d.%03ds"   parts.P.sec parts.P.ms
  else                                sprintf        "%dms"                parts.P.ms

let pretty_mem_usage =
  let words_per_kb = 1024 / 8 in
  fun () ->
    let stat = Gc.stat () in
    let live = stat.Gc.Stat.live_words / words_per_kb in
    let heap = stat.Gc.Stat.heap_words / words_per_kb in
    sprintf "heap(Kb)= %d/%d" live heap


let omake_style_logger config event =

  let quiet = Config.quiet config in   (* quiet trumps verbose *)
  let verbose = Config.verbose config in

  let elapsed =
    if not (Config.time config) then "" else
      let duration = Time.diff (Time.now()) (!base_time) in
      sprintf "%s " (fixed_span_for_message_prefix duration)
  in

  let put fmt =
    ksprintf (fun s -> Printf.printf "%s%s\n%!" elapsed s) fmt
  in
  let jput fmt =
    ksprintf (fun s -> Printf.printf "%s%s: %s\n%!" elapsed build_system_message_tag s) fmt
  in
  match event with
  (* jput -- with leading triple stars *)
  | Event.Tagged_message (Tag.E,s) -> jput "ERROR: %s" s
  | Event.Tagged_message (Tag.M,s) -> jput "%s" s
  | Event.Tagged_message (Tag.V,s) -> if verbose then jput "%s" s
  (*put*)
  | Event.Tagged_message (Tag.T,s) -> if Config.debug config then put "TRACE: %s" s
  | Event.Tagged_message (Tag.U,s) -> put "%s" s

  | Event.Load_jenga_root path ->
    if not quiet then (
      if verbose then (
        let where = Path.X.to_string (Path.X.dirname path) in
        let need = Path.X.basename path in
        put "- build %s %s" where need;
      );
      jput "reading %s" (Path.X.to_string path)
    )
  | Event.Load_jenga_root_done (path,duration) ->
    jput "finished reading %s (%s)"
      (Path.X.to_string path)
      (pretty_span duration)

  | Event.Load_sexp_error (path,`loc (line,col),exn) ->
    if not quiet then (
      let where = Path.to_string (Path.dirname path) in
      let file = Path.basename path in
      (* hack on .cmx suffix for benefit of omake-server *)
      let need = file ^ ".cmx" in
      if verbose then (
        put "- build %s %s" where need;
      );
      put "File \"%s\", line %d, characters %d-%d:" file line col col;
      put "Error: sexp_conversion failed\n%s" (Exn.to_string exn);
      if verbose then (
        put "- exit %s %s" where need;
      );
    )
  | Event.Job_started _ -> () (* used to print the "- build" line here *)
  | Event.Job_completed (
    {Job_start. where; need; stdout_expected; prog; args; uid=_},
    {Job_finish. outcome; duration},
    {Job_output. stdout; stderr}
  ) ->
    let stdout = output_lines stdout in
    let stderr = output_lines stderr in
    if not quiet then (
      let job_failed =
        match outcome with | `success -> false | `error _ -> true
      in
      let has_stderr_or_unexpected_stdout =
        (match stderr with [] -> false | _ -> true)
        || (not stdout_expected && (match stdout with [] -> false | _ -> true))
      in
      let show_something = job_failed  || has_stderr_or_unexpected_stdout || verbose in
      if show_something then (
        let duration_string = pretty_span duration in
        put "- build %s %s" where need;
        (* print out the command in a format suitable for cut&pasting into bash
           (except for the leading "+")
        *)
        put "+ %s %s" prog (
          String.concat ~sep:" "
            (List.map args ~f:Q.quote_arg_prevent_bash_interpretation)
        );
        let status_string =
          match outcome with
          | `success -> "code 0"
          | `error status_string -> status_string
        in
        List.iter stdout ~f:(fun line -> put "%s" line);
        List.iter stderr ~f:(fun line -> put "%s" line);
        put "- exit %s %s, %s, %s" where need duration_string status_string;
      )
    )
  | Event.Build_done (duration,`u u,total,s) ->
    jput "%d/%d targets are up to date" total total;
    jput "done (#%d, %s, %s, %s) -- HURRAH" u (pretty_span duration) (pretty_mem_usage()) s

  | Event.Build_failed (duration, `u u,(num,den),s) -> (
    jput "%d/%d targets are up to date" num den;
    jput "failed (#%d, %s, %s, %s)" u (pretty_span duration) (pretty_mem_usage()) s;
  )

  | Event.Progress (num,den) -> (
    put "\r[= ] %d / %d" num den;
  )

  | Event.Polling ->
    jput "polling for filesystem changes"

  | Event.File_changed desc ->
    jput "%s changed" desc

  | Event.Sensitized_on desc ->
    jput "- sensitized to: %s" desc

  | Event.Rebuilding ->
    jput "rebuilding--------------------------------------------------"


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
  let log = Log.create ~level:`Debug ~output in
  log


let init_logging config ~log_filename =
  let log = make_log ~log_filename in
  install_logger ~f:(to_log_full_logger log) ~flushed:(fun () -> Log.flushed log);
  install_logger ~f:(omake_style_logger config) ~flushed:(fun () -> Deferred.unit)


let flushed = T.flushed the_log
