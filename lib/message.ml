
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
    stdout : string list;
    stderr : string list;
  } with fields, sexp_of
end

module Tag = struct
  (* Error, Message(info), Verbose, Reason, Trace, Unlogged *)
  type t = E | M | V | R | T | U with sexp_of
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Event = struct
  type t =
  | Tagged_message of Tag.t * string
  | Load_jenga_root of Path.LR.t
  | Load_jenga_root_done of Path.LR.t * Time.Span.t
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
let reason fmt  = tagged_message Tag.R fmt
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

let rebuilding () =
  T.dispatch the_log (Event.Rebuilding)


let need_quoting x = String.contains x ' '
let quote_arg x = if need_quoting x then sprintf "'%s'" x else x
let concat_args_quoting_spaces xs = String.concat ~sep:" " (List.map xs ~f:quote_arg)


let omake_style_logger config event =

  let quiet = Config.quiet config in   (* quiet trumps verbose *)
  let verbose = Config.verbose config in

  let put fmt =
    ksprintf (fun s -> Printf.printf "%s\n%!" s) fmt
  in
  let jput fmt =
    ksprintf (fun s -> Printf.printf "%s: %s\n%!" build_system_message_tag s) fmt
  in
  match event with
  (* jput -- with leading triple stars *)
  | Event.Tagged_message (Tag.E,s) -> jput "ERROR: %s" s
  | Event.Tagged_message (Tag.M,s) -> jput "%s" s
  | Event.Tagged_message (Tag.V,s) -> if verbose then jput "%s" s
  | Event.Tagged_message (Tag.R,s) -> if Config.show_run_reason config then jput "%s" s
  (*put*)
  | Event.Tagged_message (Tag.T,s) -> if Config.debug config then put "TRACE: %s" s
  | Event.Tagged_message (Tag.U,s) -> put "%s" s

  | Event.Load_jenga_root path ->
    if not quiet then (
      if verbose then (
        let where = Path.LR.to_rrr_string (Path.LR.dirname path) in
        let need = Path.LR.basename path in
        put "- build %s %s" where need;
      );
      jput "reading %s" (Path.LR.to_rrr_string path)
    )
  | Event.Load_jenga_root_done (path,duration) ->
    jput "finished reading %s (%s)"
      (Path.LR.to_rrr_string path)
      (Time.Span.to_string duration)

  | Event.Load_sexp_error (path,`loc (line,col),exn) ->
    if not quiet then (
      if verbose then (
        let where = Path.to_rrr_string (Path.dirname path) in
        let need = Path.basename path in
        put "- build %s %s" where need;
      );
    (* write location like ocaml compiler.. *)
      let file = Path.basename path in
      put "File \"%s\", line %d, characters %d-%d:" file line col col;
      put "Error: sexp_conversion failed\n%s\n" (Exn.to_string exn)
    )
  | Event.Job_started {Job_start.where; need; stdout_expected=_; prog=_; args=_; uid=_} ->
    if not quiet then (
      (* if verbose, show the "- build" line when the job starts *)
      if verbose then (
        put "- build %s %s" where need;
      )
    )
  | Event.Job_completed (
    {Job_start. where; need; stdout_expected; prog; args; uid=_},
    {Job_finish. outcome; duration},
    {Job_output. stdout; stderr}
  ) ->
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
        let duration_string = Time.Span.to_string duration in
        if not verbose then (
        (* if verbose, we already showed this line when the job started *)
          put "- build %s %s" where need;
        );
        put "+ %s %s" prog (concat_args_quoting_spaces args);
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
    jput "done (#%d, %s, %s) -- HURRAH" u (Time.Span.to_string duration) s

  | Event.Build_failed (duration, `u u,(num,den),s) -> (
    jput "%d/%d targets are up to date" num den;
    jput "failed (#%d, %s, %s)" u (Time.Span.to_string duration) s;
  )

  | Event.Progress (num,den) -> (
    put "[= ] %d / %d" num den;
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
