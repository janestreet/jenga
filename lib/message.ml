open Core
open! Int.Replace_polymorphic_compare
open Async
module Log = Async.Log

module Tag = struct
  type t =
  (* with triple leading stars *)
  | Error
  | Message
  | Verbose (* -verbose *)
  | Trace   (* -trace *)
  (* without triple leading stars *)
  | Unlogged
  | Printf
  | Printf_verbose
  [@@deriving sexp_of, bin_io]

  let to_string = function
    | Error -> "E"
    | Message -> "M"
    | Verbose -> "V"
    | Trace -> "T"
    | Unlogged -> "U"
    | Printf -> "P"
    | Printf_verbose -> "PV"
end

module Event = struct
  type t =
  | Tagged_message of Tag.t * string
  | Transient of string
  | Load_jenga_root of Path.t
  | Load_jenga_root_done of Path.t * Time.Span.t
  | Job_started of Job_summary.Start.t
  | Job_completed of Job_summary.t
  | Job_summary of Job_summary.t
  | Build_done of Time.Span.t * [`u of int] * int * string * Metrics.Memory.t
  | Build_failed of Time.Span.t * [`u of int] * (int*int) * string * Metrics.Memory.t
  | Polling
  | Sensitized_on of string
  | File_changed of string
  | Var_changed of string * string option * string option
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

let error fmt    = tagged_message Error fmt
let message fmt  = tagged_message Message fmt
let verbose fmt  = tagged_message Verbose fmt
let trace fmt    = tagged_message Trace fmt
let unlogged fmt = tagged_message Unlogged fmt
let printf fmt   = tagged_message Printf fmt
let printf_verbose fmt = tagged_message Printf_verbose fmt

let last_transient_message = ref None

let transient fmt =
  ksprintf (fun string ->
    last_transient_message := Some string;
    let event = Event.Transient string in
    T.dispatch the_log event
  ) fmt

let clear_transient () = last_transient_message := None


let load_jenga_root path =
  T.dispatch the_log (Event.Load_jenga_root path)

let load_jenga_root_done path span =
  T.dispatch the_log (Event.Load_jenga_root_done (path,span))

let job_started ~need ~where ~prog ~args ~sandboxed =
  let started = Job_summary.Start.create ~need ~where ~prog ~args ~sandboxed in
  let event = Event.Job_started started in
  T.dispatch the_log event;
  started

let job_finished start ~outcome ~duration ~stdout ~stderr =
  let summary = Job_summary.create start ~outcome ~duration ~stdout ~stderr in
  let event = Event.Job_completed summary  in
  T.dispatch the_log event;
  summary

let build_done ~duration ~u ~total s memory_metrics =
  T.dispatch the_log (Event.Build_done (duration, `u u, total, s, memory_metrics))

let build_failed ~duration ~u ~fraction s memory_metrics =
  T.dispatch the_log (Event.Build_failed (duration, `u u, fraction, s, memory_metrics))

let polling () =
  clear_transient();
  T.dispatch the_log (Event.Polling)

let sensitized_on ~desc =
  T.dispatch the_log (Event.Sensitized_on desc)

let file_changed ~desc =
  T.dispatch the_log (Event.File_changed desc)

let var_changed ~var ~old ~new_ =
  T.dispatch the_log (Event.Var_changed (var, old, new_))

let repeat_job_summary summary =
  let event = Event.Job_summary summary  in
  T.dispatch the_log event

let rebuilding () =
  T.dispatch the_log (Event.Rebuilding)

let pretty_mem_usage (config : Config.t) (m : Metrics.Memory.t) =
  let gc_details =
    if config.show_memory_allocations
    then sprintf !", m=%{Byte_units}, M=%{Byte_units}, p=%{Byte_units}, major=%d"
           m.minor m.major m.promoted m.major_collections
    else ""
  in
  let top_heap =
    if Byte_units.(=) m.heap m.top_heap
    then ""
    else sprintf !", top heap=%{Byte_units}" m.top_heap
  in
  sprintf !"heap=%{Byte_units}%s%s" m.heap top_heap gc_details
;;

let stdout_logger config event =

  let verbose = Config.verbose config in
  let show_trace_messages = Config.show_trace_messages config in

  let elapsed =
    if not (Config.prefix_time config) then "" else
      sprintf "[%s] " (Time.to_string (Time.now ()))
  in

  let dont_emit_kill_line () =
    Config.dont_emit_kill_line config
  in

  let put_trans s =
    if dont_emit_kill_line()
    then Core.Printf.printf "%s%s\r%!" elapsed s
    else Core.Printf.printf "\027[K%s%s\r%!" elapsed s
  in

  let redisplay_transient() =
    match (!last_transient_message) with
    | None -> ()
    | Some s -> put_trans s
  in

  let put s =
    if dont_emit_kill_line()
    then Core.Printf.printf "%s%s\n%!" elapsed s
    else Core.Printf.printf "\027[K%s%s\n%!" elapsed s
  in
  let jput s =
    (if dont_emit_kill_line()
    then Core.Printf.printf "%s*** jenga: %s\n%!" elapsed s
    else Core.Printf.printf "\027[K%s*** jenga: %s\n%!" elapsed s);
    redisplay_transient()
  in

  match event with
  (* jput -- with leading triple stars *)
  | Event.Tagged_message (Error, s) -> jput (sprintf "ERROR: %s" s)
  | Event.Tagged_message (Message, s) -> jput s
  | Event.Tagged_message (Verbose, s) -> if verbose then jput s
  | Event.Tagged_message (Trace, s) -> if show_trace_messages then jput s
  (*put*)
  | Event.Tagged_message ((Unlogged | Printf), s) -> put s
  | Event.Tagged_message (Printf_verbose, s) -> if verbose then put s

  (* progress style message - wll be overwritten by next transient or normal message *)
  | Event.Transient s -> put_trans s

  | Event.Load_jenga_root path ->
    if verbose then (
      let where = Path.to_string (Path.dirname path) in
      let need = Path.basename path in
      put (sprintf "- build %s %s" where need);
    );
    jput (sprintf !"reading %{Path}" path)

  | Event.Load_jenga_root_done (path,duration) ->
    jput (sprintf "finished reading %s (%s)"
      (Path.to_string path)
      (Job_summary.pretty_span duration))

  | Event.Job_started _ -> () (* used to print the "- build" line here *)

  | Event.Job_completed summary ->
    let outcome = Job_summary.outcome summary in
    let job_failed =
      match outcome with | `success -> false | `error _ -> true
    in
    let show_something = job_failed || verbose in
    if show_something then (
      Job_summary.iter_lines summary ~f:put
    )

  | Event.Job_summary summary ->
    Job_summary.iter_lines summary
      ~f:(fun line -> put (sprintf "      %s" line))

  | Event.Build_done (duration, `u u, total, s, memory_metrics) ->
    jput (sprintf "%d/%d targets are up to date" total total);
    jput (sprintf "done (#%d, %s, %s, %s) -- HURRAH"
            u (Job_summary.pretty_span duration)
            (pretty_mem_usage config memory_metrics) s)

  | Event.Build_failed (duration, `u u, (num,den), s, memory_metrics) -> (
    jput (sprintf "%d/%d targets are up to date" num den);
    jput (sprintf "failed (#%d, %s, %s, %s)"
            u (Job_summary.pretty_span duration)
            (pretty_mem_usage config memory_metrics) s);
  )

  | Event.Polling ->
    jput "polling for filesystem changes"

  | Event.File_changed desc ->
    jput (sprintf "%s changed" desc)

  | Event.Var_changed (var, old_, new_) ->
    let old_ = Option.value old_ ~default:"<none>" in
    let new_ = Option.value new_ ~default:"<none>" in
    jput (sprintf "variable %s changed: %s -> %s" var old_ new_)

  | Event.Sensitized_on desc ->
    jput (sprintf "- sensitized to: %s" desc)

  | Event.Rebuilding ->
    jput "rebuilding--------------------------------------------------"
;;

let install_logger ~f ~flushed = T.install_logger the_log ~f ~flushed

let string_of_event event =
  match event with
  | Event.Tagged_message (tag,s) -> sprintf "%s: %s" (Tag.to_string tag) s
  | _  -> Sexp.to_string (Event.sexp_of_t event)

let to_log_full_logger log event =
  match event with
  | Event.Tagged_message (Unlogged, _) -> ()
  | _ ->
    Log.raw log "%s" (string_of_event event)

let make_log ~log_filename =
  let output = [Log.Output.file `Text ~filename:log_filename] in
  let log = Log.create ~level:`Debug ~output ~on_error:`Raise in
  log


let init_logging config ~log_filename =
  let log = make_log ~log_filename in
  install_logger ~f:(to_log_full_logger log) ~flushed:(fun () -> Log.flushed log);
  install_logger ~f:(stdout_logger config) ~flushed:(fun () -> Deferred.unit)


let flushed = T.flushed the_log
