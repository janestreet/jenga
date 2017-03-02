(** This module implements the two forms of logging in jenga:
    - logging to .jenga/debug
    - printing on stdout
*)

open! Core
open! Async

val init_logging : Config.t -> log_filename:string -> unit

val error : ('a, unit, string, unit) format4 -> 'a
val message : ('a, unit, string, unit) format4 -> 'a
val verbose : ('a, unit, string, unit) format4 -> 'a
val trace : ('a, unit, string, unit) format4 -> 'a

(** [unlogged] - no leading triple stars; not recorded in log *)
val unlogged : ('a, unit, string, unit) format4 -> 'a

(** [printf] - no leading triple stars *)
val printf : ('a, unit, string, unit) format4 -> 'a

(** [printf_verbose] - no leading triple stars; tagged verbose *)
val printf_verbose : ('a, unit, string, unit) format4 -> 'a

(** Progress style message - will be overwritten by next transient or normal message *)
val transient : ('a, unit, string, unit) format4 -> 'a

val clear_transient : unit -> unit

val job_started :
  need:string ->
  where:string ->
  prog:string ->
  args:string list ->
  sandboxed:bool ->
  Job_summary.Start.t (* returned for use in call to job_finished *)

val job_finished :
  Job_summary.Start.t ->
  outcome : [`success | `error of string] ->
  duration : Time.Span.t ->
  stdout : string ->
  stderr : string ->
  Job_summary.t (* returned for use in call to repeat_job_finished *)

val repeat_job_summary : Job_summary.t -> unit

val load_jenga_root : Path.t -> unit
val load_jenga_root_done : Path.t -> Time.Span.t -> unit

val build_done
  : duration:Time.Span.t
  -> u:int
  -> total:int
  -> string
  -> Metrics.Memory.t
  -> unit
val build_failed
  : duration:Time.Span.t
  -> u:int
  -> fraction:(int*int)
  -> string
  -> Metrics.Memory.t
  -> unit

val polling : unit -> unit
val sensitized_on : desc:string -> unit
val file_changed : desc:string -> unit
val rebuilding : unit -> unit
val var_changed : var:string -> old:string option -> new_:string option -> unit

val flushed : unit -> unit Deferred.t
