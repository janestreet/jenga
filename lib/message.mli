
open Core.Std
open Async.Std

module Job_start : sig type t end

val init_logging : Config.t -> log_filename:string -> unit

val error : ('a, unit, string, unit) format4 -> 'a
val message : ('a, unit, string, unit) format4 -> 'a
val verbose : ('a, unit, string, unit) format4 -> 'a
val reason : ('a, unit, string, unit) format4 -> 'a
val checked : ('a, unit, string, unit) format4 -> 'a
val considering : ('a, unit, string, unit) format4 -> 'a

val trace : ('a, unit, string, unit) format4 -> 'a
val dev : ('a, unit, string, unit) format4 -> 'a

val job_started :
  need:string ->
  where:string ->
  prog:string ->
  args:string list ->
  Job_start.t (* returned for use in call to job_finished *)

val job_finished :
  Job_start.t ->
  outcome : [`success | `error of string] ->
  duration : Time.Span.t ->
  stdout : string list ->
  stderr : string list ->
  unit

val load_jenga_root : Path.LR.t -> unit
val load_jenga_root_done : Path.LR.t -> Time.Span.t -> unit
val load_sexp_error : Path.t -> loc:(int*int) -> exn -> unit

val build_done : duration:Time.Span.t -> u:int -> total:int -> unit
val build_failed : duration:Time.Span.t -> u:int -> fraction:(int*int) -> unit
val progress : fraction:(int * int) -> unit

val polling : unit -> unit
val sensitized_on : Heart.Desc.t -> unit
val file_changed : Heart.Desc.t -> unit
val rebuilding : unit -> unit

val flushed : unit -> unit Deferred.t
