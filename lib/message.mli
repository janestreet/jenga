
open Core.Std
open Async.Std

module Q : sig
  (** [shell_escape s] can be used as a part of bash command line to mean the word [s]
      with any special characters escaped. *)
  val shell_escape : string -> string
  (** [shell_escape_list l] constructs a part of bash command line with multiple
      blank-separated words [l] on it with any special characters escaped *)
  val shell_escape_list : string list -> string
end

module Job_start   : sig type t with bin_io end
module Job_summary : sig type t with bin_io end

val init_logging : Config.t -> log_filename:string -> unit

val error : ('a, unit, string, unit) format4 -> 'a
val message : ('a, unit, string, unit) format4 -> 'a
val verbose : ('a, unit, string, unit) format4 -> 'a
val trace : ('a, unit, string, unit) format4 -> 'a
val unlogged : ('a, unit, string, unit) format4 -> 'a

(* progress style message - will be overwritten by next transient or normal message *)
val transient : ('a, unit, string, unit) format4 -> 'a

val clear_transient : unit -> unit

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
  stdout : string ->
  stderr : string ->
  Job_summary.t (* returned for use in call to repeat_job_finished *)

val repeat_job_summary : Job_summary.t -> unit

val load_jenga_root : Path.t -> modules:string list -> unit
val load_jenga_root_done : Path.t -> Time.Span.t -> unit

module Err : sig
  type t
  val create : ?pos:(int*int) -> ?extra:string -> string -> t
end

val errors_for_omake_server : Path.t -> Err.t list -> unit

val build_done : duration:Time.Span.t -> u:int -> total:int -> string -> unit
val build_failed : duration:Time.Span.t -> u:int -> fraction:(int*int) -> string -> unit

val polling : unit -> unit
val sensitized_on : desc:string -> unit
val file_changed : desc:string -> unit
val rebuilding : unit -> unit

val flushed : unit -> unit Deferred.t
