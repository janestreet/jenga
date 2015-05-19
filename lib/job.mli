
open Core.Std
open Async.Std

exception Shutdown

(* [Job.t] is a process description
   We can run it directly. Or we can extract a suitable quoted string to run it via a
   shell, such as "bash -c". *)

type t = Db.Job.t
with sexp_of, bin_io, compare
val create : dir:Path.t -> prog:string -> args:string list -> t

val bracket : t -> sh_prelude:string -> sh_postlude:string -> t

(** returns a bash script that expects to be run from [dir t] *)
val to_sh_ignoring_dir : t -> string

val dir : t -> Path.t

module Output : sig

  (* Policy for treating the command output *)
  type 'a t

  (* no stdout expected; command being run for effect *)
  val ignore : unit t

  (* stdout expected & wanted *)
  val stdout : string t

  (* given an ouput policy of a specific type, cause no output *)
  val none : 'a t -> 'a

end

val run :
  t ->
  config:Config.t ->
  need:string ->
  putenv : (string * string) list ->
  output : 'a Output.t ->
  ('a,
   [
   | `non_zero_status of Message.Job_summary.t
   | `other_error of exn
   ]
  ) Result.t Deferred.t
