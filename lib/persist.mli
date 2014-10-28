
open Core.Std
open Async.Std

type t

val create_saving_periodically : root_dir:string -> Time.Span.t -> t Deferred.t

val disable_periodic_saving_and_save_now : t -> unit Deferred.t
val re_enable_periodic_saving : t -> unit

val fs_persist : t -> Fs.Persist.t
val build_persist : t -> Build.Persist.t
val quality : t -> Build.Persistence_quality.t

(* internal state of the persist module, used for offline inspection *)
module State : sig
  type t
  val load_db : db_filename:string -> t Deferred.t
  val sexp_of_t : t -> Sexp.t
  val build_persist : t -> Build.Persist.t
end
