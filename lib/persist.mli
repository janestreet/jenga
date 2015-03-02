
open Core.Std
open Async.Std

val saves_done : Effort.Counter.t

type t

val create_saving_periodically : root_dir:string -> Time.Span.t -> t Deferred.t

(** tries to save; on error prints out the message and becomes determined *)
val disable_periodic_saving_and_save_now : t -> unit Deferred.t
val re_enable_periodic_saving : t -> unit

module Quality : sig
  type t = [`initial | `format_changed | `good] with sexp_of
  val to_string : t -> string
end

val quality : t -> Quality.t

val modify : string -> 'a -> 'a

val db : t -> Db.t

val load_db_as_sexp : db_filename:string -> Sexp.t Deferred.t
