(** The module that handles the jenga database, loading it on starting and saving it
    periodically, assuming something needs to be saved.
    Manipulation of the database is done in other places. *)

open! Core
open! Async

type t

val create_saving_periodically : Time.Span.t -> t Or_error.t Deferred.t

(** tries to save; on error prints out the message and becomes determined *)
val disable_periodic_saving_and_save_now : t -> unit Deferred.t
val re_enable_periodic_saving : t -> unit

module Quality : sig
  type t = [`initial | `good] [@@deriving sexp_of]
  val to_string : t -> string
end

val quality : t -> Quality.t

val modify : string -> 'a -> 'a

val db : t -> Db.t

(** The interface below is meant for inspecting the database, not for use by the core of
    jenga. *)
val load_db : unit -> Db.t Or_error.t Deferred.t
val get_db_filename : unit -> string
