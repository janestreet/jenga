(* The trivial rewrite of Core_extended.find - The code duplication makes me sad *)

open Core.Std
open Async.Std
type t

val xlstat_counter : Effort.Counter.t

module Options : sig
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (string -> unit Deferred.t)

  type t = {
      max_depth: int option;
      follow_links: bool;
      on_open_errors: error_handler;
      on_stat_errors: error_handler;
      filter: ((string * Unix.Stats.t) -> bool Deferred.t) option;
      skip_dir: ((string * Unix.Stats.t) -> bool Deferred.t) option;
    }

  val default : t
  val ignore_errors : t
end

(** [create ?options dir] create a Find.t based in dir *)
val create : ?options:Options.t -> string -> t Deferred.t

(** [next t] return the next file from the collection of valid files in t or None
  if no more files remain *)
val next : t -> (string * Unix.Stats.t) option Deferred.t

(** [close t] drops all the resources associated with t.  Attempting to use t again will
  raise an exception.  Any Find.t will be automatically closed after the last file is read
  by any means. *)
val close : t -> unit Deferred.t

(** [iter t ~f] calls f on every file in t *)
val iter : t -> f:((string * Unix.Stats.t) -> unit Deferred.t) -> unit Deferred.t

(** [fold t ~init ~f] folds f over the files in t *)
val fold : t -> init:'a -> f:('a -> (string * Unix.Stats.t) -> 'a Deferred.t) -> 'a Deferred.t

(** [to_list t] returns all of the remaining files in t as a list in the order they
  would have been returned by subsequent calls to next *)
val to_list : t -> (string * Unix.Stats.t) list Deferred.t

(** [find_all ?options dir] short for to_list (create ?options dir) *)
val find_all : ?options:Options.t -> string -> (string * Unix.Stats.t) list Deferred.t
