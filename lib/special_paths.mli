(** The names of various files that jenga itself knows about.
    Jenga created all its files below .jenga, but knows about a few
    other files, like the "jengaroot.ml". *)

open! Core
open! Async

(** Files created in the [.jenga] sub-directory. *)
module Dot_jenga : sig

  (** Prepares the directory for special files (mkdir -p .jenga).
      Not async because we do it very early (but after [discover_root]). *)
  val prepare : unit -> unit

  val debug : Path.Rel.t
  val metrics : Path.Rel.t
  val server : Path.Rel.t
  val plugin_cache : Path.Rel.t
  val db : version:string -> Path.Rel.t
  val local_lock : Path.Rel.t

  val matches : Path.t -> bool

end

(** User provided files, found at the top level of the repository. *)
val jenga_root : Path.Rel.t
val jenga_conf : Path.Rel.t

(** To be used with [Path.Repo.set_root] to initialize the repo-root-dependent parts of
    [Path] module (see [discover_and_set_root]).
    Can't be async because we need to call it before initializing Async.Parallel. *)
val discover_root : ?start_dir:string -> unit -> Path.Abs.t Or_error.t
val discover_and_set_root : ?start_dir:string -> unit -> unit Or_error.t

(** Look for the closest ancestor directory containing one of the given files. *)
val find_ancestor_directory_containing :
  ?start_dir:string -> one_of:Path.Rel.t list -> Path.Abs.t Or_error.t

val when_did_build_finish_most_recently :
  root_dir:Path.Abs.t -> Time.t option Deferred.t
