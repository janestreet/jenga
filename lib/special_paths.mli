open! Core.Std

(** Files created in the [.jenga] sub-directory. *)
module Dot_jenga : sig

  (** Prepares the directory for special files (mkdir -p .jenga).
      Not async because we do it very early (but after [discover_root]). *)
  val prepare : unit -> unit

  val log : Path.Rel.t
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
val discover_root : unit -> Path.Abs.t Or_error.t
val discover_and_set_root : unit -> unit Or_error.t
