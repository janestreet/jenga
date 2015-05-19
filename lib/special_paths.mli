open Core.Std
open Async.Std

(* files created at the top level of the repository *)
val lock : Path.Rel.t

(* files created in the [.jenga] sub-directory *)
module Dot_jenga : sig

  (* prepares the directory for special files (mkdir -p .jenga)
     not async because we do it very early (but after [discover_root]) *)
  val prepare : unit -> unit

  val log : Path.Rel.t
  val server : Path.Rel.t
  val plugin_cache : Path.Rel.t
  val db : version:string -> Path.Rel.t

  val matches : Path.t -> bool

end

(* user provided files, found at the top level of the repository *)
val jenga_root : Path.Rel.t
val jenga_conf : Path.Rel.t

(* can't be async because we need to call it before initializing Async.Parallel *)
val discover_root : unit -> unit Or_error.t
