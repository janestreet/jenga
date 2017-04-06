(** Module supporting interface to file-system -- stat, digest, glob.
    Services: Digest caching & inotify wrapping (as hearts). *)

open! Core
open! Async

module Ocaml_digest : sig
  val init : Config.t -> unit (* just once *)
end

module Digest = Db.Digest
module Kind = Db.Kind

module Glob : sig (* glob specification *)
  type t = Db.Glob.t [@@deriving sexp, hash, compare]
  include Hashable with type t := t
  val dir : t -> Path.t
  val pattern : t -> Pattern.t
  val kind_allows_file : t -> bool
  val to_string : t -> string
  val create : dir:Path.t -> ?kinds: Kind.t list -> string -> t
  (** matches exactly one path *)
  val create_from_path : kinds:Kind.t list option -> Path.t -> t
end

module Listing = Db.Listing

type t (* handle to the file-system *)
val create : Config.t -> Persist.t -> t Deferred.t

module Contents_result : sig
  type t = [
  | `file_read_error of Error.t
  | `is_a_dir
  | `contents of string
  ]
end

module Digest_result : sig
  type t = [
  | `stat_error of Error.t
  | `does_not_exist
  | `is_a_dir
  | `undigestable of Kind.t
  | `digest_error of Error.t
  | `digest of Digest.t
  ]
end

module Listing_result : sig
  type t = [
    | `does_not_exist
    | `not_a_dir
    | `listing of Listing.t
  ] [@@deriving compare, sexp]
end

val contents_file : t -> file:Path.t -> Contents_result.t Tenacious.t
val digest_file : t -> file:Path.t -> Digest_result.t Tenacious.t
val list_glob : t -> Glob.t -> Listing_result.t Or_error.t Tenacious.t

(** This function either checks the directory exists if [mkdir_root] is None, or creates
    all the directories between [mkdir_root] and [dir] ([mkdir_root] must be a non-strict
    ancestor of [dir]). *)
val ensure_directory
  : t -> mkdir_root:Path.Rel.t option -> dir:Path.Rel.t -> unit Or_error.t Tenacious.t

(** Locks [targets] for writing and masks the corresponding 'file changed' messages *)
val lock_targets_and_mask_updates :
  t -> targets:Path.Rel.t list -> (unit -> 'a Deferred.t) -> 'a Deferred.t

val clear_watcher_cache : t -> Path.t -> needed_for_correctness:bool -> unit

module Mtime : sig
  type t [@@deriving hash, compare, sexp_of]
  val equal : t -> t -> bool
end

val mtime_file : t -> file:Path.t -> Mtime.t option Tenacious.t (* [None] - no file *)
(* [mtime_files_right_now] avoids the cache. It batches stats for performance. *)
val mtime_files_right_now : Path.t list -> ((Path.t * Mtime.t) list, string) Result.t Deferred.t
