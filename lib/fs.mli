
open Core.Std
open Async.Std

module Digester : sig
  val init : Config.t -> unit (* just once *)
end

(* Module supporting interface to file-system -- stat, digest, glob.
   Services: Digest caching & inotify wrapping (as hearts).
*)

val lstat_counter : Effort.Counter.t
val digest_counter : Effort.Counter.t
val ls_counter : Effort.Counter.t
val mkdir_counter : Effort.Counter.t

module Digest : sig (* proxy for file contents *)
  type t with sexp, bin_io, compare
  val equal : t -> t -> bool
end

module Kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  with sexp, bin_io, compare
  val to_string : t -> string
end

module Glob : sig (* glob specification *)
  type t with sexp, bin_io, compare
  val to_string : t -> string
  val create : dir:Path.t -> kinds:Kind.t list option -> glob_string:string -> t
  val create_from_path : kinds:Kind.t list option -> Path.t -> t
end

module Listing : sig (* result of globbing *)
  type t with sexp, bin_io, compare
  val paths : t -> Path.t list
end

module Persist : sig
  type t with sexp, bin_io
  val create : unit -> t
end

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
  | `is_a_dir
  | `undigestable of Kind.t
  | `digest_error of Error.t
  | `digest of Digest.t
  ]
end

module Listing_result : sig
  type t = [
  | `stat_error of Error.t
  | `not_a_dir
  | `listing_error of Error.t
  | `listing of Listing.t
  ]
end

module Ensure_directory_result : sig
  type t = [`ok | `failed | `not_a_dir]
end

val contents_file : t -> file:Path.X.t -> Contents_result.t Tenacious.t
val digest_file : t -> file:Path.X.t -> Digest_result.t Tenacious.t
val list_glob : t -> Glob.t -> Listing_result.t Tenacious.t
val ensure_directory : t -> dir:Path.X.t -> Ensure_directory_result.t Tenacious.t

val sync_inotify_delivery : t -> sync_contents:string -> 'a Tenacious.t -> 'a Tenacious.t
