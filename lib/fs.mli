
open Core.Std
open Async.Std

(* Module supporting interface to file-system -- stat, digest, glob.
   Services: Digest caching & inotify wrapping (as hearts).
*)

val lstat_counter : Effort.Counter.t
val digest_counter : Effort.Counter.t
val ls_counter : Effort.Counter.t

module Digest : sig (* proxy for file contents *)
  type t with sexp
  val equal : t -> t -> bool
end

module Glob : sig (* glob specification *)
  type t with sexp
  val to_string : t -> string
  val create : dir:Path.t -> glob_string:string -> t
  val compare : t -> t -> int
end

module Listing : sig (* result of globbing *)
  type t with sexp
  val equal : t -> t -> bool
  val paths : t -> Path.t list
end

module Persist : sig
  type t with sexp
  val create : unit -> t
end

type t (* handle to the file-system *)
val create : Persist.t -> t Deferred.t

module Digest_result : sig
  type t = [
  | `stat_error of Error.t
  | `directory
  | `undigestable
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

val digest_file : t -> file:Path.t -> Digest_result.t Tenacious.t
val list_glob : t -> Glob.t -> Listing_result.t Tenacious.t
