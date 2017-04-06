(** The type and operations on filesystem paths. *)

open! Core

(* [Path.Rel.t]
   Type for repo root-relative paths. Used throughout jenga.
   Allows repos to be relocated without causing rebuilding.
   Internally represented & displayed without a leading /.
*)
module Rel : sig
  type t [@@deriving sexp, hash, compare, bin_io]
  include Hashable_binable with type t := t
  include Comparable_binable with type t := t
  val the_root : t
  val create : string -> t
  val relative : dir:t -> string -> t

  (** splits into dirname and basename;
      [split the_root = (the_root, ".")] *)
  val split : t -> t * string
  val dirname : t -> t
  val basename : t -> string
  val parts : t -> string list
  val of_parts : string list -> t
  val is_descendant : dir:t -> t -> bool

  (** [x = reach_from ~dir t] is such that [relative ~dir x = t],
      x starts with a "." or "..", and x is otherwise as short as possible *)
  val reach_from : dir:t -> t -> string
  val to_string : t -> string (* repo-root relative string *)
end

module Abs : sig

  (** Type for absolute paths. *)
  type t [@@deriving sexp_of, hash, compare, bin_io]

  val unix_root : t
  val create : string -> t

  (** Always has leading /. *)
  val to_string : t -> string

  (** append '/'-separated parts to the path.
      suppresses the '.' and empty parts and reduces the ".."s *)
  val relative : dir:t -> string -> t

  val basename : t -> string
  val dirname : t -> t
  val split : t -> t * string

  (** [reach_from ~dir t = x] means [relative ~dir x = t] *)
  val reach_from : dir:t -> t -> string

  (** [is_descendant ~dir t] means [parts dir] is a prefix of [parts t].
      A path is considered a descendant of itself. *)
  val is_descendant : dir:t -> t -> bool
end

(* [t] Type for relative or absolute path *)
type t [@@deriving sexp, hash, compare, bin_io]
include Hashable_binable with type t := t
include Comparable_binable with type t := t

(*** repo-root-invariant: ***)
val of_relative : Rel.t -> t
val of_absolute : Abs.t -> t
val case : t -> [ `relative of Rel.t | `absolute of Abs.t ]
val is_absolute : t -> bool
val the_root : t
val unix_root : t
val root_relative : string -> t
val absolute : string -> t
val relative : dir:t -> string -> t
val split : t -> t * string
val dirname : t -> t
val basename : t -> string
val to_string : t -> string (** leading / for absolute; not for relative *)

(** [reach_from ~dir t = x] means [relative_or_absolute ~dir x = t] *)
val reach_from : dir:t -> t -> string

(** [is_descendant ~dir t = true] means that [reach_from ~dir t = Some x] and
    [relative ~dir x = t] *)
val is_descendant : dir:t -> t -> bool

val relative_or_absolute : dir:t -> string -> t

module Repo : sig
  val set_root : Abs.t -> unit
  val root : unit -> Abs.t
end

(*** repo-root-dependent! ***)
val to_absolute_string : t -> string

(* [of_absolute_string] - create repo-relative path if possible. *)
val of_absolute_string : string -> t

(** [is_a_root t = (t = the_root) || (t = unix_root)]*)
val is_a_root : t -> bool

module With_store : sig
  type 'a t [@@deriving bin_io]
  val snapshot : 'a -> 'a t
  val value : 'a t -> 'a
end
