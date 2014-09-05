
open Core.Std
open Async.Std

module Root : sig
  val discover : unit -> [ `ok | `cant_find_root ]
  val set : dir:string -> unit
end

(* [Path.Rel.t]
   Type for repo root-relative paths. Used throughout jenga.
   Allows repos to be relocated without causing rebuilding.
   Internally represented & displayed without a leading /.
*)
module Rel : sig
  type t with sexp, compare, bin_io
  include Hashable_binable with type t := t
  include Comparable_binable with type t := t
  val the_root : t
  val root_relative : string -> t
  val create_from_absolute : string -> t option
  val relative : dir:t -> string -> t
  val equal : t -> t -> bool
  val split : t -> t * string
  val dirname : t -> t
  val basename : t -> string
  val dotdot : dir:t -> t -> string
  val to_string : t -> string (* repo-root relative string *)
  val to_absolute_string : t -> string
end

module Abs : sig
  (* [Path.Abs.t]
     Type for absolute paths. Always has leading /. *)
  type t with sexp, compare, bin_io
  val create : string -> t
  val to_string : t -> string
end

(* [t] Type for relative or absolute path *)
type t with sexp, compare, bin_io
include Hashable_binable with type t := t
include Comparable_binable with type t := t

val of_relative : Rel.t -> t
val of_absolute : Abs.t -> t
val case : t -> [ `relative of Rel.t | `absolute of Abs.t ]
val is_absolute : t -> bool
val the_root : t
val root_relative : string -> t
val absolute : string -> t
val relative : dir:t -> string -> t
val relative_or_absolute : dir:t -> string -> t
val equal : t -> t -> bool
val dirname : t -> t
val basename : t -> string
val to_string : t -> string (* leading / for absolute; not for relative *)
val to_absolute_string : t -> string

val is_descendant : dir:t -> t -> bool
val reach_from : dir:t -> t -> string (* like [dotdot] but better! *)

val dotdot : dir:t -> t -> string

val is_special_jenga_path : t -> bool

(* [of_absolute_string] - create repo-relative path if possible. *)
val of_absolute_string : string -> t

val relativize_if_possible : t -> t

