
open Core.Std

module Root : sig
  val discover : unit -> [ `ok | `cant_find_root ]
  val set : dir:string -> unit
end

val db_basename : string
val sexp_db_basename : string
val log_basename : string
val dot_basename : string
val lock_basename : string
val server_basename : string
val plugin_cache_basename : string

(* [Path.Rel.t] == [Path.t]
   Type for repo root-relative paths. Used throughout jenga.
   Allows repos to be relocated without causing rebuilding.
   Internally represented & displayed without a leading /.
*)
module Rel : sig type t end
type t = Rel.t with sexp, compare, bin_io
include Hashable_binable with type t := t
val the_root : t
val root_relative : string -> t
val create_from_absolute : string -> t option
val relative : dir:t -> string -> t
val equal : t -> t -> bool
val split : t -> t * string
val dirname : t -> t
val basename : t -> string
(* [dotdot ~dir path]
compute relative ".."-based-path-string to reach [path] from [dir] *)
val dotdot : dir:t -> t -> string
val to_string : t -> string (* "repo-root-relative" string *)
val to_absolute_string : t -> string
val is_special_jenga_path : t -> bool

module Abs : sig
  (* [Path.Abs.t]
     Type for absolute paths. Always has leading /. *)
  type t with sexp, compare, bin_io
  val create : string -> t
  val to_string : t -> string
end

module X : sig
  (* [Path.X.t]
     Type for relative or absolute path *)
  type t with sexp, compare, bin_io
  include Hashable_binable with type t := t
  val of_relative : Rel.t -> t
  val of_absolute : Abs.t -> t
  val case : t -> [ `relative of Rel.t | `absolute of Abs.t ]
  val basename : t -> string
  val dirname : t -> t
  val to_string : t -> string (* leading / for absolute; not for relative *)
  val to_absolute_string : t -> string
end
