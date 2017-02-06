
open! Core
open! Async

(** [Goal.t] is a build goal, as demanded on the command line or requested by rules.

    [Jengaroot] demands only that the jengaroot configuration is read
    [Path path] demands the target at [path]
    [Alias] demands the particular alias t
*)
type t =
  | Jengaroot
  | Path of Path.Rel.t
  | Alias of Alias.t
[@@deriving sexp, bin_io]
include Hashable_binable with type t := t
include Comparable_binable with type t := t

val to_string : t -> string
val directory : t -> Path.Rel.t

(**
   [parse_string ~dir path] parses a demand string given on the command line.

   Looks at the current state of file system to interpret what the user meant.

   If basename of [path] starts with a '.', it's interpreted as an alias name.
   Otherwise, [path] refers to either [Path path] or [Alias (Alias.default ~dir:path)],
   whichever makes more sense (Alias if directory exists, Path otherwise).
*)
val parse_string : dir:Path.Rel.t -> string -> t Deferred.t
