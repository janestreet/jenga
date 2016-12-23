(** See api.mli for documentation. *)

module Loc : sig
  module Source : sig
    type t =
      | File of Path.t
      | Other of string
  end

  type t =
    { source    : Source.t
    ; line      : int
    ; start_col : int
    ; end_col   : int
    }
end

type t = { loc : Loc.t; message : string }

exception E of t

val create : loc:Loc.t -> string -> t
val create' : ?line:int -> ?start_col:int -> ?end_col:int -> source:Loc.Source.t -> string -> t

val raise : loc:Loc.t -> string -> _
val raisef : loc:Loc.t -> ('a, unit, string, unit -> _) format4 -> 'a

val to_lines : t -> dir:Path.Rel.t -> string list
