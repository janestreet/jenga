
open Core.Std

val discover : marker:string -> [`ok of string | `cant_find_root ]
val set : dir:string -> unit
val get : unit -> string (* must follow successful discover *)
