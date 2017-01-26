open! Core

(** A table of registered environment variables, with typed access *)

(** [Var.t] is a typed environment variable *)
type 'a t [@@deriving sexp_of]

(** It is an error to register a [name] more than once, unless it has been unregistered by
    [unregister name] or [clear_all_registrations] *)

val register                : ?choices:string list -> string                    -> string option t
val register_with_default   : ?choices:string list -> string -> default:string  -> string t

(** Clears the set of known vars, but if variable by the same name are recreated later,
    the relevant [setenv] will still be in effect.  *)
val clear_all_registrations : unit -> unit

val unregister : string -> unit
val registered_names : unit -> string list
val is_registered : string -> bool

module Info : sig
  type t = {
    name    : string;
    default : string option;
    choices : string list;
    value   : string option; (** None is unset *)
    peeked  : bool;
  } [@@deriving sexp_of]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io]
    end
  end
end

val change_to_peeked_var : unit Tenacious.t

val all_registered : unit -> Info.t list

val peek : ?dont_trigger:unit -> 'a t -> 'a
val watch: 'a t -> 'a Or_error.t Tenacious.t
val map  : 'a t -> f:('a -> 'b) -> 'b t

module Getenv : sig
  type query
  type response = Info.t Or_error.t
  val query : string -> query
  val run : query -> response

  module Stable : sig
    module V1 : sig
      type nonrec query = query [@@deriving bin_io]
      type nonrec response = response [@@deriving bin_io]
    end
  end
end

module Setenv : sig
  type query
  type response = unit Or_error.t
  val query : string -> value:string option -> query
  val run : query -> response

  module Stable : sig
    module V1 : sig
      type nonrec query = query [@@deriving bin_io]
      type nonrec response = response [@@deriving bin_io]
    end
  end
end
