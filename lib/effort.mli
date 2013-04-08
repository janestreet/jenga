
open Core.Std
open Async.Std

module Counter : sig

  type t
  val create : string -> t

end

val track : Counter.t -> (unit -> 'a Deferred.t) -> 'a Deferred.t

type t

val create : Counter.t list -> t

val reset_to_zero : t -> unit

module Snapped : sig
  type t
  val to_string :  t -> string
end

val snap : t -> Snapped.t
