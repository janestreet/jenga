
open Core.Std
open Async.Std

module Counter : sig

  type t
  val create : string -> t

end

val track : Counter.t -> (unit -> 'a) -> 'a

type t
type counter_set = t

val create : Counter.t list -> t

val reset_to_zero : t -> unit

module Counts : sig
  type t with bin_io
  val to_string :  ?limit:counter_set -> t -> string
end

val snap : t -> Counts.t
