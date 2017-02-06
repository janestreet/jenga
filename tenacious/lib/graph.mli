open Core
open Async

(** A global graph of async computations used by tenacious: each node normally corresponds
    to a single execution of a [memoize]: if [memoize]'s heart gets broken, we create a new
    node next time it's demanded.

    There is an edge from [a] to [b] if computation [a] is waiting for result of
    computation [b].
*)

module Node : sig
  type t

  val create : String.t Lazy.t -> t
end

module Dump : sig

  type t = {
    id : Int63.t;
    name : string;
    age : Time.Span.t;
    children : children;
  }
  and children =
    | See_above
    | Here of t list
  [@@deriving sexp]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io]
      type nonrec children = children [@@deriving bin_io]
    end
  end

  val collect : unit -> t

end

(** Any created node that blocks no other node should be made a root, otherwise it doesn't
    participate in the cycle checking and dumping at all. *)
val root_until : node:Node.t -> 'a Deferred.t -> 'a Deferred.t

val edge_until : Node.t -> blocked_on:Node.t -> 'a Deferred.t -> 'a Deferred.t

(**
   Looks for a cycle in the graph.

   If [Some (`Prefix ["a"; "b"], `Cycle ["c"; "d"]) = look_for_a_cycle ()], it means the
   graph has a path "a; b; c; d; c; d; c; ...". *)
val look_for_a_cycle
   : unit -> ([ `Prefix of string list ] * [ `Cycle of string list ]) option
