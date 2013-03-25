
open Core.Std
open Async.Std


module Catagory : sig
  type t = Error | Child_error | Waiting | Working | Good
end

module Status : sig
  type t = Unknown | Source | Cat of Catagory.t
end

val dump_graph :
  Discovered_graph.t ->
  status_of_dep:(Description.Dep.t -> Status.t) ->
  unit Or_error.t Deferred.t
