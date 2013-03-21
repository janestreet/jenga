
open Core.Std
open Async.Std

(* The explicitly represented graph `discovered from the build description.
   This is used for cycyle checking.
   And also provides a facility to dump to file for dev/debug.
*)

module Item : sig

  type t =
  | Root (* todo: remove *)
  | Dep of Description.Dep.t
  | Target_rule of Description.Target_rule.t
  | Gen_key of Description.Gen_key.t

  val to_string : t -> string

end

module Catagory : sig
  type t = Error | Child_error | Waiting | Working | Good
end

module Status : sig
  type t = Unknown | Source | Cat of Catagory.t
end

module Graph : sig

  type t (* the item-graph + roots *)

  val create : unit -> t
  val disregard_roots : t -> unit

  val dump :
    t ->
    status_of_dep:(Description.Dep.t -> Status.t) ->
    unit Or_error.t Deferred.t

end

module Node : sig

  type t
  val create : Graph.t -> root:Item.t -> t
  val new_child : t -> Item.t -> t
  val link_parents : t -> additional:t -> [ `ok | `cycle_report of Item.t list ]
  val kill_existing_children : t -> unit

end
