open Core.Std
open Async.Std
type 'a t
type 'a v = ('a * Heart.t) option

val pure   : 'a -> 'a t
val lift   : (cancel:Heart.t -> 'a v Deferred.t) -> 'a t
val map    : 'a t -> f:('a -> 'b) -> 'b t
val bind   : 'a t -> ('a -> 'b t) -> 'b t
val reify  : 'a t -> 'a t
val all    : 'a t list -> 'a list t
val stable : 'a t -> 'a t
val with_ten : 'a t -> f:(cancel:Heart.t -> (cancel:Heart.t -> ('a * Heart.t) option Deferred.t) -> ('b * Heart.t) option Deferred.t) -> 'b t

val sample : 'a t -> cancel:Heart.t -> 'a v Deferred.t
