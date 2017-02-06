(** Layer error monad within tenacious monad *)

open Core
open Async

module Problem : sig
  type t

  (** The list of individual errors together with dependency traces.
     (a, [b,c,d]) means
     "error 'a' happened at subgoal 'd',
     which was a part of subgoal 'c',
     which was a part of subgoal 'b'".
  *)
  val reasons : t -> (Reason.t * Goal.t list) list

  (** The list of errors that happened at the current subgoal.
    This should be true:
    [reasons_here t = List.filter_map (reasons t) ~f:(function
      | (r, []) -> Some r
      | (r, _) -> None)]
    *)
  val reasons_here : t -> Reason.t list

  (** This is a superset of the needs listed in [reasons]
      because a single Reason.t can be reachable by multiple path,
      all of needs along which are going to be in error.
  *)
  val needs_in_error : t -> Goal.Set.t

  val create : Reason.t -> t
  val all : t list -> t
  val subgoal : Goal.t -> t -> t
end

type 'a t

val wrap : ('a, Problem.t) Result.t Tenacious.t -> 'a t
val expose : 'a t -> ('a, Problem.t) Result.t Tenacious.t

val of_tenacious : 'a Tenacious.t -> 'a t

val return : 'a -> 'a t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val ( *>>| ) : 'a t -> ('a -> 'b) -> 'b t

val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t

val memoize : name:String.t Lazy.t -> 'a t -> 'a t

val error : Reason.t -> 'a t
val all : 'a t list -> 'a list t
val all_unit : unit t list -> unit t
val subgoal : Goal.t -> 'a t -> 'a t

val both : 'a t -> 'b t -> ('a * 'b) t

val of_deferred : (unit -> 'a Deferred.t) -> 'a t

val desensitize : 'a t -> ('a * Tenacious.Heart.t) t
val sensitize : Tenacious.Heart.t -> unit t

val bracket
  :  'a t
  -> running:(int -> unit)
  -> finished:(('a, Problem.t) Result.t -> unit)
  -> cancelled:(unit -> unit)
  -> 'a t

val uncancellable : 'a t -> 'a t

val return_result : ('a, Reason.t) Result.t -> 'a t
val bind_result : 'a t -> ('a -> ('b, Reason.t) Result.t) -> 'b t
