
open Core.Std
open Async.Std

(* Type for "Tenacious computations" ['a t]

   Tenacious computation are constructed by composing `redo-able' leaf computations
   expressed as functions of type:

       unit -> ('a * Heart.t) Deferred.t

   Leaf computations are `lifted' into the tenacious-compuation type, and then composed
   sequentially, using monadic [bind], or in parallel, with [all].

   While running, tenacious computations detect "as early as possible" if they have become
   invalid or `broken', as indicated by [Heart.is_broken heart] being [true] for any heart
   returned by a lifted deferred computation which has completed.

   If a lifted computation becomes broken, it will be re-run (if it's still required).
   Re-running will occur at the earliest possible moment.

   If a (now broken) value has been used to construct other tenacious computation (i.e.
   it was passed to the function on the RHS of bind), the dependent computations will be
   stopped/re-run as necessary.

   If one of a collection of tenacious computation combined in parallel using [all]
   becomes broken, while some of its siblings are still running, then the broken tenacious
   computation will be re-run immediately.

   Once a value escapes the tenacious-computation monad, i.e. by calling [exec], then the
   caller becomes responsible for monitoring the associated [Heart.t] if this is
   necessary.

   Lifted leaf computations are not stopped once started. But no further leaf computations
   will be started if they are only referenced from a tenacious computation which is no
   longer required.
*)

type 'a t
type 'a node

(* lift/exec
   Unlike the cancelable versions below, these DON'T have the nice semantic equality:

        t == lift_uncancelable (fun () -> exec t)     (* NOT TRUE *)

   because the RHS forms a barrier to cancels.
   However, these version are more useful, and the more commonly used.
*)

val exec : 'a t -> ('a * Heart.t) Deferred.t
val lift : (unit -> ('a * Heart.t) Deferred.t) -> 'a t

val with_tenacious
  :  'a t
  ->  f:( cancel:Heart.t
     -> (cancel:Heart.t -> ('a * Heart.t) option Deferred.t)
     -> ('b * Heart.t) option Deferred.t)
  -> 'b t

(* standard combinators *)

val return : 'a -> 'a t
val map    : 'a t -> f:('a -> 'b) -> 'b t
val bind   : 'a t -> ('a -> 'b t) -> 'b t
val all    : 'a t list -> 'a list t
val reify  : 'a t -> 'a node
val use    : 'a node -> 'a t

(*
  lift_cancelable / exec_cancelable

  The following semantic equality should hold...
    t == lift_cancelable (fun ~cancel -> exec_cancelable t ~cancel)

  [lift_cancelable] allows lifted computation to know they are cancelled.

  [exec_cancelable t ~cancel >>= fun res -> ... ] allows the caller to notify his
  disinterest in the exec'ed tenacious by means of the [cancel] heart being broken,
  in which case [res] may be [None].

  If the caller cancels, he has no way of knowing when computations run as part of the
  tenacious finish - they may not even be cancelled if there are other uses.

  The caller MAY assume the result will only be [None] if he cancels.
*)

val exec_cancelable : 'a t -> cancel:Heart.t -> ('a * Heart.t) option Deferred.t
val lift_cancelable :        (cancel:Heart.t -> ('a * Heart.t) option Deferred.t) -> 'a t


(* non primitive ops... *)

val all_unit : unit t list -> unit t

