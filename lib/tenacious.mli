
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

(* The types for exec/lift now pass a [cancel:Heart.t] to allow the following equality:

    t == lift (fun ~cancel -> exec t ~cancel)
*)

val exec : 'a t -> cancel:Heart.t -> ('a * Heart.t) option Deferred.t
val lift :        (cancel:Heart.t -> ('a * Heart.t) option Deferred.t) -> 'a t


val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val all : 'a t list -> 'a list t

val when_redo : 'a t -> f:(unit -> unit) -> 'a t
val all_unit : unit t list -> unit t


(* Simpler versions of lift/exec; with no requirement to pass the [cancel:Heart.t].
   Sadly these dont have the nice semantic equality as the unsuffixed versions above.

   t !== lift1 (fun () -> exec1 t)     (* DIS-equality *)

   The RHS versions forms a barrier to cancels.
*)

val exec1 : 'a t -> ('a * Heart.t) Deferred.t
val lift1 : (unit -> ('a * Heart.t) Deferred.t) -> 'a t
