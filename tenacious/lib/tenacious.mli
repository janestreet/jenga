
open Core.Std
open Async.Std

(* Type for "Tenacious computations" ['a t]

   Tenacious computation are constructed by composing `redo-able' leaf computations
   expressed as functions of type:

       unit -> ('a * Heart.t) Deferred.t

   Leaf computations are `lifted' into the tenacious-computation type, and then composed
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

   Note: lift/exec.  Sadly these DON'T have the nice semantic equality:
        t == lift (fun () -> exec t)     (* NOT TRUE *)
   because the RHS forms a barrier to cancels.

   Use with_semantics to get a hook into when a computation is evaluated/re-evaluated

   Note. Only reified computations have identity.

*)

type 'a t

val return      : 'a -> 'a t
val map         : 'a t -> f:('a -> 'b) -> 'b t
val bind        : 'a t -> ('a -> 'b t) -> 'b t
val all         : 'a t list -> 'a list t
val all_unit    : unit t list -> unit t

val reify       : 'a t -> 'a t

val lift        :  (unit -> ('a * Heart.t) Deferred.t) -> 'a t
val exec        : 'a t -> ('a * Heart.t) Deferred.t
val cutoff      : equal:('a -> 'a -> bool) -> 'a t -> 'a t


(* Use of with_semantics is tricky & discouraged, unless you
   really understand what you are doing! *)

type 'a result = ('a * Heart.t) option Deferred.t
type 'a semantics = cancel:Heart.t -> 'a result

val with_semantics : 'a t -> f:('a semantics -> 'b semantics) -> 'b t

