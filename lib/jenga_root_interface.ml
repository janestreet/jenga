(** [S] is the interface that a jengaroot must provide to jenga. This interface is not
    used when the rules are statically linked in (see build.mli). *)

open Core
open Async
open! Int.Replace_polymorphic_compare

module type S = sig
  val setup : unit -> Env.t Deferred.t
end

let univ_constr =
  (Ocaml_plugin.Dynloader.Univ_constr.create () : (module S) Ocaml_plugin.Dynloader.Univ_constr.t)
