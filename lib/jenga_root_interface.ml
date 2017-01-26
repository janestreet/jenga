(** [S] is the interface that a jengaroot must provide to jenga. This interface is not
    used when the rules are statically linked in (see build.mli). *)

open Core
open Async.Std
open! Int.Replace_polymorphic_compare

module type S = sig
  val setup : unit -> Env.t Deferred.t
end

let univ_constr =
  let open Ocaml_plugin.Std in
  (Ocaml_dynloader.Univ_constr.create () : (module S) Ocaml_dynloader.Univ_constr.t)
