open Core.Std
open Async.Std
open! No_polymorphic_compare

module type S = sig
  val setup : unit -> Env.t Deferred.t
end

let univ_constr =
  let open Ocaml_plugin.Std in
  (Ocaml_dynloader.Univ_constr.create () : (module S) Ocaml_dynloader.Univ_constr.t)
