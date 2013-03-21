
open Core.Std

module type S = sig
  val setup : unit -> Description.Env.t
end

let univ_constr =
  (Univ.Constr.create "Jenga_root_interface.S" sexp_of_opaque : (module S) Univ.Constr.t)
