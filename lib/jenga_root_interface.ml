
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module type S = sig
  val setup : unit -> Description.Env.t
end

let univ_constr =
  (Univ.Constr.create "Jenga_root_interface.S" sexp_of_opaque : (module S) Univ.Constr.t)
