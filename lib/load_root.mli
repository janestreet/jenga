(** This module implements loading the jengaroot (whose type is defined in
    jenga_root_interface.ml) using ocaml_plugin. *)

open! Core
open! Async

val is_loading : unit -> bool

(** A description of what files are to be loaded, ie what is the "jengaroot" exactly. *)
module Spec : sig
  type t
  val ml_file : ml:Path.t -> t
  val config_file : conf:Path.t -> mls:Path.t list -> t
end

val get_env : Spec.t -> [ `Env of Env.t | `Toplevel_exn of Exn.t ] Or_error.t Deferred.t
