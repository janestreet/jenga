
open! Core

(* [Reflected.Action.t] and [Reflected.Trip.t] are returned through the API in response to
   the reflection functions, which can discover the current rule-triple which generate a
   given [Path.t] or [Alias.t] *)

module Action : sig
  type t = Action.t [@@deriving sexp_of]
  val dir : t -> Path.t
  (** [to_sh_ignoring_dir t] returns a shell script representing the action,
      that expects to be run from [dir t]. *)
  val to_sh_ignoring_dir : t -> string
  val string_for_one_line_make_recipe_ignoring_dir : t -> string
end

module Trip : sig
  type t = {
    targets: Path.t list;
    deps : Path.t list;
    action : Action.t;
  } [@@deriving sexp_of]
  val to_string : t -> string
end
