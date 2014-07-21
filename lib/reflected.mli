
open Core.Std

(* [Reflected.Action.t] and [Reflected.Trip.t] are returned through the API in response to
   the reflection functions, which can discover the current rule-triple which generate a
   given [Path.t] or [Alias.t] *)

module Action : sig
  type t = Job.t
  val dir : t -> Path.t
  val string_for_sh : t -> string
  val string_for_one_line_make_recipe : t -> string
end

module Trip : sig
  type t = {
    targets: Path.t list;
    deps : Path.t list;
    action : Action.t;
  }
  val to_string : t -> string
end
