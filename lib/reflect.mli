(** The main entry point to the reflection api, which is documented in [api.mli]. *)

val alias : Alias.t -> Path.t list Dep.t
val path : Path.t -> Reflected.Trip.t option Dep.t

val reachable :
  keep:(Path.t -> bool) ->
  ?stop:(Path.t -> bool) -> (* defaults to: [not keep] *)
  Path.t list ->
  Reflected.Trip.t list Dep.t

val putenv : (string * string option) list Dep.t
