
type t =
| Use_persistent_state
| Artifacts of (dir:Path.t -> Path.t list Dep.t)
