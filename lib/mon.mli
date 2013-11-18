
open Core.Std

type t with bin_io

val snap : Progress.t -> t
val progress : t -> Progress.Snapped.t
val effort : t -> Effort.Snapped.t
