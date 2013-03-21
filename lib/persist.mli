
open Core.Std
open Async.Std

type t

val create_saving_periodically : filename:string -> Time.Span.t -> t Deferred.t
val save_now : t -> unit Deferred.t

val fs_persist : t -> Fs.Persist.t
val build_persist : t -> Build.Persist.t
