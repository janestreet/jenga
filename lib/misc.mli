
open Core.Std
open Async.Std

val jenga_root_basename : string
val jenga_conf_basename : string

val in_async : f:(unit -> int Deferred.t) -> 'a

val persist_is_modified : unit -> bool
val set_persist_is_saved : unit -> unit
val mod_persist : 'a -> 'a
