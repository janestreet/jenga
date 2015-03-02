
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let special_prefix = ".jenga"
let special suf = special_prefix ^ suf

let log_basename = special ".debug"
let db_basename = special ".db"
let lock_basename = special ".lock"
let server_basename = special ".server"
let plugin_cache_basename = special ".plugin-cache"

let jenga_root_basename =
  match Core.Std.Sys.getenv "JENGA_ROOT_BASENAME" with
  | None -> "jengaroot.ml"
  | Some x -> x

let jenga_conf_basename =
  match Core.Std.Sys.getenv "JENGA_CONF_BASENAME" with
  | None -> "jenga.conf"
  | Some x -> x

let in_async ~f = (* used by: jem / offline, not jenga *)
  Deferred.unit >>> (fun () ->
    f () >>> (fun n ->
      Shutdown.shutdown n (* only called by: jem / offline, not jenga *)
    )
  );
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())
