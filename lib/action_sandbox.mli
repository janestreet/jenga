(** This sandbox is a way of running the action that attempts to detect rules that are
    incorrectly described. We run the action in such a way that relative paths in actions
    can only access specified dependencies (to find missing dependencies). Files that
    are specified to be targets but are not created will be found as well. *)

open! Core.Std

val maybe_sandbox
  :  sandbox:bool
  -> Action.t
  -> deps:Db.Proxy_map.t
  -> targets:Path.Rel.t list
  -> Action.t
