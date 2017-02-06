(** This sandbox is a way of running the action that attempts to detect rules that are
    incorrectly described. We run the action in such a way that relative paths in actions
    can only access specified dependencies (to find missing dependencies). Files that are
    specified to be targets but are not created, or created but not specified to be
    targets, will be found as well. *)

open! Core
open! Async

type kind = Db.Sandbox_kind.t =
  | No_sandbox
  | Hardlink
  | Copy
  | Hardlink_ignore_targets
  | Copy_ignore_targets
[@@deriving sexp]

type t

val root : t -> Path.t

type error =
  | Creation_failed of exn
  | Closing_failed of exn
  | Unexpected_targets of string list
  | Missing_targets of Path.Rel.t list
[@@deriving sexp_of]

val with_sandbox :
  dir:Path.t
  -> deps:Db.Proxy_map.t
  -> kind:kind
  -> targets:Path.Rel.t list
  -> f:(t -> ('a, 'err) Result.t Deferred.t)
  -> (('a, 'err) Result.t, error) Result.t Deferred.t
