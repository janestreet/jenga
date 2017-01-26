(** Various configuration options for jenga meant for development or debugging, not for
    casual users. These are specified as an sexp in the env var "JENGA_OPTIONS". *)

open! Core

type t =
  { cycle_checking_interval : Time.Span.t option
  ; fd_close_timeout : Time.Span.t
  ; sigstop_on_thread_pool_stuck : bool
  ; compact_and_save_delay : Time.Span.t
  ; turn_off_db_saves : bool
  ; turn_off_mtimes_check : bool
  }

val t : t
