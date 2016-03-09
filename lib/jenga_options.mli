open! Core.Std

type t =
  { sigstop_on_thread_pool_stuck : bool
  ; turn_off_mtimes_check        : bool
  ; turn_off_db_saves            : bool
  ; cycle_checking_interval : Time.Span.t option
  ; fd_close_timeout             : Time.Span.t
  }

val t : t
