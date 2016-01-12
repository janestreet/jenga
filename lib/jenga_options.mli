open! Core.Std

type t =
  { sigstop_on_thread_pool_stuck : bool
  ; turn_off_mtimes_check        : bool
  ; turn_off_db_saves            : bool
  }

val t : t
