open! Core
open! Int.Replace_polymorphic_compare

type t =
  { cycle_checking_interval :
      Time.Span.t option [@default (Some (Time.Span.of_sec 10.))]
  ; fd_close_timeout             : Time.Span.t [@default Time.Span.of_min 1.]
  ; sigstop_on_thread_pool_stuck : bool [@default false]
  ; compact_and_save_delay       : Time.Span.t [@default Time.Span.of_min 1.]
  ; turn_off_db_saves            : bool [@default false]
  ; turn_off_mtimes_check        : bool [@default false]
  (* Turning off mtimes check is only safe if no one is changing the filesystem, and
     the build system works without self triggering.
     If you are not sure, use it in combination with [turn_off_db_saves] to to avoid
     breaking the future jenga runs. *)
  }
[@@deriving sexp]

let default = [%of_sexp: t] (Sexp.List [])

let env_var = "JENGA_OPTIONS"

let t =
  match Sys.getenv env_var with
  | None -> default
  | Some "" ->
    Printf.eprintf !"%s should be set to a value of the form:\n%{sexp#hum:t}\n%!"
      env_var default;
    exit 1
  | Some str ->
    match Sexp.of_string_conv_exn str [%of_sexp: t] with
    | exception e ->
      Printf.eprintf !"Environment variable %s has a bad value:\n%{Exn}\n%!"
        env_var e;
      exit 1
    | t -> t
;;
