open! Core.Std

type t =
  { sigstop_on_thread_pool_stuck : bool [@default false]
  ; turn_off_mtimes_check        : bool [@default false]
  (* Turning off mtimes check is only safe if no one is changing the filesystem, and
     the build system works without self triggering.
     If you are not sure, use it in combination with [turn_off_db_saves] to to avoid
     breaking the future jenga runs. *)
  ; turn_off_db_saves            : bool [@default false]
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
