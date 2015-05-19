open! Core.Std

type t =
  { sigstop_on_thread_pool_stuck : bool with default(false);
  }
with sexp

let default = <:of_sexp< t >> (Sexp.List [])

let env_var = "JENGA_OPTIONS"

let t =
  match Sys.getenv env_var with
  | None -> default
  | Some "" ->
    Printf.eprintf !"%s should be set to a value of the form:\n%{sexp#hum:t}\n%!"
      env_var default;
    exit 1
  | Some str ->
    match Sexp.of_string_conv_exn str <:of_sexp< t >> with
    | exception e ->
      Printf.eprintf !"Environment variable %s has a bad value:\n%{Exn}\n%!"
        env_var e;
      exit 1
    | t -> t
;;
