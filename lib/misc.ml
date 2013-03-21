
open Core.Std
open Async.Std

let err s = return (Or_error.error_string s)

let try_load tag t_of_sexp path =
  let filename = Path.to_absolute_string path in
  let err s = err (sprintf "%s: load: %s -- %s" tag filename s) in
  Sys.file_exists filename >>= function
  | `No -> err "file_exists - No"
  | `Unknown -> err "file_exists - Unknown"
  | `Yes ->
    try_with (fun () ->
      Reader.load_sexp_exn filename t_of_sexp
    ) >>= function
    | Ok x -> return (Ok x)
    | Error exn -> err (Exn.to_string exn)

let try_save t tag sexp_of_t path =
  let filename = Path.to_absolute_string path in
  let err s = err (sprintf "%s: save: %s -- %s" tag filename s) in
  try_with (fun () ->
    Writer.save_sexp ~fsync:true(*?*) ~hum:true filename (sexp_of_t t)
  ) >>= function
  | Ok x -> return (Ok x)
  | Error exn -> err (Exn.to_string exn)
