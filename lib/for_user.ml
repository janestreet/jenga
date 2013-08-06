
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

(* user access to config/fs *)

let the_installed_config = ref None
let install_config_for_user_rules config = the_installed_config := Some config

let config() =
  match !the_installed_config with
  | Some config -> config
  | None -> assert false

let the_installed_fs = ref None
let install_fs_for_user_rules fs = the_installed_fs := Some fs

let fs() =
  match !the_installed_fs with
  | Some fs -> fs
  | None -> assert false

exception Extract_fail_on_step of int
let extract_location exn =
  let open Sexp in
  let sexp = Exn.sexp_of_t exn in
  (*Message.message "extract_location: %s" (Sexp.to_string sexp);*)
  let fail n = raise (Extract_fail_on_step n) in
  try
    let sexp = match sexp with | List [_;x] -> x | _ -> fail 1 in
    let sexp = match sexp with | List [_;x] -> x | _ -> fail 2 in
    let sexp = match sexp with | List [x] -> x | _ -> fail 3 in
    let sexp =
      match sexp with
      | List [Atom "Sexplib.Conv.Of_sexp_error"; x; _] -> x
      | _ -> fail 4
    in
    let sexp =
      match sexp with
        List [Atom "Sexplib.Sexp.Annotated.Conv_exn"; x ; _ ] -> x
      | _ -> fail 5
    in
    let loc_string = match sexp with Atom x -> x | _ -> fail 6 in
    let line,col =
      match String.rsplit2 loc_string ~on:':' with | None -> fail 7
      | Some (file_line,col) ->
        match String.rsplit2 file_line ~on:':' with | None -> fail 8
        | Some (_,line) ->
          try (Int.of_string line, Int.of_string col) with
          | _ -> fail 9
    in
    line,col
  with
  | Extract_fail_on_step n ->
    Message.error "jbuild sexp location extract error on step %d" n;
    (* dummy location; use col to indicate the extraction failure step number *)
    (1,n)


let load_for_jenga_with ~reader_load t_of_sexp path =
  File_access.enqueue (fun () ->
    let filename = Path.to_string path in
    Monitor.try_with (fun () ->
      reader_load filename t_of_sexp
    ) >>| function
    | Ok t -> t
    | Error exn ->
      let exn = Monitor.extract_exn exn in
      let loc = extract_location exn in
      Message.load_sexp_error path ~loc exn;
      failwith "load_sexp_for_jenga"
  )

let load_sexp_for_jenga t_of_sexp = (* one *)
  load_for_jenga_with ~reader_load:Reader.load_sexp_exn t_of_sexp

let load_sexps_for_jenga t_of_sexp = (* more *)
  load_for_jenga_with ~reader_load:Reader.load_sexps_exn t_of_sexp
