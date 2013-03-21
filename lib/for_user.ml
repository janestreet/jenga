
open Core.Std
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


let extract_locatopm_from_parse_error sexp =
  let open Sexp in
  match sexp with
  | List (Atom "Reader.load_sexp(s) error"::_) -> Some (1,5) (* TODO *)
  | _ -> None

let extract_location exn =
  (* for some reason,
     I am unable to match directly against the form of the exn *)
  let sexp = Exn.sexp_of_t exn in
  let open Sexp in
  match (extract_locatopm_from_parse_error sexp) with
  | Some loc -> loc
  | None ->
  match sexp with
  | List [
    Atom "Sexplib.Conv.Of_sexp_error";
    List [Atom "Sexplib.Sexp.Annotated.Conv_exn"; Atom loc_string ; _ ];
    _;
  ] ->
    (* base/sexplib/lib/Jenga.conf:4:3
       --> File "Jenga.conf", line 4, characters 3-3: *)
    begin
      match String.rsplit2 loc_string ~on:':' with | None -> (1,4)
      | Some (file_line,col) ->
        match String.rsplit2 file_line ~on:':' with | None -> (1,3)
        | Some (_,line) ->
          try (Int.of_string line, Int.of_string col) with
          | _ -> (1,2)
    end
  | _ -> (1,1) (* make up dummy line,col if fail to extract *)


let load_for_jenga_with ~reader_load t_of_sexp path =
  let filename = Path.to_rrr_string path in
  Monitor.try_with (fun () ->
    reader_load filename t_of_sexp
  ) >>| function
  | Ok t -> t
  | Error exn ->
    let exn = Monitor.extract_exn exn in
    let loc = extract_location exn in
    Message.load_sexp_error path ~loc exn;
    raise exn

let load_sexp_for_jenga t_of_sexp = (* one *)
  load_for_jenga_with ~reader_load:Reader.load_sexp_exn t_of_sexp

let load_sexps_for_jenga t_of_sexp = (* more *)
  load_for_jenga_with ~reader_load:Reader.load_sexps_exn t_of_sexp
