open Core
open Async
open! Int.Replace_polymorphic_compare

module Info = struct

  type t = {
    host : string;
    port : int;
    pid : Pid.t;
  } [@@deriving sexp, fields]

  let create ~port =
    let host = Unix.gethostname () in
    let pid = Unix.getpid () in
    { host; port; pid; }

end

let lock_running_server ~root_dir ~port =
  let local_lock_filename =
    Path.Abs.to_string root_dir ^/ Path.Rel.to_string Special_paths.Dot_jenga.local_lock
  in
  Lock_file.create local_lock_filename
  >>= function
  | false ->
    let surely_not_taken = local_lock_filename ^ Uuid.to_string (Uuid.create ()) in
    begin
      Lock_file.create ~unlink_on_exit:true surely_not_taken
      >>= function
      | false ->
        Message.error "cannot take any lock in %S (running on nfs?)"
          (Filename.dirname surely_not_taken);
        Quit.exit Exit_code.cant_start;
      | true ->
        Message.error
          "failed to take lock %S (jenga already running?)" local_lock_filename;
        Quit.exit Exit_code.server_locked;
    end
  | true ->
    let server_filename =
      Path.Abs.to_string root_dir ^/ Path.Rel.to_string Special_paths.Dot_jenga.server in
    let info = Info.create ~port in
    Message.trace !"server info: %{sexp:Info.t} -> %s" info server_filename;
    Writer.save_sexp ~fsync:true ~hum:true server_filename (Info.sexp_of_t info)
    >>= fun () ->
    at_exit (fun () ->
      (* To reduce user confusion: remove the .server file when jenga is not running *)
      try Core.Unix.unlink (server_filename) with _ -> ()
    );
    return ()

let server_location ~root_dir =
  let server_filename =
    Path.Abs.to_string root_dir ^/ Path.Rel.to_string Special_paths.Dot_jenga.server in
  Sys.file_exists server_filename >>= function
  | `No | `Unknown -> return `server_not_running
  | `Yes ->
    Reader.load_sexp_exn server_filename Info.t_of_sexp >>= fun info ->
    return (`info info)
