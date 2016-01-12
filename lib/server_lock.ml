
open Core.Std
open Async.Std

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

let nfs_lock = ".nfs_lock"

let lock_running_server ~root_dir ~port =
  let nfs_lock_filename =
    Path.Abs.to_string root_dir ^/ Path.Rel.to_string Special_paths.lock
  in
  let local_lock_filename =
    Path.Abs.to_string root_dir ^/ Path.Rel.to_string Special_paths.Dot_jenga.local_lock
  in
  let error lockfile =
    Message.error "jenga already running, lockfile = %s" lockfile;
    Quit.quit Exit_code.server_locked;
    Deferred.never ()
  in
  Lock_file.Nfs.create nfs_lock_filename
  >>= function
  | Error _ -> error (nfs_lock_filename ^ nfs_lock)
  | Ok () ->
    Lock_file.create local_lock_filename
    >>= function
    | false -> error local_lock_filename
    | true ->
      let server_filename =
        Path.Abs.to_string root_dir ^/ Path.Rel.to_string Special_paths.Dot_jenga.server in
      let info = Info.create ~port in
      Message.trace !"server info: %{sexp:Info.t} -> %s" info server_filename;
      Writer.save_sexp ~fsync:true ~hum:true server_filename (Info.sexp_of_t info)
      >>= fun () ->
      at_exit (fun () ->
        (* To reduce user confusion: remove the .server file when jenga is not running *)
        try Core.Std.Unix.unlink (server_filename) with _ -> ()
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
