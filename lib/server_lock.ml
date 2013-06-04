
open Core.Std
open Async.Std


module Info = struct

  type t = {
    host : string;
    port: int;
  } with sexp,fields

  let create ~port =
    let host = Unix.gethostname () in
    { host; port; }

end


let nfs_lock = ".nfs_lock" (* Internal knowledge of nfs locks... sad *)


let lock_running_server ~root_dir  ~port =
  let lock_filename = root_dir ^/ Path.lock_basename in
  In_thread.run (fun () ->
    try (Core.Std.Lock_file.Nfs.create_exn lock_filename; true)
    with _ -> false
  ) >>= function
  | false ->
    Message.error "jenga already running, lockfile = %s" (lock_filename ^ nfs_lock);
    Shutdown.shutdown 1;
    Deferred.never()
  | true ->
    let server_filename = root_dir ^/ Path.server_basename in
    let info = Info.create ~port in
    Message.message "server info: %s -> %s"
      (Sexp.to_string (Info.sexp_of_t info))
      server_filename;
    Writer.save_sexp ~fsync:true ~hum:true server_filename (Info.sexp_of_t info)
    >>= fun () ->
    at_exit (fun () ->
      (* To reduce user confusion: remove the .server file when jenga is not running *)
      (try Core.Std.Unix.unlink (server_filename) with _ -> ());
    );
    return ()


let server_location ~root_dir =
  let server_filename = root_dir ^/ Path.server_basename in
  Sys.file_exists server_filename >>= function
  | `No | `Unknown -> return `server_not_running
  | `Yes ->
    Reader.load_sexp_exn server_filename Info.t_of_sexp >>= fun info ->
    return (`info info)
