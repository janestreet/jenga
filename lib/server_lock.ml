
open Core.Std
open Async.Std

module Info = struct

  type t = {
    host : string;
    port : int;
  } with sexp,fields

  let create ~port =
    let host = Unix.gethostname () in
    { host; port; }

end

let nfs_lock = ".nfs_lock"

let lock_running_server ~root_dir ~port =
  let lock_filename = root_dir ^/ Path.Rel.to_string Special_paths.lock in
  In_thread.run (fun () -> Core.Std.Lock_file.Nfs.create lock_filename)
  >>= function
  | Error _ ->
    Message.error "jenga already running, lockfile = %s" (lock_filename ^ nfs_lock);
    Quit.quit Exit_code.server_locked;
    Deferred.never()
  | Ok () ->
    let server_filename = root_dir ^/ Path.Rel.to_string Special_paths.Dot_jenga.server in
    let info = Info.create ~port in
    Message.trace "server info: %s -> %s"
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
  let server_filename = root_dir ^/ Path.Rel.to_string Special_paths.Dot_jenga.server in
  Sys.file_exists server_filename >>= function
  | `No | `Unknown -> return `server_not_running
  | `Yes ->
    Reader.load_sexp_exn server_filename Info.t_of_sexp >>= fun info ->
    return (`info info)
