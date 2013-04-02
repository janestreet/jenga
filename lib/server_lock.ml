
open Core.Std
open Async.Std


module Where = struct

  type t = {
    host : string;
    pid : Pid.t;
    port: int;
  } with sexp

  let create ~port =
    let host = Unix.gethostname () in
    let pid = Unix.getpid () in
    { host; pid; port; }

  let to_string t = Sexp.to_string (sexp_of_t t)

end


let lock_running_server ~root_dir  ~port =
  let lock_filename = root_dir ^/ Path.lock_basename in
  let where = Where.create ~port in
  let server_info_string = Where.to_string where in
  Message.message "server lock: %s -> %s" server_info_string lock_filename;
  In_thread.run (fun () ->
    Core.Std.Lock_file.Nfs.create ~message:server_info_string lock_filename
  ) >>= function
  | true -> return ()
  | false ->
    Message.error "jenga already running, lockfile = %s" lock_filename;
    Shutdown.shutdown 1;
    Deferred.never()


let server_location ~root_dir =
  let lock_filename = root_dir ^/ Path.lock_basename in
  (* knowledge of internals of nfs file locking is a bit sad... *)
  let lock_filename = lock_filename ^ ".nfs_lock" in
  Sys.file_exists lock_filename >>= function
  | `No -> return `server_not_running
  | `Yes | `Unknown ->
    Reader.load_sexp_exn lock_filename Where.t_of_sexp >>= fun t ->
    return (`hostname_and_port (t.Where.host, t.Where.port))
