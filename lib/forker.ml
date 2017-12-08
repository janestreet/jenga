open Core
open Async
open! Int.Replace_polymorphic_compare

let heartbeat_config =
  Rpc.Connection.Heartbeat_config.create
    ~timeout: Time_ns.Span.max_value
    ~send_every: (Time_ns.Span.of_sec 10.)

module Request = struct
  type t = {
    putenv : (string * string option) list;
    dir_as_string : string; (* can't send path straighforwardly because of
                               the bin_io in interning.ml *)
    prog : string;
    args : string list;
  } [@@deriving of_sexp, bin_io]
  let create ~putenv ~dir ~prog ~args =
    { putenv; dir_as_string = Path.to_string dir; prog; args }
end

module Reply = struct
  type t = {
    stdout : string;
    stderr : string;
    outcome : [`success | `error of string];
    duration : Time.Span.t;
  } [@@deriving sexp_of, bin_io]

  let of_error error ~duration =
    let exn = Error.to_exn error in
    let outcome = `error (Exn.to_string exn) in
    let stdout = "" in
    let stderr = "" in
    { stdout; stderr; outcome; duration }
end

module Fork = struct
  type query = Request.t [@@deriving of_sexp, bin_io]
  type response = Reply.t [@@deriving sexp_of, bin_io]
  let rpc = Rpc.Rpc.create ~name:"fork" ~version:0 ~bin_query ~bin_response

  let implementation { Request. putenv; dir_as_string; prog; args } =
    let start = Time.now () in
    List.iter putenv ~f:(fun (key, data) ->
      match data with
      | None -> Core.Unix.unsetenv key
      | Some data -> Core.Unix.putenv ~key ~data);
    Process.create ~working_dir:dir_as_string ~prog ~args () >>= function
    | Error e -> return (Reply.of_error e ~duration:(Time.diff (Time.now ()) start))
    | Ok process ->
      let stdout = Process.stdout process in
      let stderr = Process.stderr process in
      let out_err = Deferred.both (Reader.contents stdout) (Reader.contents stderr) in
      Deferred.both
        (Writer.close (Process.stdin process))
        (Process.wait process)
      >>= fun ((), exit_status) ->
      let timeout = Jenga_options.t.fd_close_timeout in
      Clock.with_timeout timeout out_err
      >>= (function
        | `Result out_err -> return (`Closed_normally, out_err)
        | `Timeout ->
          Deferred.both
            (Deferred.all_unit [ Reader.close stdout; Reader.close stderr ])
            out_err
          >>| fun ((), v) ->
          (`Forcefully_closed timeout, v)
      ) >>= fun (what_happened_to_fds, (stdout, stderr)) ->
      let outcome =
        match exit_status with
        | Error _ -> `error (Unix.Exit_or_signal.to_string_hum exit_status)
        | Ok () ->
          match what_happened_to_fds with
          | `Closed_normally -> `success
          | `Forcefully_closed timeout ->
            `error (sprintf !"stdout or stderr wasn't closed %{Time.Span} after process \
                              exited (due to a stray process perhaps?)" timeout)
      in
      return { Reply. stdout; stderr; outcome; duration = Time.diff (Time.now ()) start }
end

let command =
  let fork_throttle =
    (* We need 3 fds per forked process, so conservatively we
       throttle the number of forked processed to be < 1024/3 *)
    lazy (Throttle.create ~continue_on_error:true ~max_concurrent_jobs:300)
  in
  let module Fork = struct
    include Fork
    let implementation _ query =
      Throttle.enqueue (force fork_throttle) (fun () -> implementation query)
  end in
  Command_rpc.Command.create ~heartbeat_config ~summary:"" [ `Plain (module Fork) ]
;;

type t =
  | Dispatch of Rpc.Connection.t
  | In_process

let t_opt_ref = ref None

let the_t () = match !t_opt_ref with | None -> assert false | Some t -> t

let init config ~args =
  let t_or_error_def =
    if Config.f_number config > 0
    then Command_rpc.Connection.create ~heartbeat_config
           ~propagate_stderr:true ~prog:Sys.executable_name ~args ()
         >>| Or_error.map ~f:(fun conn -> Dispatch conn)
    else return (Ok In_process)
  in
  t_or_error_def
  >>| function
  | Error _ as e -> e
  | Ok t ->
    if Option.is_some !t_opt_ref then failwith "Forker.init() called more than once";
    t_opt_ref := Some t;
    Ok ()

let run request =
  match the_t () with
  | In_process -> Fork.implementation request
  | Dispatch connection ->
    Rpc.Rpc.dispatch Fork.rpc connection request >>| function
    | Error e -> Reply.of_error e ~duration:Time.Span.zero
    | Ok ok -> ok
