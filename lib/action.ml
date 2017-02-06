open Core
open Async
open! Int.Replace_polymorphic_compare

type process = {
  dir : Path.t;
  prog : string;
  args : string list;
  ignore_stderr : bool;
  sandbox : Db.Sandbox_kind.t sexp_option;
}
[@@deriving sexp_of]

type t =
  | Process of process
  | Save of Db.Save_proxy.t
[@@deriving sexp_of]

let proxy ~default_sandbox = function
  | Process { dir; prog; args; ignore_stderr; sandbox } ->
    let sandbox = Option.value sandbox ~default:default_sandbox in
    let process = Db.Process_proxy.create ~dir ~prog ~args ~ignore_stderr ~sandbox in
    Db.Action_proxy.Process process
  | Save save -> Db.Action_proxy.Save save

let dir = function
  | Process { dir; _ } -> dir
  | Save sd -> Path.dirname (Db.Save_proxy.target sd)

module Output = struct

  type 'a t = {
    get_result: (stdout:string -> 'a);
    none : 'a;
  }

  let ignore = {
    get_result = (fun ~stdout:_ -> ());
    none = ();
  }

  let stdout = {
    get_result = (fun ~stdout -> stdout);
    none = "";
  }

  let none t = t.none

end

exception Shutdown

let run_save ~contents ~target ~chmod_x ~output =
  let perm = if chmod_x then Some 0o777 else None in
  Monitor.try_with ~extract_exn:true (fun () ->
    File_access.enqueue (fun () ->
      let temp_file =
        (* make all these temp files fit an easily ignorable pattern *)
        let dir, base = Path.split target in
        Filename.concat (Path.to_string dir) (".jenga." ^ base)
      in
      Writer.save ?perm ~temp_file (Path.to_string target) ~contents
    ) >>| fun () ->
    Output.none output
  ) >>| function
  | Ok _ as ok -> ok
  | Error exn -> Error (`other_error exn)

let run_process ~where ~sandboxed ~dir ~prog ~args ~ignore_stderr ~need ~putenv ~output =
  let {Output.get_result;none=_} = output in
  let job_start =
    Message.job_started ~need ~where ~prog ~args ~sandboxed
  in
  let request = Forker.Request.create ~putenv ~dir ~prog ~args in
  Forker.run request >>= fun { stdout; stderr; outcome; duration } ->
  match Quit.is_quitting() with
  | true  ->
    return (Error (`other_error Shutdown))
  | false ->
    let outcome =
      match outcome with
      | `success when not ignore_stderr && not (String.is_empty stderr) ->
        `error "has unexpected stderr"
      | (`success | `error _) as outcome -> outcome
    in
    let summary =
      Message.job_finished job_start ~outcome ~duration ~stdout ~stderr
    in
    match outcome with
    | `success -> return (Ok (get_result ~stdout))
    | `error _ -> return (Error (`command_failed summary))

let run_process_sandboxed ~deps ~targets ~sandbox ~where
      ~dir ~prog ~args ~ignore_stderr ~need ~putenv ~output =
  Sandbox.with_sandbox ~kind:sandbox ~deps ~dir ~targets
    ~f:(fun box ->
      let dir = Path.reach_from ~dir:Path.the_root dir in
      let dir = Path.relative ~dir:(Sandbox.root box) dir in
      run_process ~where ~sandboxed:true
        ~dir ~prog ~args ~ignore_stderr ~need ~putenv ~output)
  >>| function
  | Error err -> Error (`sandbox_error err)
  | Ok res -> res

let run_gen t ~message ~output ~deps ~targets ~putenv ~progress ~need ~default_sandbox =
  match t with
  | Save sd ->
    let contents = Db.Save_proxy.contents sd in
    let target = Db.Save_proxy.target sd in
    let chmod_x = Db.Save_proxy.chmod_x sd in
    message();
    Metrics.Counter.incr Progress.saves_run;
    run_save ~contents ~target ~chmod_x ~output
  | Process { dir; prog; args; ignore_stderr; sandbox } ->
    let sandbox = Option.value sandbox ~default:default_sandbox in
    let where = Path.to_string dir in
    let run () =
      if Quit.is_quitting() then
        return (Error (`other_error Shutdown))
      else begin
        message();
        Metrics.Counter.incr Progress.actions_run;
        match sandbox with
        | No_sandbox ->
          run_process ~where ~sandboxed:false
            ~dir ~prog ~args ~ignore_stderr ~need ~putenv ~output
        | Hardlink | Hardlink_ignore_targets | Copy | Copy_ignore_targets ->
          run_process_sandboxed ~deps ~targets ~sandbox ~where
            ~dir ~prog ~args ~ignore_stderr ~need ~putenv ~output
      end
    in
    match progress with
    | None -> run ()
    | Some progress -> Progress.enqueue_job progress run

let run_now t ~output =
  match%bind
    run_gen t
      ~message:(fun () -> ())
      ~output
      ~deps:Db.Proxy_map.empty
      ~targets:[]
      ~putenv:[]
      ~progress:None
      ~need:"run_now"
      ~default_sandbox:No_sandbox
  with
  | Error (`command_failed _) -> failwith "action_run_now_failed"
  | Error (`other_error exn) -> raise exn
  | Error (`sandbox_error err) -> raise_s [%sexp (`sandbox_error (err : Sandbox.error))]
  | Ok x -> return x

let run t ~message ~output ~deps ~targets ~putenv ~progress ~need ~default_sandbox =
  let progress = Some progress in
  run_gen t ~message ~output ~deps ~targets ~putenv ~progress ~need ~default_sandbox

let escape_backslashes_and_newlines s =
  String.concat_map s ~f:(function
  | '\n' -> "\\n"
  | '\\' -> "\\\\"
  | c -> String.make 1 c)

let to_sh_ignoring_dir = function
  | Process { dir = _; prog; args; ignore_stderr = _; sandbox = _; } ->
    Job_summary.Q.shell_escape_list (prog :: args)
  | Save sd ->
    let contents = Db.Save_proxy.contents sd in
    let target = Db.Save_proxy.target sd in
    let chmod_x = Db.Save_proxy.chmod_x sd in
    let base = Job_summary.Q.shell_escape (Path.basename target) in
    (* By escaping newlines & using "echo -e", we avoid embedded newlines which are tricky
       to escape if we later extract a Makefile *)
    let cmd =
      sprintf "echo -n -e %s > %s%s"
        (Job_summary.Q.shell_escape (escape_backslashes_and_newlines contents))
        base
        (if chmod_x then sprintf "; chmod +x %s" base else "")
    in
    Job_summary.Q.shell_escape_list ["bash"; "-c"; cmd]

let process ~dir ~prog ~args ~sandbox ~ignore_stderr =
  Process {dir; prog; args; ignore_stderr; sandbox}

let save ?chmod_x contents ~target =
  let chmod_x = match chmod_x with Some() -> true | None -> false in
  Save (Db.Save_proxy.create ~contents ~target ~chmod_x)
