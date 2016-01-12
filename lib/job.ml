
open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

include Db.Job

let to_sh_ignoring_dir t =
  Message.Q.shell_escape_list (prog t :: args t)

(* returns a bash script that expects to be run from the repo root *)
let to_sh_root_relative t =
  if Path.(=) (dir t) Path.the_root
  then to_sh_ignoring_dir t
  else sprintf "cd %s && %s"
         (Message.Q.shell_escape (Path.reach_from ~dir:Path.the_root (dir t)))
         (to_sh_ignoring_dir t)

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

let run t ~config ~need ~putenv ~output =
  let dir = dir t
  and prog = prog t
  and args = args t
  and ignore_stderr = ignore_stderr t
  in
  let {Output.get_result;none=_} = output in
  let where = Path.to_string dir in
  let job_start =
    Message.job_started ~need ~where ~prog ~args
  in
  let start_time = Time.now() in
  begin
    match Config.delay_for_dev config with
    | None -> return ()
    | Some seconds -> Clock.after seconds
  end >>= fun () ->
  let request = Forker.Request.create ~putenv ~dir ~prog ~args in
  Forker.run request >>= fun {Forker.Reply. stdout;stderr;outcome} ->
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
    let duration = Time.diff (Time.now()) start_time in
    let summary =
      Message.job_finished job_start ~outcome ~duration ~stdout ~stderr
    in
    match outcome with
    | `success -> return (Ok (get_result ~stdout))
    | `error _ -> return (Error (`command_failed summary))

let bracket t ~sh_prelude ~sh_postlude =
  let script =
    sprintf "%s; __code=0; (%s) || __code=$?; %s; exit $__code"
      sh_prelude (to_sh_root_relative t) sh_postlude
  in
  create
    ~dir:(Path.the_root)
    ~prog:"bash"
    ~args:[ "-c"; script ]
    ~ignore_stderr:(ignore_stderr t)
