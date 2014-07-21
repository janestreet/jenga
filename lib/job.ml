
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module T = struct
  type t = {
    dir : Path.t;
    prog : string;
    args : string list;
  } with sexp, bin_io, compare
  let hash = Hashtbl.hash
end
include T
include Hashable.Make_binable(T)

let create ~dir ~prog ~args = { dir ; prog; args; }

let dir t = t.dir

let string_for_sh {dir=_;prog;args} =
  let args = List.map args ~f:(fun arg -> Message.Q.shell_escape ~arg) in
  sprintf "%s %s" prog (String.concat ~sep:" " args)

module Output = struct

  type 'a t = {
    stdout_expected : bool;
    get_result: (stdout:string -> 'a);
    none : 'a;
  }

  let ignore = {
    stdout_expected = false;
    get_result = (fun ~stdout:_ -> ());
    none = ();
  }

  let stdout = {
    stdout_expected = true;
    get_result = (fun ~stdout -> stdout);
    none = "";
  }

  let none t = t.none

end

exception Shutdown

let run {dir;prog;args} ~config ~need ~putenv ~output =
  let {Output.stdout_expected;get_result;none=_} = output in
  let where = Path.to_string dir in
  let job_start =
    Message.job_started ~need ~stdout_expected ~where ~prog ~args
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
    let duration = Time.diff (Time.now()) start_time in
    let summary =
      Message.job_finished job_start ~outcome ~duration ~stdout ~stderr
    in
    match outcome with
    | `success -> return (Ok (get_result ~stdout))
    | `error _ -> return (Error (`non_zero_status summary))
