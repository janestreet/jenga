
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Shell = Async_shell

let lines s =
  match s with
  | "" -> []
  | "\n" -> [""]
  | _ ->
    let s =
      match String.chop_suffix s ~suffix:"\n" with
      | None -> s
      | Some s -> s
    in
    String.split s ~on:'\n'


let run_shell =
  fun ~delay_for_dev ~need ~dir ~prog ~args ~stdout_expected ~get_result ->

    let where = Path.to_rrr_string dir in

    let job_start =
      Message.job_started ~need ~stdout_expected ~where ~prog ~args
    in
    let start_time = Time.now() in
    try_with (fun () ->
      (match delay_for_dev with
      | None -> return ()
      | Some seconds -> Clock.after seconds
      ) >>= fun () ->
      Shell.run_full_and_error ~working_dir:(Path.to_absolute_string dir) prog args
    ) >>= fun result ->
    let duration = Time.diff (Time.now()) start_time in

    match result with
    | Ok (stdout,stderr) ->
      let result = get_result ~stdout in
      let stdout = lines stdout in
      let stderr = lines stderr in
      let outcome = `success in
      Message.job_finished job_start ~outcome ~duration ~stdout ~stderr;
      return (Ok result)

    | Error exn ->
      let exn = Monitor.extract_exn exn in
      let module SP = Shell.Process in

      match exn with
      | Shell.Process.Failed res ->
        let outcome = `error (SP.status_to_string res.SP.status) in
        let stdout = lines res.SP.stdout in
        let stderr = lines res.SP.stderr in
        Message.job_finished job_start ~outcome ~duration ~stdout ~stderr;
        return (Error `non_zero_status)

      | _ -> (* what? *)
        let outcome = `error (Exn.to_string exn) in
        let stdout = [] in
        let stderr = [] in
        Message.job_finished job_start ~outcome ~duration ~stdout ~stderr;
        return (Error (`other_error exn))


module Scheduler = struct

  type t = {
    delay_for_dev : Time.Span.t option;
    throttle : Throttle.t;
  }

  let create ~delay_for_dev ~max_concurrent_jobs =
    let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs in
    { delay_for_dev; throttle; }

  let shell t ~need ~dir ~prog ~args =
    Throttle.enqueue t.throttle (fun () ->
      run_shell ~delay_for_dev:t.delay_for_dev ~need ~dir ~prog ~args
        (* ignore stdout for commands run for effect *)
        ~stdout_expected:false
        ~get_result:(fun ~stdout:_ -> ())
    )

  let shell_stdout t ~need ~dir ~prog ~args =
    Throttle.enqueue t.throttle (fun () ->
      run_shell ~delay_for_dev:t.delay_for_dev ~need ~dir ~prog ~args
        (* grab stdout for commands run as scanner *)
        ~stdout_expected:true
        ~get_result:(fun ~stdout -> stdout)
    )

end

include Scheduler
