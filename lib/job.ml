
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

open Description

let external_jobs_run = Effort.Counter.create "job"

(*----------------------------------------------------------------------
Fork_and_report
----------------------------------------------------------------------*)

exception Shutdown

module Fork_and_report :  sig

  val run :
    Config.t ->
    stdout_expected : bool ->
    get_result: (stdout:string -> 'a) ->
    need:string ->
    rel_path_semantics:Forker.Rel_path_semantics.t ->
    putenv : (string * string) list ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    ('a, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t

end = struct

  let run config ~stdout_expected ~get_result ~need
      ~rel_path_semantics ~putenv ~dir ~prog ~args =
    let where = Path.to_string dir in
    let job_start =
      Message.job_started ~need ~stdout_expected ~where ~prog ~args
    in
    let start_time = Time.now() in
    Effort.track external_jobs_run (fun () ->
      (match Config.delay_for_dev config with
      | None -> return ()
      | Some seconds -> Clock.after seconds
      ) >>= fun () ->
      let request = Forker.Request.create ~rel_path_semantics ~putenv ~dir ~prog ~args in
      Forker.run request
    ) >>= fun {Forker.Reply. stdout;stderr;outcome} ->
    match Heart.is_broken Heart.is_shutdown with
    | true  ->
      return (Error (`other_error Shutdown))
    | false ->
      let duration = Time.diff (Time.now()) start_time in
      Message.job_finished job_start ~outcome ~duration ~stdout ~stderr;
      match outcome with
      | `success -> return (Ok (get_result ~stdout))
      | `error _ -> return (Error `non_zero_status)

end

(*----------------------------------------------------------------------
 Run_external_job
----------------------------------------------------------------------*)

module Run_external_job : sig

  val shell :
    Config.t ->
    need:string ->
    rel_path_semantics:Forker.Rel_path_semantics.t ->
    putenv : (string * string) list ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    (unit, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t

  val shell_stdout :
    Config.t ->
    need:string ->
    rel_path_semantics:Forker.Rel_path_semantics.t ->
    putenv : (string * string) list ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    (string, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t

end = struct

  let shell =
    (* ignore stdout for commands run for effect *)
    Fork_and_report.run
      ~stdout_expected:false
      ~get_result:(fun ~stdout:_ -> ())

  let shell_stdout =
    (* grab stdout for commands run as scanner *)
    Fork_and_report.run
      ~stdout_expected:true
      ~get_result:(fun ~stdout -> stdout)

end

(*----------------------------------------------------------------------
 Run_now
----------------------------------------------------------------------*)

module Run_now = struct

  exception Run_now_of_internal_action_not_supported of Action_id.t
  exception Non_zero_status_from_action_run_now of Action.t

  let lift_for_run_now ~shell action =
    match Action.case action with
    | `id id -> raise (Run_now_of_internal_action_not_supported id)
    | `xaction x ->
      let config = For_user.config() in
      let need = "run_now" in
      let rel_path_semantics =
        Forker.Rel_path_semantics.New_wrt_working_dir
      in
      let putenv = [] in
      let {Xaction.dir;prog;args} = x in
      shell config ~need ~rel_path_semantics ~putenv ~dir ~prog ~args >>= function
      | Error `non_zero_status     -> raise (Non_zero_status_from_action_run_now action)
      | Error (`other_error exn)   -> raise exn
      | Ok x                       -> Deferred.return x

  let run_action_now        = lift_for_run_now ~shell:Run_external_job.shell
  let run_action_now_stdout = lift_for_run_now ~shell:Run_external_job.shell_stdout


end
