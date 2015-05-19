
open Core.Std
open Async.Std

type t =
| Process of Job.t
| Save of Save_description.t
with sexp_of

let job = function
  | Process pd -> pd
  | Save sd -> Save_description.job sd

let enqueue_external_job progress f =
  Progress.enqueue_job progress (fun () ->
    if Quit.is_quitting()
    then (
      Deferred.return (Error (`other_error Job.Shutdown));
    )
    else (
      f ()
    )
  )

let run t ~message ~output ~putenv ~progress ~config ~need =
  match t with
  | Save save_description ->
    begin
      message();
      Monitor.try_with (fun () ->
        Save_description.run save_description >>= fun () ->
        Deferred.return (Job.Output.none output)
      ) >>| function
      | Ok x -> Ok x
      | Error exn ->
        let exn = Monitor.extract_exn exn in
        Error (`other_error exn)
    end
  | Process job ->
    enqueue_external_job progress (fun () ->
      message();
      Effort.track Progress.actions_run (fun () ->
        Job.run job ~config ~need ~putenv ~output
      )
    )

let of_job job = Process job

let process ~dir ~prog ~args =
  Process (Job.create ~dir ~prog ~args)

let save1 ~contents ~target ~chmod_x =
  Save (Save_description.create ~contents ~target ~chmod_x)

let save ?chmod_x contents ~target =
  let chmod_x = match chmod_x with Some() -> true | None -> false in
  save1 ~contents ~target ~chmod_x
