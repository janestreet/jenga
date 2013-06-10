
(* jem - jenga monitor *)

open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let retry_span = sec 0.5

module Finish_time_estimator : sig

  type t
  val create : decay_factor_per_second:float -> t
  val push_todo : t -> int -> unit
  val estimated_finish_time_string : t -> string

end = struct

  type t = {
    decay_factor_per_second : float;
    mutable rate : float; (* latest estimate of targets/sec *)
    mutable last_push_time : Time.t;
    mutable last_todo : int;
  }

  let create ~decay_factor_per_second = {
    decay_factor_per_second;
    rate = 0.0;
    last_push_time = Time.epoch;
    last_todo = 0;
  }

  let calc_new_rate t ~now ~todo =
    let duration = Time.diff now t.last_push_time in
    let seconds = Time.Span.to_sec duration in
    assert (Float.(seconds >= 0.0));
    let progress = t.last_todo - todo in (* might be negative *)
    let current_rate = float progress /. seconds in
    let decay = t.decay_factor_per_second ** seconds in
    (decay *. t.rate) +. ((1.0 -. decay) *. current_rate)

  let push_todo t todo =
    let now = Time.now () in
    let new_rate = calc_new_rate t ~now ~todo in
    (
      t.last_todo      <- todo;
      t.last_push_time <- now;
      t.rate           <- new_rate
    )

  let to_min_string_no_date time =
    let ofday = Time.to_local_ofday time in
    let span = Time.Ofday.to_span_since_start_of_day ofday in
    let parts = Time.Span.to_parts span in
    let module P = Time.Span.Parts in
    sprintf "%02d:%02d" parts.P.hr parts.P.min

  (*let to_sec_string_no_date t = Time.Ofday.to_sec_string (Time.to_local_ofday t)*)

  let estimated_finish_time_string t =
    if Int.(t.last_todo <= 0) then ", finished"
    else
      (* will never finish with a negative or zero rate *)
      if Float.(t.rate <= 0.0) then ""
      else
        let duration = Time.Span.scale (sec 1.0) (float t.last_todo /. t.rate) in
        (* pointless to show a duration which is silly big *)
        if Time.Span.(duration > Time.Span.of_hr 12.) then ""
        else
          ", finish: " ^ to_min_string_no_date (Time.add (Time.now()) duration)

end

let message fmt = ksprintf (fun s -> Printf.printf "\027[2K%s\r%!" s) fmt

let run mode =

  let root_dir = Init.discover_root() in
  let last_progress = ref None in
  let estimator =
    (* The bigger the decay-factor, the more stable the estimate *)
    Finish_time_estimator.create ~decay_factor_per_second:0.95
  in

  let string_of_progress progress =
    let todo = Mon.Progress.todo progress in
    Finish_time_estimator.push_todo estimator todo;
    let finish = Finish_time_estimator.estimated_finish_time_string estimator in
    Mon.Progress.to_string mode progress ^ finish
  in

  let string_of_last_progress () =
    match !last_progress with
    | None -> "no progress seen!"
    | Some progress -> string_of_progress progress
  in

  let suck_progress_pipe conn =
    let stop = ref false in
    let fresh = ref false in
    don't_wait_for (
      let rec loop q =
        Clock.after (sec 0.5) >>= fun () ->
        if !stop then return ()
        else
          if !fresh then (fresh := false; loop 1)
          else (
            let qmes = String.concat (List.init q ~f:(fun _ -> "?")) in
            message "%s %s" (string_of_last_progress ()) qmes;
            loop (q+1);
          )
      in loop 1
    );
    Rpc.Pipe_rpc.dispatch_exn Rpc_intf.progress_stream conn () >>= fun (reader,_id) ->
    Pipe.iter_without_pushback reader ~f:(fun progress ->
      last_progress := Some progress;
      fresh := true;
      message "%s" (string_of_progress progress);
    ) >>= fun () ->
    stop := true;
    return ()
  in

  let poll_for_connection ~retry =
    Server_lock.server_location ~root_dir >>= function
    | `server_not_running ->
      message "%s (not running)" (string_of_last_progress ()); (*root_dir*)
      retry()
    | `info info ->
      let host = Server_lock.Info.host info in
      let port = Server_lock.Info.port info in
      let server_name = sprintf "%s:%d" host port in
      let where_to_connect = Tcp.to_host_and_port host port in
      try_with (fun () ->
        Tcp.with_connection where_to_connect (fun _ reader writer ->
          Rpc.Connection.create reader writer ~connection_state:() >>= function
          | Error exn ->
            (* when does this ever come?.. *)
            message "with_rpc_connection: %s\n%s" server_name (Exn.to_string exn);
            return false
          | Ok conn ->
            suck_progress_pipe conn >>= fun () ->
            (*message "lost connection with: %s" server_name;*)
            return true
        )
      )
      >>= function
      | Ok ok -> if ok then retry() else return 1
      | Error exn ->
        message "failed to connect with: %s\n%s" server_name (Exn.to_string exn);
        return 2
  in
  let rec retry () =
    Clock.after retry_span >>= fun () ->
    poll_for_connection ~retry
  in
  poll_for_connection ~retry


let main mode =
  Deferred.unit >>> (fun () ->
    run mode >>> (fun n ->
      Shutdown.shutdown n
    )
  );
  never_returns (Scheduler.go ~raise_unhandled_exn:true ())


module Spec = Command.Spec
let (+>) = Spec.(+>)

let full =
  Spec.step (fun m x -> m ~full:x)
  +> Spec.flag "full" Spec.no_arg
    ~doc:" display breakdown for good & doing counts"

let command_line () =
  Command.run (
    Command.basic (full)
      ~summary:"Jenga monitor - monitor jenga running in the current repo."
      ~readme:Mon.Progress.readme
      (fun ~full () ->
        main (if full then `full else `brief))
  )
