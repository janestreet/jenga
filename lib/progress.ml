
open Core.Std
open Async.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Description

let persist_saves_done = Effort.Counter.create "db-save" (* for use in persist.ml *)
let actions_run = Effort.Counter.create "act" (* for use in build.ml *)

module Status = struct
  type t =
  | Todo
  | Built
  | Error of Reason.t list (* empty list means failure in deps *)
end

type t = {
  status : Status.t Need.Table.t;
  mutable is_reachable_error : (Need.t -> bool);
  job_throttle : unit Throttle.t
}

let enqueue_job t f =
  Throttle.enqueue t.job_throttle f

let all_reachable _ = true

let create config = {
  status = Need.Table.create();
  is_reachable_error = all_reachable;
  job_throttle =
    let max_concurrent_jobs = Config.j_number config in
    Throttle.create ~continue_on_error:true ~max_concurrent_jobs ;
}

let set_status t need status =
  Hashtbl.set t.status ~key:need ~data:status

let mask_unreachable t ~is_reachable_error = t.is_reachable_error <- is_reachable_error
let unmask_everything t = t.is_reachable_error <- all_reachable

let iter_masked t ~f = (* dont apply f to unreachable errors *)
  Hashtbl.iter t.status ~f:(fun ~key ~data:status ->
    match status with
    | Status.Error _ -> if t.is_reachable_error key then f ~key ~data:status else ()
    | _ -> f ~key ~data:status
  )

let message_errors config t =
  iter_masked t ~f:(fun ~key:need ~data:status ->
    match status with
    | Status.Error reasons ->
      List.iter reasons ~f:(fun reason ->
        match reason with
        | Reason.Shutdown -> () (* suppress these *)
        | _ -> Reason.message_summary config need reason
      )
    | _ -> ()
  )

module Counts = struct

  type t = {
    todo    : int;
    built   : int;
    error   : int;
    failure : int; (* error in dep *)
  } with compare, bin_io

  let total {todo;built;error;failure} = todo + built + error + failure

  let bad t = t.error + t.failure
  let no_errors t = Int.(bad t = 0)

  let to_string t =
    let err = if no_errors t then "" else sprintf " !%d ~%d" t.error t.failure in
    sprintf "todo: %d (%d / %d)%s" t.todo t.built (total t) err

  let todo t = t.todo
  let built t = t.built
  (*let fraction t = (built t) , (built t) + (bad t)*)
  let fraction t = (built t) , (total t)

  let snap t =
    let todo = ref 0 in
    let built = ref 0 in
    let error = ref 0 in
    let failure = ref 0 in
    iter_masked t
      ~f:(fun ~key:_ ~data:status ->
        let x =
          match status with
          | Status.Todo       -> todo
          | Status.Built      -> built
          | Status.Error []   -> failure (* error in deps *)
          | Status.Error _    -> error
        in incr x
      );
    {
     todo     = !todo;
     built    = !built;
     error    = !error;
     failure  = !failure;
    }

end

let estimator =
  (* The bigger the decay-factor, the more stable the estimate *)
  Finish_time_estimator.create ~decay_factor_per_second:0.95

module Snap = struct

  type t = {
    counts : Counts.t;
    running : int;
    waiting : int;
    all_effort : Effort.Counts.t;
    act_effort : Effort.Counts.t;
  } with bin_io, fields

  let no_errors t = Counts.no_errors t.counts
  let built t = Counts.built t.counts
  let fraction t = Counts.fraction t.counts

  let to_effort_string t =
    Effort.Counts.to_string t.all_effort

  let to_string t style =
    match style with
    | `omake_style ->
      let fraction = Counts.fraction t.counts in
      let top,bot = fraction in
      sprintf "[= ] %d / %d" top bot
    | `jem_style ->
      let todo = Counts.todo t.counts in
      Finish_time_estimator.push_todo estimator todo;
      sprintf "%s j=%d+%d %s%s"
        (Counts.to_string t.counts)
        t.running t.waiting
        (Effort.Counts.to_string t.act_effort)
        (Finish_time_estimator.estimated_finish_time_string estimator)

end

let all_effort =
  Effort.create [
    Fs.lstat_counter;
    Fs.digest_counter;
    (*Fs.mkdir_counter;*)
    Fs.ls_counter;
    (*Job.external_jobs_run;*)
    persist_saves_done;
  ]

let act_effort =
  Effort.create [
    actions_run
  ]

let snap t = {
  Snap.
  counts = Counts.snap t;
  running = Throttle.num_jobs_running t.job_throttle;
  waiting = Throttle.num_jobs_waiting_to_start t.job_throttle;
  all_effort = Effort.snap all_effort;
  act_effort = Effort.snap act_effort;
}

let reset_effort () = (
  Effort.reset_to_zero all_effort;
  Effort.reset_to_zero act_effort;
)

let readme () = "
Jem connects to the jenga instance running in the current repo,
(or waits until one is started), and displays progress counts:

    todo (built / total) !error ~failure

(The error/failure counts are shown only when non-zero)
It can NEVER happen that error=0 but failure>0.

- todo    : target being checked
- build   : target is built and up to date
- error   : leaf build error; i.e. non-zero exit or target not created
- failure : unable to build because of errors in dependencies

- total = todo + built + error + failure
- (built / total) is the omake style fraction

The line is optionally suffixed by an estimated finish time if the
build is still proceeding, or an indication that jenga is finished and
polling for file-system changes. Note: \"finished\" does not necessary
mean that the build was successful. Trailing \"?\"s indicate jem has not
heard from jenga for more than half a second."

