open Core.Std
open Async.Std
open! Int.Replace_polymorphic_compare

let lstat_counter = Metrics.Counter.create "stat"
let digest_counter = Metrics.Counter.create "digest"
let ls_counter = Metrics.Counter.create "ls"
let mkdir_counter = Metrics.Counter.create "mkdir"
let saves_done = Metrics.Counter.create "db-save"

let fs_metrics =
  Metrics.Counters.create [
    lstat_counter;
    digest_counter;
    ls_counter;
    saves_done;
  ]

let actions_run = Metrics.Counter.create "act" (* for use in action.ml *)
let saves_run = Metrics.Counter.create "save" (* for use in save_description.ml *)
let considerations_run = Metrics.Counter.create "con" (* for use in build.ml *)

let act_metrics =
  Metrics.Counters.create [
    considerations_run;
    saves_run;
    actions_run;
  ]

module Status = struct
  type t =
  | Todo
  | Built
  | Error of Reason.t list (* empty list means failure in deps *)
end

type t = {
  status : Status.t Goal.Table.t;
  mutable is_reachable_error : (Goal.t -> bool);
  job_throttle : unit Throttle.t
}

let enqueue_job t f =
  Throttle.enqueue t.job_throttle f

let all_reachable _ = true

let create config = {
  status = Goal.Table.create();
  is_reachable_error = all_reachable;
  job_throttle =
    let max_concurrent_jobs = Config.j_number config in
    Throttle.create ~continue_on_error:true ~max_concurrent_jobs ;
}

let set_status t need = function
  | None -> Hashtbl.remove t.status need
  | Some status -> Hashtbl.set t.status ~key:need ~data:status

let mask_unreachable t ~is_reachable_error = t.is_reachable_error <- is_reachable_error

let iter_masked t ~f = (* don't apply f to unreachable errors *)
  Hashtbl.iteri t.status ~f:(fun ~key ~data:status ->
    match status with
    | Status.Error _ -> if t.is_reachable_error key then f ~key ~data:status else ()
    | _ -> f ~key ~data:status
  )

module Counts = struct

  type t = {
    todo    : int;
    built   : int;
    error   : int;
    failure : int; (* error in dep *)
  } [@@deriving compare, bin_io]

  let total {todo;built;error;failure} = todo + built + error + failure

  let bad t = t.error + t.failure
  let no_errors t = bad t = 0

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
    fs_metrics : Metrics.Counters.Snap.t;
    act_metrics : Metrics.Counters.Snap.t;
  } [@@deriving bin_io, fields]

  let no_errors t = Counts.no_errors t.counts
  let built t = Counts.built t.counts
  let fraction t = Counts.fraction t.counts

  let to_effort_string t =
    Metrics.Counters.Snap.to_string t.fs_metrics

  let to_metrics t =
    Metrics.disjoint_union_exn
      (Metrics.Counters.Snap.to_metrics t.fs_metrics)
      (Metrics.Counters.Snap.to_metrics t.act_metrics)
  ;;

  let to_string t style =
    match style with
    | `omake_style ->
      let top, bot = Counts.fraction t.counts in
      sprintf "[= ] %d / %d" top bot
    | `jem_style ->
      let todo = Counts.todo t.counts in
      Finish_time_estimator.push_todo estimator todo;
      sprintf "%s j=%d+%d %s%s"
        (Counts.to_string t.counts)
        t.running t.waiting
        (Metrics.Counters.Snap.to_string ~sep:" " t.act_metrics)
        (Finish_time_estimator.estimated_finish_time_string estimator)
    | `fraction ->
      let top, bot = Counts.fraction t.counts in
      sprintf "%d / %d" top bot

  let finished t =
    if t.counts.Counts.todo <= 0
    then
      if t.counts.Counts.error > 0
      then Some `Failure
      else Some `Success
    else None
  ;;
end

let snap t = {
  Snap.
  counts = Counts.snap t;
  running = Throttle.num_jobs_running t.job_throttle;
  waiting = Throttle.num_jobs_waiting_to_start t.job_throttle;
  fs_metrics = Metrics.Counters.snap fs_metrics;
  act_metrics = Metrics.Counters.snap act_metrics;
}

let reset_metrics () = (
  Metrics.Counters.reset_to_zero fs_metrics;
  Metrics.Counters.reset_to_zero act_metrics;
)

let readme () = "
jenga monitor connects to the jenga instance running in the current
repo, (or waits until one is started), and displays progress counts:

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
