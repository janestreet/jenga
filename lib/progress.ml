open Core
open Async
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
    mkdir_counter;
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

(* [the_reportable_errors : Reportable.t] tracks all errors in a running jenga.  Its value
   is maintained by [set_status_*] and [clear_status].  It is exposed in the interface of
   this module to allow errors to be reported over the [Error_pipe] RPC. *)
let the_reportable_errors = Reportable.create ~name:"the_reportable_errors"

module Status_classification = struct
  type t =
    | Todo
    | Built
    | Error
    | Failure (* means the corresponding goal is not necessarily broken itself, but one of
                 its transitive dependencies is, and so we cannot build the goal *)
    | Unreachable_error_or_failure
  [@@deriving sexp_of]
end

module Status : sig

  (* [Status.t] is the state of an individual goal, held in the [Goal.Table.t] below.

     [t.reachable] indicates that the goal with this state is reachable from the root of
     the build graph. The reachability is determined periodically in build.ml *)

  type t

  val classification : t -> Status_classification.t

  val todo : t
  val built : t
  val error : Goal.t -> Reason.t list -> t

  val set_reachability : t -> bool -> unit

  (* [unreport] is to be called when the [State] of a [Goal] is replaced in the
     [Goal.Table] by [set_status] below *)
  val unreport : t -> unit

end = struct

  type e = {
    mutable reachable : bool;
    reported : Reportable.Error.t list;
  }

  type t =
  | Todo
  | Built
  | Error of e

  let classification : t -> Status_classification.t = function
    | Todo -> Todo
    | Built -> Built
    | Error e ->
      if e.reachable
      then match e.reported with
        | [] -> Failure
        | _::_ -> Error
      else Unreachable_error_or_failure

  let todo = Todo
  let built = Built

  let error goal reasons =
    let reported =
      List.map reasons ~f:(fun reason ->
        let e = Reportable.Error.create goal reason in
        Reportable.add the_reportable_errors e;
        e)
    in
    Error { reported; reachable = true; }

  let unreport_errors er =
    List.iter er.reported ~f:(fun e ->
      Reportable.remove the_reportable_errors (Reportable.Error.id e))

  let rereport_errors er =
    List.iter er.reported ~f:(fun e ->
      Reportable.add the_reportable_errors e)

  let set_reachability t new_ =
    match t with
    | Todo | Built -> ()
    | Error er ->
      let old = er.reachable in
      er.reachable <- new_;
      match old, new_ with
      | true, true  -> ()
      | true, false -> unreport_errors er
      | false,true  -> rereport_errors er
      | false,false -> ()

  let unreport t =
    match t with
    | Todo | Built -> ()
    | Error er -> unreport_errors er

end

(* There will only ever be one [t] created *)
type t = {
  table : Status.t Goal.Table.t;
  job_throttle : unit Throttle.t;
}

let create config = {
  table = Goal.Table.create();
  job_throttle = (
    let max_concurrent_jobs = Config.j_number config in
    Throttle.create ~continue_on_error:true ~max_concurrent_jobs );
}

let enqueue_job t f =
  Throttle.enqueue t.job_throttle f

let unreport t goal =
  Option.iter (Hashtbl.find t.table goal) ~f:(fun v -> Status.unreport v)

let set_status t goal status =
  unreport t goal;
  Hashtbl.set t.table ~key:goal ~data:status

let set_status_todo t goal = set_status t goal Status.todo
let set_status_built t goal = set_status t goal Status.built
let set_status_error t goal reasons = set_status t goal (Status.error goal reasons)

let clear_status t goal =
  unreport t goal;
  Hashtbl.remove t.table goal

let mask_unreachable t ~is_reachable_error =
  Hashtbl.iteri t.table ~f:(fun ~key ~data ->
    Status.set_reachability data (is_reachable_error key))

let iter_classification t ~f =
  Hashtbl.iter t.table ~f:(fun state ->
    f (Status.classification state)
  )

module Counts = struct

  module Stable = struct
    open Core.Core_stable

    module V1 = struct
      type t = {
        todo    : int;
        built   : int;
        error   : int;
        failure : int; (* error in dep *)
      } [@@deriving hash, compare, bin_io]

    end
    let%expect_test _ =
      print_endline [%bin_digest: V1.t];
      [%expect {| 9e44ed1db4e5565de4edd72452409d8b |} ]
  end
  include Stable.V1

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
    iter_classification t ~f:(function
      | Todo                         -> incr todo
      | Built                        -> incr built
      | Failure                      -> incr failure
      | Error                        -> incr error
      | Unreachable_error_or_failure -> () (* not counted *)
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

  module Stable = struct
    open Core.Core_stable
    module V1 = struct
      type t = {
        counts : Counts.Stable.V1.t;
        running : int;
        waiting : int;
        fs_metrics : Metrics.Counters.Snap.Stable.V1.t;
        act_metrics : Metrics.Counters.Snap.Stable.V1.t;
      } [@@deriving bin_io, fields]
    end
    let%expect_test _ =
      print_endline [%bin_digest: V1.t];
      [%expect {| 0d0d8424b08b40a9f568b17042d17d0b |} ]
  end

  include Stable.V1

  let no_errors t = Counts.no_errors t.counts
  let built t = Counts.built t.counts
  let fraction t = Counts.fraction t.counts
  let todo t = Counts.todo t.counts

  let to_act_string t =
    Metrics.Counters.Snap.to_string t.act_metrics

  let to_effort_string t =
    Metrics.Counters.Snap.to_string t.fs_metrics

  let to_metrics t =
    Metrics.disjoint_union_exn
      (Metrics.Counters.Snap.to_metrics t.fs_metrics)
      (Metrics.Counters.Snap.to_metrics t.act_metrics)
  ;;

  let to_string t style =
    match style with
    | `monitor_style ->
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

The line is optionally suffixed by an estimated finish time if the
build is still proceeding, or an indication that jenga is finished
and polling for file-system changes. Note: \"finished\" does not
necessary mean that the build was successful. Trailing \"?\"s
indicate the monitor has not heard from jenga for more than half a
second."
