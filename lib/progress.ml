
open Core.Std
open Async.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

let persist_saves_done = Effort.Counter.create "db-save" (* for use in persist.ml *)
let actions_run = Effort.Counter.create "act" (* for use in action.ml *)
let saves_run = Effort.Counter.create "save" (* for use in save_description.ml *)
let considerations_run = Effort.Counter.create "con" (* for use in build.ml *)

module Status = struct
  type t =
  | Todo
  | Built
  | Error of Reason.t list (* empty list means failure in deps *)
  with bin_io
end

module Need = struct

  module T = struct
    type t = Jengaroot | Goal of Goal.t
    with sexp, bin_io, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make_binable(T)
  include Comparable.Make_binable(T)

  let goal x = Goal x
  let jengaroot = Jengaroot

  let to_string = function
    | Jengaroot -> "<jenga.conf>"
    | Goal x -> Goal.to_string x

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

let iter_masked t ~f = (* don't apply f to unreachable errors *)
  Hashtbl.iter t.status ~f:(fun ~key ~data:status ->
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
    con : int;
    save : int;
    act : int;
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
      sprintf "%s j=%d+%d con=%d save=%d act=%d%s"
        (Counts.to_string t.counts)
        t.running t.waiting
        t.con
        t.save
        t.act
        (Finish_time_estimator.estimated_finish_time_string estimator)

  let finished t =
    Int.(<=) t.counts.Counts.todo 0

  let error_code t =
    t.counts.Counts.error

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
    saves_run;
    actions_run;
    considerations_run;
  ]

let snap t = {
  Snap.
  counts = Counts.snap t;
  running = Throttle.num_jobs_running t.job_throttle;
  waiting = Throttle.num_jobs_waiting_to_start t.job_throttle;
  all_effort = Effort.snap all_effort;
  con = Effort.get considerations_run;
  save = Effort.get saves_run;
  act = Effort.get actions_run;
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

module Update = struct
  (* An [Update.t] expresses the change in status for some [Need.t], or the removal of any
     status information for a [Need.t]. *)
  type t = Set of Need.t * Status.t | Remove of Need.t with bin_io

  module State = struct
    (* An [Update.State.t] records the state-information, as sent to a given RPC
       client. *)
    type t = Status.t Need.Table.t
    let create () = Need.Table.create ()
  end
end

let updates t state =
  (* [update t state] computes a list of [Update.t] values (to be sent to an RPC client)
     by comparing the current state: [t.status], with the state known by the client:
     [state].  The client [state] is updated to reflect that it is now `up to date', for
     next time. *)
  let updates = Queue.create () in
  Hashtbl.merge_into ~src:t.status ~dst:state ~f:(fun ~key src dst ->
    if match dst with None -> true | Some dst -> not (phys_equal src dst) then
      Queue.enqueue updates (Update.Set (key,src));
    Some src);
  Hashtbl.iter state ~f:(fun ~key:dst ~data:_ ->
    if not (Hashtbl.mem t.status dst) then
      (Hashtbl.remove state dst;
       Queue.enqueue updates (Remove dst)));
  Queue.to_list updates

module Updates = struct type t = Update.t list with bin_io end
