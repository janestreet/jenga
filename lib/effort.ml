
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let (=) = Int.(=)

module Counter = struct

  type t = {
    name : string;
    start : int ref;
    done_normal : int ref;
    done_exn : int ref;
    done_monitor_exn : int ref;
  }

  let create name = {
    name;
    start = ref 0;
    done_normal = ref 0;
    done_exn = ref 0;
    done_monitor_exn = ref 0;
  }

  let reset_to_zero t = (
    t.start := 0;
    t.done_normal := 0;
    t.done_exn := 0;
    t.done_monitor_exn := 0;
  )

  module Snapped = struct

    type t = {
      name : string;
      start : int;
      done_normal : int;
      done_exn : int;
      done_monitor_exn : int;
    }

    let to_string t =
      let finished = t.done_normal + t.done_exn + t.done_monitor_exn in
      let running = t.start - finished in
      String.concat [
        sprintf "%s=%d" t.name t.start;
        (if t.done_exn=0 then "" else sprintf "(!!%d)" t.done_exn);
        (if t.done_monitor_exn=0 then "" else sprintf "(!%d)" t.done_monitor_exn);
        (if running=0 then "" else sprintf "(@%d)" running);
      ]

  end

  let snap t = {
    Snapped.
    name = t.name;
    start = ! (t.start);
    done_normal = ! (t.done_normal);
    done_exn = ! (t.done_exn);
    done_monitor_exn = ! (t.done_monitor_exn);
  }

end

let track counter f =
  incr counter.Counter.start;
  try ( (* not necessary surely... *)
    Monitor.try_with f >>= function
    | Ok res ->
      incr counter.Counter.done_normal;
      Deferred.return res
    | Error exn ->
      incr counter.Counter.done_monitor_exn;
      raise exn
  )
  with
  | exn ->
    (* ...and so never get here *)
    incr counter.Counter.done_exn;
    raise exn


type t = {
  tag : string;
  counters : Counter.t list;
}

let create ~tag counters = { tag; counters; }

let reset_to_zero t =
  List.iter t.counters ~f:Counter.reset_to_zero

module Snapped = struct

  type t = {
    tag : string;
    snapped : Counter.Snapped.t list;
  }

  let to_string t =
    sprintf "%s=[%s]" t.tag
      (String.concat ~sep:", " (List.map t.snapped ~f:Counter.Snapped.to_string))

end

let snap t = {
  Snapped.
  tag = t.tag;
  snapped = List.map t.counters ~f:Counter.snap;
}
