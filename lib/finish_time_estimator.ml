
open Core
open! Int.Replace_polymorphic_compare

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
  if Float.(seconds <= 0.0)
  then
    (* This formula is the limit of the [else] branch as [seconds] approaches zero.
       It serves to avoid [nan] rate for 0.0 [seconds]. *)
    t.rate -. float progress *. log (t.decay_factor_per_second)
  else
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
  let ofday = Time.to_ofday ~zone:(force Time.Zone.local) time in
  let span = Time.Ofday.to_span_since_start_of_day ofday in
  let parts = Time.Span.to_parts span in
  let module P = Time.Span.Parts in
  sprintf "%02d:%02d" parts.P.hr parts.P.min

let estimated_finish_time_string t =
  if t.last_todo <= 0 then ", finished"
  else
      (* will never finish with a negative or zero rate *)
    if Float.(t.rate <= 0.0) then ""
    else
      let duration = Time.Span.scale (sec 1.0) (float t.last_todo /. t.rate) in
        (* pointless to show a duration which is silly big *)
      if Time.Span.(duration > Time.Span.of_hr 12.) then ""
      else
        ", finish at: " ^ to_min_string_no_date (Time.add (Time.now()) duration)
