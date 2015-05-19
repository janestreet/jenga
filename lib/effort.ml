
open Core.Std
open! No_polymorphic_compare

module Counter = struct

  type t = {
    name : string;
    start : int ref;
  }

  let create name = {
    name;
    start = ref 0;
  }

  let reset_to_zero t = (
    t.start := 0;
  )

  module Counts = struct

    type t = {
      name : string;
      start : int;
    } with bin_io

    let to_string t =
      String.concat [
        sprintf "%s=%d" t.name t.start;
      ]

  end

  let snap t = {
    Counts.
    name = t.name;
    start = ! (t.start);
  }

end

let track counter f =
  incr counter.Counter.start;
  f()

let incr counter =
  incr counter.Counter.start

let get counter = ! (counter.Counter.start)

type t = {
  counters : Counter.t list;
}
type counter_set = t

let create counters = { counters; }

let reset_to_zero t =
  List.iter t.counters ~f:Counter.reset_to_zero

module Counts = struct

  type t = {
    counts : Counter.Counts.t list;
  } with bin_io

  let to_string ?limit t =
    let restricted =
      match limit with None -> t.counts
      | Some limit ->
        let set =
          String.Hash_set.of_list (List.map limit.counters ~f:(fun x -> x.Counter.name))
        in
        List.filter t.counts ~f:(fun x -> Hash_set.mem set x.Counter.Counts.name)
    in
    String.concat ~sep:", " (List.map restricted ~f:Counter.Counts.to_string)

end

let snap t = {
  Counts.
  counts = List.map t.counters ~f:Counter.snap;
}
