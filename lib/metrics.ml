
open Core
open! Int.Replace_polymorphic_compare

module Unit = struct
  type t =
    | Byte
    | Second
    | Dimensionless
  [@@deriving compare, sexp]
end

type t = (float * Unit.t) String.Map.t [@@deriving sexp]
type metrics = t [@@deriving sexp]

let disjoint_union_exn map1 map2 =
  Map.merge map1 map2
    ~f:(fun ~key v ->
      match v with
      | `Left x | `Right x -> Some x
      | `Both _ -> failwithf "didn't expect metrics %s to be in both map" key ())
;;

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

  module Snap = struct

    module Stable = struct
      open Core.Core_stable

      module V1 = struct
        type t = {
          name : string;
          start : int;
        } [@@deriving bin_io]
      end

      let%expect_test _ =
        print_endline [%bin_digest: V1.t];
        [%expect {| 9698f151fa21e5ebc4daca817fd5e0a1 |} ]
    end

    include Stable.V1

    let to_string t = sprintf "%s=%d" t.name t.start

  end

  let snap t = {
    Snap.
    name = t.name;
    start = ! (t.start);
  }

  let incr t =
    incr t.start

  let get t = ! (t.start)

end

module Counters = struct
  type t = {
    counters : Counter.t list;
  }

  let create counters = { counters; }

  let reset_to_zero t =
    List.iter t.counters ~f:Counter.reset_to_zero

  module Snap = struct

    module Stable = struct
      open Core.Core_stable

      module V1 = struct
        type t = {
          counts : Counter.Snap.Stable.V1.t list;
        } [@@deriving bin_io]
      end

      let%expect_test _ =
        print_endline [%bin_digest: V1.t];
        [%expect {| fad3a0cc0c47d30c28221bb0c309e867 |} ]
    end

    include Stable.V1

    let to_string ?(sep = ", ") t =
      String.concat ~sep (List.map t.counts ~f:Counter.Snap.to_string)

    let to_metrics t =
      String.Map.of_alist_exn
        (List.map t.counts ~f:(fun c ->
           c.name, (Float.of_int c.start, Unit.Dimensionless)))
    ;;
  end

  let snap t = {
    Snap.
    counts = List.map t.counters ~f:Counter.snap;
  }
end

let stat_since_last_checked get_current zero (-) =
  let last = ref zero in
  fun stat ->
    let current = get_current stat in
    let diff = current - !last in
    last := current;
    diff
;;

let stat_since_last_checked_int get_current =
  stat_since_last_checked get_current 0 (-)
let stat_since_last_checked_float get_current =
  stat_since_last_checked get_current 0. (-.)
;;

module Memory = struct
  type t =
    { top_heap : Byte_units.t
    ; minor : Byte_units.t
    ; major : Byte_units.t
    ; promoted : Byte_units.t
    ; major_collections : int
    ; heap : Byte_units.t
    } [@@deriving sexp, fields]

  let create_diff_from_previous_create =
    let float_words x = Byte_units.create `Words x in
    let int_words x = float_words (Float.of_int x) in
    let major_collections = stat_since_last_checked_int Gc.Stat.major_collections in
    let minor_words = stat_since_last_checked_float Gc.Stat.minor_words in
    let major_words = stat_since_last_checked_float Gc.Stat.major_words in
    let promoted_words = stat_since_last_checked_float Gc.Stat.promoted_words in
    fun () ->
      let stat = Gc.quick_stat () in
      let heap = int_words stat.heap_words in
      let minor = float_words (minor_words stat) in
      let major = float_words (major_words stat) in
      let promoted = float_words (promoted_words stat) in
      let major_collections = major_collections stat in
      let top_heap = int_words stat.top_heap_words in
      { top_heap; minor; major; promoted; major_collections; heap }
  ;;

  let to_metrics =
    let int acc field _t value =
      Map.set acc ~key:(Fieldslib.Field.name field)
        ~data:(Float.of_int value, Unit.Dimensionless) in
    let byte_units acc field _t value =
      Map.set acc ~key:(Fieldslib.Field.name field)
        ~data:(Byte_units.bytes value, Unit.Byte)
    in
    fun t ->
      Fields.Direct.fold t ~init:String.Map.empty
        ~top_heap:byte_units
        ~minor:byte_units
        ~major:byte_units
        ~promoted:byte_units
        ~major_collections:int
        ~heap:byte_units
  ;;
end

module System_resources = struct
  type t =
    { user_time : float
    ; system_time : float
    }
  let create_diff_from_previous_create =
    let user_time = stat_since_last_checked_float Unix.Resource_usage.utime in
    let system_time = stat_since_last_checked_float Unix.Resource_usage.stime in
    fun () ->
      let resource_usage = Unix.Resource_usage.get `Self in
      { user_time = user_time resource_usage
      ; system_time = system_time resource_usage
      }
  let to_metrics { user_time; system_time } =
    String.Map.of_alist_exn
      [ "jenga-user-time", (user_time, (Second : Unit.t))
      ; "jenga-system-time", (system_time, Second)
      ]
end

module Disk_format = struct
  open Async

  type t =
    { build_info : Sexp.t
    ; version_util : string list
    ; metrics : metrics
    }
  [@@deriving sexp]

  let append metrics =
    let t =
      { build_info = Version_util.build_info_as_sexp
      ; version_util = Version_util.version_list
      ; metrics
      }
    in
    Writer.with_file ~append:true (Path.Rel.to_string Special_paths.Dot_jenga.metrics)
      ~f:(fun w ->
        Writer.write_sexp w ~terminate_with:Newline
          [%sexp (t : t)];
        return ())
  ;;
end
