open Core
open Async
open! Int.Replace_polymorphic_compare

module Error = struct

  module Stable = struct
    open! Core.Core_stable

    module V2 = struct

      (** An error when running an external command *)
      type command_failed = {
        job_summary : Job_summary.Stable.V2.t;
      } [@@deriving bin_io, sexp_of]

      (** Any other error *)
      type internal = {
        lines : string list;
      } [@@deriving bin_io, sexp_of]

      type kind =
        | Command_failed of command_failed
        | Internal of internal
      [@@deriving bin_io, sexp_of]

      module Id = Unique_id.Int()

      type t = {
        (* We cant use [Goal.t] here because it contains a [Db] type (Path.t) *)
        goal_string : string;
        dir_string : string;
        (* Instead of [kind] we would have prefered to use [Reason.t], but again this makes
           use of [Db] types such as path. *)
        kind : kind;
        id : Id.t;
      } [@@deriving bin_io, sexp_of]

    end

    module V1 = struct

      (** An error when running an external command *)
      type command_failed = {
        job_summary : Job_summary.Stable.V1.t;
      } [@@deriving bin_io]

      (** Any other error *)
      type internal = V2.internal
      [@@deriving bin_io]

      type kind =
        | Command_failed of command_failed
        | Internal of internal
      [@@deriving bin_io]

      module Id = V2.Id

      type t = {
        (* We cant use [Goal.t] here because it contains a [Db] type (Path.t) *)
        goal_string : string;
        dir_string : string;
        (* Instead of [kind] we would have prefered to use [Reason.t], but again this makes
           use of [Db] types such as path. *)
        kind : kind;
        id : Id.t;
      } [@@deriving bin_io]

      let upgrade_kind : kind -> V2.kind = function
        | Command_failed { job_summary } ->
          let job_summary = Job_summary.Stable.V1.upgrade job_summary in
          Command_failed { job_summary }
        | Internal i -> Internal i

      let upgrade { goal_string; dir_string; kind; id; } =
        let kind = upgrade_kind kind in
        { V2.goal_string; dir_string; kind; id }
    end

    let%expect_test _ =
      print_endline [%bin_digest: V1.t];
      [%expect {| e4eff16a7561fba7224c6a1af6a51b54 |} ]

    let%expect_test _ =
      print_endline [%bin_digest: V2.t];
      [%expect {| 0b55e9a45719d49f6bae9358b13869b6 |} ]

  end

  include Stable.V2

  let id t = t.id

  let goal_string t = t.goal_string

  let directory t = Path.Rel.create t.dir_string

  let create0 goal kind =
    let goal_string = Goal.to_string goal in
    let dir_string = Path.Rel.to_string (Goal.directory goal) in
    { goal_string; dir_string; kind; id = Id.create(); }

  let command_failed goal job_summary =
    create0 goal (Command_failed { job_summary })

  let internal goal ~lines  =
    create0 goal (Internal { lines })

  let create goal reason =
    match reason with
    | Reason.Command_failed job_summary -> command_failed goal job_summary
    | r -> internal goal ~lines:(Reason.to_string_one_line r
                                 :: Reason.to_extra_lines r ~dir:(Goal.directory goal))

  let contents t =
    match t.kind with
    | Internal x -> `Internal x.lines
    | Command_failed x -> `Command_failed x.job_summary

end

module Stable = struct
  module E = Error
  open Core.Core_stable
  module Error = E.Stable

  module V2 = struct

    module Update = struct
      type t =
        | Add of Error.V2.t
        | Remove of Error.V2.Id.t
      [@@deriving bin_io]
    end

    module Snap = struct
      type t = {
        data : Error.V2.t list;
      } [@@deriving bin_io]
    end

  end

  module V1 = struct

    module Update = struct
      type t =
        | Add of Error.V1.t
        | Remove of Error.V1.Id.t
      [@@deriving bin_io]

      let upgrade : t -> V2.Update.t = function
        | Add err ->
          let err = Error.V1.upgrade err in
          Add err
        | Remove id -> Remove id
    end

    module Snap = struct
      type t = {
        data : Error.V1.t list;
      } [@@deriving bin_io]

      let upgrade { data } =
        let data = List.map ~f:Error.V1.upgrade data in
        { V2.Snap.data }
    end

  end

  let%expect_test _ =
    print_endline [%bin_digest: V1.Update.t];
    [%expect {| bce425a7498eb811cee88a76ed7d3755 |} ]

  let%expect_test _ =
    print_endline [%bin_digest: V1.Snap.t];
    [%expect {| 23db845eef46b5f2d2e5950526c94972 |} ]

  let%expect_test _ =
    print_endline [%bin_digest: V2.Update.t];
    [%expect {| e4d60a5018bc6e458a0507f33abda300 |} ]

  let%expect_test _ =
    print_endline [%bin_digest: V2.Snap.t];
    [%expect {| dfb9d9b77beb54f89a50611844961e98 |} ]

end

include Stable.V2

(* Each [Error.t] has a unique handle. This is used to key the hashtable representing an
   error-bag.  We dont use [Bag.t] because updates must be communicated via the RPC. *)
module T : sig

  type t
  [@@deriving sexp_of]

  val create : name:string -> t
  val of_snap : name:string -> Snap.t -> t

  (* The table returned by [read_only] should NOT be modified.  [read_only] isn't exposed
     from this module, so it's easy to check all callers satisfy this constraint. *)
  val read_only : t -> Error.t Error.Id.Table.t

  val update : t -> Update.t -> unit

  val register_for_updates :
    trace : (string -> unit) ->
    t -> Update.t Pipe.Writer.t -> unit

  val drop_clients : t -> unit

end = struct

  type t = {
    name : string; (* for client registration trace *)
    table : Error.t Error.Id.Table.t;
    clients : Update.t Pipe.Writer.t Bag.t;
  }
  let sexp_of_t t = [%sexp_of: Error.t Error.Id.Table.t] t.table

  let of_list ~name errors = {
    name;
    table = Error.Id.Table.of_alist_exn (List.map errors ~f:(fun e -> Error.id e, e));
    clients = Bag.create ();
  }

  let create ~name = of_list ~name []

  let of_snap ~name snap = of_list ~name snap.Snap.data

  let read_only t = t.table

  let update t (update : Update.t) =
    Bag.iter t.clients ~f:(fun pipe -> Pipe.write_without_pushback_if_open pipe update);
    match update with
    | Add error -> Hashtbl.add_exn t.table ~key:(Error.id error) ~data:error
    | Remove id -> Hashtbl.remove t.table id

  let register_for_updates ~trace t writer =
    let client = Bag.add t.clients writer in
    let mes s = trace (sprintf "%s: %s (#clients=%d)" s t.name (Bag.length t.clients)) in
    mes "client registered";
    don't_wait_for (
      Pipe.closed writer >>| fun () ->
      Bag.remove t.clients client;
      mes "client unregistered"
    )

  let drop_clients t =
    Bag.iter t.clients ~f:Pipe.close

end

type t = T.t
[@@deriving sexp_of]

let of_snap = T.of_snap
let update = T.update

let create ~name = T.create ~name

let add t error = T.update t (Update.Add error)

let remove t id = T.update t (Update.Remove id)

let length t = Hashtbl.length (T.read_only t)

let iter t ~f = Hashtbl.iteri (T.read_only t) ~f:(fun ~key:_ ~data -> f data)

let to_list t = Hashtbl.data (T.read_only t)

let do_snap t = { Snap.data = Hashtbl.data (T.read_only t); }

let drop_clients = T.drop_clients

let snap_with_updates ~trace t =
  let snap = do_snap t in
  let reader,writer = Pipe.create () in
  T.register_for_updates ~trace t writer;
  snap,reader
