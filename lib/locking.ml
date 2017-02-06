
open Core
open Async
open! Int.Replace_polymorphic_compare

let with_acquire_release : (
  acquire:(unit-> 'lock Deferred.t) ->
  release:('lock -> unit) ->
  (unit -> 'a Deferred.t) ->
  'a Deferred.t
) = fun ~acquire ~release f ->
  acquire() >>= fun lock ->
  f () >>| fun res ->
  release lock;
  res

module Target_resource : sig

  type t
  val create : unit -> t
  val is_available : t -> bool
  val acquire_all : t list -> unit Ivar.t Deferred.t

end = struct

  type t = {
    (* not determined - is locked; determined when lock released *)
    mutable released : unit Deferred.t;
  }

  let create () = { released = Deferred.unit; }
  let is_available t = Deferred.is_determined t.released
  let lock t ~finished = assert (is_available t); t.released <- finished
  let wait t = t.released

  let acquire_all ts =
    let finish = Ivar.create () in
    let finished = Ivar.read finish in
    let rec loop () =
      match
        List.rev_filter_map ts ~f:(fun t ->
          if is_available t
          then None
          else Some (wait t)
        )
      with
      | [] -> List.iter ts ~f:(lock ~finished); Deferred.return finish
      | unavailable -> Deferred.all_unit unavailable >>= loop
    in
    loop ()

end

let target_resources = Path.Rel.Table.create ()

let lock_targets_for_action ~targets f =
  (* Prevent overlapping execution of actions on each target. *)
  let rs = List.map targets ~f:(
    Hashtbl.find_or_add target_resources ~default:Target_resource.create)
  in
  let acquire () = Target_resource.acquire_all rs in
  let release x = Ivar.fill x () in
  with_acquire_release ~acquire ~release f

module Dir_resource : sig

  (* A resource which can be acquired in one of two modes.
     The same resource is sharable between multiple acquisitions in a given mode.
     A resource acquired in one mode, is not available in the other mode.

     The two modes indicate:
     [A] - An action is running which might create temporary files in the directory.
     [L] - A directory listing is being computed for the directory
  *)

  type t
  val create : unit -> t
  module Mode : sig type t = A | L end
  val acquire : t -> Mode.t -> unit Deferred.t
  val release : t-> unit
  val is_locked_for_action : t -> bool

end = struct

  module Mode = struct
    type t = A | L [@@deriving hash, compare]
    let (=) x1 x2 = 0 = compare x1 x2
  end

  type state =
  | Free
  | Locked of Mode.t * int * unit Ivar.t (* Ivar must be filled upon release *)
  type t = state ref

  let create () = ref Free

  let is_locked_for_action t = match !t with
    | Locked (Mode.A, _, _) -> true
    | Locked (Mode.L, _, _) -> false
    | Free -> false

  let obtain : (
    t -> Mode.t ->  [
    | `wait of unit Deferred.t
    | `acquired
    ]) =
    (* [obtain t mode]
       Try acquire resource for [mode]; returns either:
       - [`wait def]
            Resource is unavailable.
            When [def] is determined, [obtain] may be attempted again.
       - [`acquired]
            Resource was available and you just acquired it. *)
    fun t mode ->
      match !t with
      | Free ->
        t := Locked (mode, 1, Ivar.create ());
        `acquired
      | Locked (mode_locked, count, finish) ->
        if Mode.(=) mode_locked mode
        then
          (t := Locked (mode_locked, count + 1, finish);
           `acquired)
        else
          `wait (Ivar.read finish)

  let release t =
    match !t with
    | Free ->
      failwith "tried to release a free lock!"
    | Locked (mode, count, finish) ->
      let count = count - 1 in
      if count = 0
      then
        (t := Free; Ivar.fill finish ())
      else
        t := Locked (mode, count, finish)

  let acquire t mode =
    let rec loop () =
      match obtain t mode with
      | `wait def -> def >>= loop
      | `acquired -> Deferred.unit
    in loop ()

end

let dir_resources = Path.Table.create ()

let lock_directory ~dir mode f =
  (* Prevent overlapping activities:
     (Mode.A) - listing the files in a directory
     (Mode.L) - running an action which may create temporary files in a directory *)
  let r = Hashtbl.find_or_add dir_resources dir ~default:Dir_resource.create in
  let acquire () = Dir_resource.acquire r mode in
  let release () = Dir_resource.release r in
  with_acquire_release ~acquire ~release f

let lock_directory_for_action ~dir = lock_directory ~dir Dir_resource.Mode.A
let lock_directory_for_listing ~dir = lock_directory ~dir Dir_resource.Mode.L

let is_action_running_for_path path =
  (* Exported for use in [fs.ml], to avoid writing "changed" messages for targets which we
     know will change because we are currently running an action which target them. *)
  match Hashtbl.find target_resources path with
  | Some r -> not (Target_resource.is_available r)
  | None -> match Hashtbl.find dir_resources (Path.of_relative path) with
    | None -> false
    | Some r -> Dir_resource.is_locked_for_action r
