open Core
open! Int.Replace_polymorphic_compare
open Async
module Heart = Tenacious.Heart

module Problem = struct
  module Id = Unique_id.Int()

  (**
    [reasons] contains all errors collected transitively together with
    dependency paths via which they've been reached;

     [reasons_here] contains only the errors at this subgoal.
  *)
  type t = {
    reasons : (Reason.t * Goal.t list) Id.Map.t
  ; reasons_here : Reason.t Id.Map.t
  ; needs_in_error : Goal.Set.t
  }

  let union_left_biased = Map.merge ~f:(fun ~key:_ -> function
    | `Left l -> Some l
    | `Both (l, _) -> Some l
    | `Right r -> Some r
  )

  let reasons t = Map.data (t.reasons)
  let reasons_here t = Map.data (t.reasons_here)
  let needs_in_error t = t.needs_in_error

  let create r =
    let id = Id.create () in
    { reasons = Id.Map.singleton id (r, [])
    ; reasons_here = Id.Map.singleton id r
    ; needs_in_error = Goal.Set.empty
    }

  let merge a b =
    { reasons = union_left_biased a.reasons b.reasons
    ; reasons_here =
        union_left_biased
          a.reasons_here
          b.reasons_here
    ; needs_in_error =
        Set.union a.needs_in_error b.needs_in_error
    }

  let empty =
    { reasons = Id.Map.empty
    ; reasons_here = Id.Map.empty
    ; needs_in_error = Goal.Set.empty
    }

  let all = List.fold_left ~init:empty ~f:merge

  let subgoal need t =
    { reasons = Id.Map.map ~f:(fun (r, l) -> (r, need :: l)) t.reasons
    ; reasons_here = Id.Map.empty
    ; needs_in_error = Set.add t.needs_in_error need
    }
end

type 'a t = ('a, Problem.t) Tenacious.Result.t

let cutoff ~equal x =
  Tenacious.cutoff
    ~equal:(fun res1 res2 ->
      match res1,res2 with
      | Ok x1, Ok x2 -> equal x1 x2
      (* never cutoff errors *)
      | Ok _, Error _
      | Error _, Ok _
      | Error _, Error _
        -> false
    ) x

let wrap t = t
let expose t = t

let memoize = Tenacious.memoize

let of_tenacious tenacious =
  Tenacious.map tenacious ~f:(fun x -> Ok x)

let return = Tenacious.Result.return
let bind = Tenacious.Result.bind
let map x ~f = Tenacious.Result.map x ~f

(* we can't use Tenacious.Result.all because:
   - it's sequential, but we want parallel
   - it only returns one error, but we want all of them *)
let all xs =
  Tenacious.map (Tenacious.all xs) ~f:(fun ys ->
    let rec collect probs oks = function
      | Ok x :: xs -> collect probs (x::oks) xs
      | Error p :: xs -> collect (p::probs) oks xs
      | [] ->
        match probs with
        | [] -> Ok (List.rev oks)
        | _::_ -> Error (Problem.all (List.rev probs))
    in
    collect [] [] ys
  )

let all_unit ts = map (all ts) ~f:(fun (_ : unit list) -> ())

let error reason = Tenacious.Result.fail (Problem.create reason)

let subgoal need builder =
  Tenacious.Result.map_error builder ~f:(Problem.subgoal need)

let of_deferred f =
  Tenacious.lift (fun () ->
    f () >>| fun x -> (Ok x, Heart.unbreakable)
  )

let desensitize t =
  Tenacious.bind (Tenacious.desensitize t) ~f:(function
  | (Ok x,heart) -> Tenacious.return (Ok (x,heart))
  | (Error e, heart) ->
    Tenacious.lift (fun () ->
      Deferred.return (Error e, heart)
    )
  )

let sensitize heart =
  Tenacious.lift (fun () -> Deferred.return (Ok (), heart))

let bracket t ~running ~finished ~cancelled =
  Tenacious.bracket t ~running ~finished ~cancelled

let uncancellable builder = Tenacious.uncancellable builder

let ( *>>| ) x f = map x ~f
let both : ('a t -> 'b t -> ('a * 'b) t) =
  fun a b ->
    all [
      (a *>>| fun a -> `a a);
      (b *>>| fun b -> `b b);
    ] *>>| function
    | [`a a; `b b] -> (a,b)
    | _ -> assert false

let return_result v =
  Tenacious.return
    (match v with
     | Error reason -> Error (Problem.create reason)
     | Ok _ as ok -> ok)

let bind_result t f = Tenacious.map t ~f:(function
  | Error _ as e -> e
  | Ok v ->
    match f v with
    | Ok _ as ok -> ok
    | Error reason -> Error (Problem.create reason))

