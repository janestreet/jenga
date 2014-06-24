
open Core.Std
open Async.Std

(*TODO:
  optimize never/always
  TEST

  specialization for with_semantics:
  (or code with_semantics, non-primitively using exec/lift)
  - before_redo
  - acquire/release
  - desensitize
  - uncancellable
*)

module Deferred = struct
  include Deferred
  (* todo: better to use something else in deferred? *)
  let either : (
    'a Deferred.t -> 'b Deferred.t -> [`A of 'a | `B of 'b] Deferred.t
  ) = fun adef bdef ->
    choose [
      choice adef (fun x -> `A x);
      choice bdef (fun x -> `B x);
    ]
end

module Glass : sig
  type t = unit Ivar.t
  val create : unit -> t
  val is_broken : t -> bool
  val break : t -> unit
end = struct
  type t = unit Ivar.t
  let create () = Ivar.create ()
  let break t = Ivar.fill t ()
  let is_broken t = Deferred.is_determined (Ivar.read t)
end

module Heart : sig
  type t = unit Deferred.t
  val when_broken : t -> unit Deferred.t
  val of_glass : Glass.t -> t
  val unbreakable : t
end = struct
  type t = unit Deferred.t
  let of_glass t = Ivar.read t
  let unbreakable = let h = Ivar.read (Glass.create ()) in h
  let when_broken t = t
end

type 'a result = ('a * Heart.t) option Deferred.t
type 'a semantics = cancel:Heart.t -> 'a result

module Supply : sig

  type 'a t

  val deferred    : 'a t Deferred.t -> 'a t
  val cons        : 'a -> Heart.t -> 'a t Deferred.t -> 'a t

  val wait        : 'a t -> 'a Deferred.t
  val upon_revoke : 'a t -> ('a t -> unit) -> unit

  val certify     : 'a t -> ('a * Heart.t) t
  val cutoff      : equal:('a -> 'a -> bool) -> 'a t -> 'a t

  val return      : 'a -> 'a t
  val bind        : 'a t -> ('a -> 'b t) -> 'b t
  val map         : 'a t -> f:('a -> 'b) -> 'b t
  val both        : 'a t -> 'b t -> ('a * 'b) t
  val all         : 'a t list -> 'a list t

  val with_semantics : 'a t -> f:('a semantics -> 'b semantics) -> 'b t

end = struct

  type 'a supply = {
    value: 'a;
    revoke : 'a t Deferred.t;
  }
  and 'a t = 'a supply Deferred.t

  let rec with_semantics t ~f =

    t >>= fun supply ->

    let b_semantics : 'b semantics =
      let a_semantics : 'a semantics =
        fun ~cancel ->
          let x = supply.value in
          return (Some (x,cancel))
      in
      f a_semantics
    in

    let b_result =
      let cancel = (supply.revoke >>| fun __t -> ()) in
      b_semantics ~cancel
    in

    b_result >>| function
    | None -> failwith "with_semantics/None"
    | Some (x,revoke) ->

    {
      value = x;
      revoke = (
        Heart.when_broken revoke >>= fun () ->
        supply.revoke >>| fun t -> (* hang? *)
        with_semantics t ~f
      )
    }

  let deferred def =
    def >>= fun x -> x

  let cons x heart def =
    return {
      value = x;
      revoke = (
        Heart.when_broken heart >>= fun () ->
        def
      )
    }

  let wait t =
    t >>| fun supply -> supply.value

  let upon_revoke t cb =
    t >>> fun s ->
    s.revoke >>> fun t' -> cb t'

  let rec certify t =
    t >>| fun s ->
    let glass = Glass.create () in
    let heart = Heart.of_glass glass in
    {
      value = (s.value, heart);
      revoke = (
        s.revoke >>| fun t ->
        Glass.break glass;
        certify t
      )
    }

  let cutoff ~equal =
    let rec loop s0 =
      s0.revoke >>= fun t ->
      t >>= fun s ->
      if equal s0.value s.value
      then loop s
      else
        return (
          return {
            value = s.value;
            revoke = loop s
          }
        )
    in
    fun t ->
      t >>| fun s -> {
        value = s.value;
        revoke = loop s
      }

  let rec bind t1 f =
    t1 >>= fun s1 ->
    let rec inner t2 =
      t2 >>= fun s2 ->
      return {
        value = s2.value;
        revoke =
          Deferred.either s1.revoke s2.revoke >>| function
          | `A t1 -> bind t1 f
          | `B t2 -> inner t2
      }
    in
    inner (f s1.value)

  let rec map t ~f =
    t >>| fun s -> {
      value = f s.value;
      revoke = s.revoke >>| fun t -> map t ~f
    }

  let both =
    let rec stepTT t1 t2 =
      Deferred.either t1 t2 >>= function
      | `A s1 -> stepST s1 t2
      | `B s2 -> stepTS t1 s2

    and stepST s1 t2 =
      Deferred.either s1.revoke t2 >>= function
      | `A t1 -> stepTT t1 t2
      | `B s2 -> stepSS s1 s2

    and stepTS t1 s2 =
      Deferred.either t1 s2.revoke >>= function
      | `A s1 -> stepSS s1 s2
      | `B t2 -> stepTT t1 t2

    and stepSS s1 s2 = return {
      value = (s1.value, s2.value);
      revoke =
        Deferred.either s1.revoke s2.revoke >>| function
        | `A t1 -> stepTS t1 s2
        | `B t2 -> stepST s1 t2
    }
    in stepTT


  let return x =
    return {
      value = x;
      revoke = Deferred.never(); (* todo: help to know this statically? *)
    }

  let all =
    (* todo, optimize - define as primitive *)
    let rec loop acc = function
      | [] -> map acc ~f:List.rev
      | t::ts -> loop (map (both t acc) ~f:(fun (x,xs) -> x::xs)) ts
    in
    fun ts -> loop (return []) ts

end

module Demand : sig

  type t

  (*val never : t (* todo: need to expose?? *)*)
  val always : t
  val until : t -> Heart.t -> t

  val supply_with : t -> (unit -> ('a * Heart.t) Deferred.t) -> 'a Supply.t

  module Feed : sig type t end
  val feed_into : unit -> Feed.t * t
  val connect_feed : t -> Feed.t -> unit

end = struct

  (* todo: improve rep style to be like supply *)
  type t = { demand : [`giveup of t Deferred.t] Deferred.t }

  (* it is necessary to optimize never/always to be statically visible *)

  let never = {
    demand = Deferred.never()
  }

  let always = {
    demand = return (`giveup (Deferred.never()))
  }

  let deferred def = {
    demand =
      def >>= fun s -> s.demand
  }

  let until init_t heart =
    let stop = Heart.when_broken heart in
    let rec wait t =
      Deferred.either stop t.demand >>| function
      | `A () -> never
      | `B (`giveup g) -> {
        demand =
          return (
            `giveup (
              Deferred.either stop g >>= function
              | `A () -> return never
              | `B t -> wait t
            )
          )
      }
    in deferred (wait init_t)

  let supply_with the_demand f =
    (*
      Q/R/V - state of computation - quiet/running/value
      D/G - demand/giveup expected next
    *)
    let rec stepQD d =
      d.demand >>= function
      | `giveup g -> stepQG g

    and stepQG g =
      let computation = f () in
      stepRG g computation

    and stepRD d computation =
      Deferred.either d.demand computation >>= function
      | `A (`giveup g) -> stepRG g computation
      | `B (x,revoke) ->
        (* supply here; but no-one waiting! *)
        return (Supply.cons x revoke (stepVD d revoke))

    and stepRG g computation =
      Deferred.either g computation >>= function
      | `A d -> stepRD d computation
      | `B (x,revoke) -> return (Supply.cons x revoke (stepVG g revoke))

    and stepVD d revoke =
      Deferred.either d.demand (Heart.when_broken revoke) >>= function
      | `A (`giveup g) -> stepVG g revoke
      | `B ()-> stepQD d (* rerun computation when next demanded *)

    and stepVG g revoke =
      Deferred.either g (Heart.when_broken revoke) >>= function
      | `A d -> stepVD d revoke
      | `B () -> stepQG g (* rerun computation straight away *)

    in
    Supply.deferred (stepQD the_demand)


  type uu = unit -> unit

  module Raw : sig (* todo, inline this into two feed ops *)

    val create : unit -> t * [`step of uu]
    val upon : t -> step:uu -> unit

  end = struct

    let create () =
      let ivr = ref (Ivar.create ()) in
      let step () =
        let old_iv = !ivr in
        ivr := Ivar.create ();
        Ivar.fill old_iv ()
      in
      let trig () = Ivar.read (!ivr) in
      let rec run () = {
        demand = (trig()) >>| fun () ->
        `giveup (
          (trig()) >>| fun () ->
          run ();
        )
      }
      in
      run (), `step step

    let upon init_t ~step =
      let rec loop t =
        t.demand >>> fun (`giveup g) -> (
          step ();
          (g >>> fun t ->
           step (); loop t;
          )
        )
      in
      loop init_t

  end

  module Feed = struct
    type t = {add : uu; remove : uu;}
  end

  let feed_into () =
    let demand, `step step = Raw.create () in
    let count = ref 0 in
    let add () = (incr count; if !count=1 then step()) in
    let remove () = assert (!count>0); decr count; if !count=0 then step() in
    let feed = {Feed. add; remove;} in
    feed,demand

  let connect_feed demand feed =
    let next = ref `Demand in (* transition expected next *)
    let {Feed.add;remove} = feed in
    let step () =
      match !next with
      | `Demand -> next := `Giveup; add()
      | `Giveup -> next := `Demand; remove()
    in
    Raw.upon demand ~step

end

module Ten : sig

  type 'a t

  val lift        : (unit -> ('a * Heart.t) Deferred.t) -> 'a t
  val exec        : 'a t -> ('a * Heart.t) Deferred.t
  val reify       : 'a t -> 'a t
  val cutoff      : equal:('a -> 'a -> bool) -> 'a t -> 'a t
  val return      : 'a -> 'a t
  val bind        : 'a t -> ('a -> 'b t) -> 'b t
  val map         : 'a t -> f:('a -> 'b) -> 'b t
  val both        : 'a t -> 'b t -> ('a * 'b) t
  val all         : 'a t list -> 'a list t

  val with_semantics : 'a t -> f:('a semantics -> 'b semantics) -> 'b t

end = struct

  type 'a t = {
    process : Demand.t -> 'a Supply.t
  }

  let lift f = {
    process = fun demand ->
      Demand.supply_with demand f
  }

  (* todo: expose this -- for unbreakable lifts? *)
  (* In build.ml -- why is Builder.of_deferred behind a thunk ? *)
  let __deferred def = {
    process = fun __demand ->
      Supply.deferred (
        def >>| Supply.return
      )
  }

  (*let demand_during ~f =
    let glass = Glass.create () in
    let heart = Heart.of_glass glass in
    let demand = Demand.until Demand.always heart in
    let res = f demand in
    Glass.break glass;
    res*)

  let exec t =
    (*let __demand = Demand.never in*)
    let demand = Demand.always in
    (*demand_during ~f:fun demand ->*)
    let supply = t.process demand in
    Supply.wait (Supply.certify supply)

  let reify t =
    let feed,demand = Demand.feed_into () in
    let supply =
      let supply = t.process demand in
      let r = ref supply in
      let rec track_revoke supply =
        Supply.upon_revoke supply (fun supply ->
          r := supply;
          track_revoke supply
        )
      in
      track_revoke supply;
      r
    in
    {
      process =
        fun demand ->
          Demand.connect_feed demand feed;
          (!supply)
    }

  let cutoff ~equal t = {
    process = fun demand ->
      Supply.cutoff ~equal (t.process demand)
  }

  let return x = {
    process = fun __demand ->
      Supply.return x
  }

  let bind t f =  {
    process = fun demand ->
      let supply = Supply.certify (t.process demand) in
      Supply.bind supply (fun (x,revoked) ->
        (f x).process (Demand.until demand revoked)
      )
  }

  let map t ~f =  {
    process = fun demand ->
      Supply.map (t.process demand) ~f
  }

  let both t1 t2 = {
    process = fun demand ->
      Supply.both (t1.process demand) (t2.process demand)
  }

  let all ts = {
    process = fun demand ->
      Supply.all (List.map ts ~f:(fun t -> t.process demand))
  }

  let with_semantics t ~f = {
    process = fun demand ->
      Supply.with_semantics (t.process demand) ~f
  }

end

include Ten

let all_unit ts = map (all ts) ~f:(fun (_:unit list) -> ())

let before_redo t ~f =
  let first_time = ref true in
  let hook () =
    if !first_time
    then first_time := false
    else f ()
  in
  let f sample ~cancel = hook (); sample ~cancel in
  with_semantics t ~f

let uncancellable t =
  with_semantics t ~f:(fun sample ~cancel:__ ->
    sample ~cancel:Heart.unbreakable
  )

let with_acquire_release t ~acquire ~release =
  with_semantics t ~f:(fun sample ~cancel ->
    acquire () >>= fun () ->
    sample ~cancel >>| fun x ->
    release();
    x
  )

let desensitize t =
  let f sample ~cancel =
    sample ~cancel >>| function
    | None -> None
    | Some (x, heart) ->
      Some ((x,heart), Heart.unbreakable)
  in
  with_semantics t ~f
