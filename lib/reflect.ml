open Core
open! Int.Replace_polymorphic_compare

let return = Dep.return
let ( *>>= ) t f = Dep.bind t ~f
let ( *>>| ) = Dep.map

let putenv = Dep.Reflect_putenv

let from_alias x = Dep.Reflect_alias x *>>| Path.Set.to_list
let from_path x = Dep.Reflect_path x

let reachable ~keep ?stop =
  let stop =
    match stop with
    | Some f -> f
    | None -> (fun x -> not (keep x))
  in
  let rec collect ~acc_trips ~acc_targets = function
    | [] -> return acc_trips
    | path::paths ->
      let skip() = collect ~acc_trips ~acc_targets paths in
      if Set.mem acc_targets path
      || stop path
      then skip()
      else
        from_path path *>>= function
        | None -> skip()
        | Some trip ->
          let acc_trips =
            if keep path
            then trip::acc_trips
            else acc_trips
          in
          let acc_targets = List.fold trip.targets ~init:acc_targets ~f:Set.add in
          collect ~acc_trips ~acc_targets (trip.Reflected.Trip.deps @ paths)
  in
  fun roots ->
    collect ~acc_trips:[] ~acc_targets:Path.Set.empty roots

let path = from_path
let alias = from_alias
