open Core
open! Int.Replace_polymorphic_compare

module Target_rule = Rule.Target_rule

type t = { by_target : Rule.Target_rule.t Path.Rel.Map.t
         ; sources : Path.Rel.Set.t
         ; by_alias : unit Dep.t list Alias.Map.t
         }
[@@deriving sexp_of]

let empty = { by_target = Path.Rel.Map.empty
            ; sources = Path.Rel.Set.empty
            ; by_alias = Alias.Map.empty
            }

let create ~sources rules =
  let sources =
    Path.Rel.Set.of_list
      (List.filter_map sources ~f:(fun p ->
         match Path.case p with
         | `absolute _ -> None
         | `relative rel -> Some rel))
  in
  let by_target, by_alias =
    List.partition_map rules ~f:(function
      | Rule.Target tr -> `Fst (List.map (Target_rule.targets tr) ~f:(fun p -> p, tr))
      | Rule.Alias (name, dep) -> `Snd (name, dep))
  in
  match Path.Rel.Map.of_alist (List.concat by_target) with
  | `Duplicate_key path -> `Duplicate_target path
  | `Ok by_target ->
    let by_alias = Alias.Map.of_alist_multi by_alias in
    `Ok { by_target; sources; by_alias }

let check_for_non_local_targets { by_target; sources; by_alias } ~dir =
  let q = Queue.create () in
  Set.iter sources ~f:(fun path ->
    if Path.Rel.(<>) (Path.Rel.dirname path) dir then Queue.enqueue q (`Sources path));
  Map.iteri by_target ~f:(fun ~key:path ~data:_ ->
    if Path.Rel.(<>) (Path.Rel.dirname path) dir then Queue.enqueue q (`Rule_target path));
  Map.iteri by_alias ~f:(fun ~key:alias ~data:_ ->
    if Path.Rel.(<>) (Alias.directory alias) dir then Queue.enqueue q (`Alias alias));
  Queue.to_list q
;;

let targets t = Map.keys t.by_target

let map_union m1 ~into:m2 ~f =
  Map.fold m1 ~init:m2 ~f:(fun ~key ~data:d1 acc ->
    match Map.find acc key with
    | None -> Map.set acc ~key ~data:d1
    | Some d2 -> Map.set acc ~key ~data:(f key d1 ~into:d2))
;;

let union t1 ~into:t2 =
  match
    With_return.with_return (fun r ->
      `Ok (map_union t1.by_target ~into:t2.by_target ~f:(fun key _ ~into:_ ->
        r.return (`Duplicate_target key))))
  with
  | `Duplicate_target _ as e -> e
  | `Ok by_target ->
    let sources = Set.union t1.sources t2.sources in
    let by_alias =
      map_union t1.by_alias ~into:t2.by_alias
        ~f:(fun _ l1 ~into:l2 -> List.rev_append l1 l2)
    in
    `Ok { by_target; sources; by_alias }

let find_target t rel =
  if Set.mem t.sources rel
  then `Found None
  else match Map.find t.by_target rel with
       | None -> `Not_found
       | Some rule -> `Found (Some rule)

let find_alias t alias =
  Map.find t.by_alias alias
