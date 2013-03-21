
open Core.Std
open Async.Std

module GG = Generic_mutable_graph

open Description

module Item = struct

  type t =
  | Root
  | Dep of Dep.t
  | Target_rule of Target_rule.t
  | Gen_key of Gen_key.t

  let to_string = function
    | Root -> "ROOT"
    | Dep dep -> (*sprintf "DEP: %s"*) (Dep.to_string dep)
    | Target_rule tr -> sprintf "RULE: %s" (Target_rule.to_string tr)
    | Gen_key g -> sprintf "GEN: %s" (Gen_key.to_string g)

end


module Catagory = struct

  type t = Error | Child_error | Waiting | Working | Good

  let merge t1 t2 = match t1,t2 with
    | Error,_ -> Error
    | _,Error -> Error
    | Child_error,_ -> Child_error
    | _,Child_error -> Child_error
    | Waiting,_ -> Waiting
    | _,Waiting -> Waiting
    | Working,_ -> Working
    | _,Working -> Working
    | Good,Good -> Good

  let colour = function
    | Error -> "red"
    | Child_error -> "magenta"
    | Good -> "green"
    | Waiting -> "white"
    | Working -> "yellow"

end


module Status = struct

  type t = Unknown | Source | Cat of Catagory.t

  let colour = function
    | Unknown -> "white"
    | Source -> "cycan"
    | Cat catagory -> Catagory.colour catagory

end


module Graph = struct

  type t = {
    graph : Item.t GG.t;
    mutable roots : Item.t GG.n list;
  }

  let create () = {
    graph = GG.create ();
    roots = [];
  }

  let graph t = t.graph

  let record_root t node = t.roots <- node :: t.roots

  let disregard_roots t = t.roots <- []

  let dump_dot t ~status_of_dep ~dot_filename =
    Writer.with_file_atomic dot_filename ~f:(fun w ->

      let visiting_or_visited = Hash_set.create ~hashable:GG.hashable_node() in

      let print_line fmt = ksprintf (fun s -> Writer.writef w "%s\n" s) fmt in

      let colour_target_rule tr =
        (* look at the targets.. *)
        let cats =
          List.filter_map (Target_rule.targets tr) ~f:(fun target ->
            match status_of_dep (Dep.path target) with
            | Status.Unknown -> None
            | Status.Source -> None
            | Status.Cat catagory -> Some catagory
          )
        in
        let cat =
          match cats with
          | [] -> Catagory.Waiting
          | cat::cats ->
            List.fold cats ~init:cat ~f:Catagory.merge
        in
        Catagory.colour cat

      in

      let colour_item = function
        | Item.Gen_key _ -> "white"
        | Item.Root -> "black"
        | Item.Target_rule tr -> colour_target_rule tr
        | Item.Dep dep ->
          match Dep.case dep with
          | `glob _ -> "cyan"
          | _ -> Status.colour (status_of_dep dep)

      in

      let shape_item = function
        | Item.Gen_key _ -> "circle"
        | Item.Root -> "circle"
        | Item.Target_rule _ -> "box"
        | Item.Dep dep ->
          match Dep.case dep with
          | `null -> "circle"
          | `scan _ -> "trapezium"
          | `glob _ -> "parallelogram"
          | `goal goal ->
            match Goal.case goal with
            | `path _ -> "oval"
            | `alias _ -> "octagon"
      in

      let label_item = function
        | Item.Gen_key _ -> "rule-gen"
        | Item.Root -> ""
        | Item.Target_rule tr -> Target_rule.to_action_string tr
        | Item.Dep dep ->
          match Dep.case dep with
          | `null -> ""
          | `scan (_,scan_id) -> sprintf "scan: %s" (Scan_id.to_string scan_id)
          | `glob glob -> sprintf "glob: %s" (Fs.Glob.to_string glob)
          | `goal goal -> sprintf "goal: %s" (Goal.to_string goal)
      in

      let print_item id item =
        (* American spelling of color needed for dot *)
        print_line "%s[shape = %s, style = filled, fillcolor = %s, label = \"%s - %s\"]"
          id
          (shape_item item)
          (colour_item item)
          id
          (label_item item)
      in

      let print_node _level node ~children = (* node "needs" children *)
        let id = GG.id_string node in
        let item = GG.value node in
        print_item id item;
        List.iter children ~f:(fun child ->
          print_line "%s -> %s" (GG.id_string child) id
        )
      in

      let rec walk level node =
        let stop = Hash_set.mem visiting_or_visited node in
        if stop then () else (
          let children = GG.inverse_step node in
          print_node level node ~children;
          Hash_set.add visiting_or_visited node;
          List.iter children ~f:(walk (level+1))
        )
      in

      print_line "digraph {";
      List.iter (List.to_list t.roots) ~f:(walk 0);
      print_line "}";
      Writer.flushed w
    )

  module Shell = Async_shell

  let postprocess_dot ~root_dir =
    try_with (fun () ->
      Shell.run_full_and_error
        ~working_dir:root_dir "./postprocess_dot.sh" []
    ) >>= fun result ->
    let show ~tag s = Message.message "dot(%s) - %s" tag s in
    match result with
    | Ok (_stdout,_stderr) ->
      return (Ok ())
    | Error exn ->
      let exn = Monitor.extract_exn exn in
      let module SP = Shell.Process in
      (
        match exn with
        | Shell.Process.Failed res ->
          show ~tag:"stdout" res.SP.stdout;
          show ~tag:"stderr" res.SP.stderr;
        | _ -> ()
      );
      return (Or_error.of_exn exn)

  let dump t ~status_of_dep =
    let root_dir = Path.to_absolute_string Path.the_root in
    let dot_filename = root_dir ^/ ".jenga.dot" in
    dump_dot t ~status_of_dep ~dot_filename >>= fun () ->
    postprocess_dot ~root_dir

end


module Node = struct

  type t = {
    node : Item.t GG.n;
  }

  let create bg ~root:value =
    let g = Graph.graph bg in
    let node = GG.add_node g value in
    Graph.record_root bg node;
    { node }

  let new_child t value =
    let old_node = t.node in
    let new_node = GG.add_node (GG.graph old_node) value in
    GG.add_edge ~src:new_node ~dest:old_node;
    { node = new_node }

  let link_parents t1 ~additional:t2 =
    let n1 = t1.node in
    let n2 = t2.node in
    match (GG.try_find_path_to_reach_node_from ~start:n2 ~goal:n1) with
    | None ->
      GG.add_edge ~src:n1 ~dest:n2;
      `ok

    | Some path ->
      let items = List.map (path@[n1]) ~f:GG.value in
      `cycle_report items

  let kill_existing_children t =
    GG.remove_all_incoming_edges_from_node t.node

end
