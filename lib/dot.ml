
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

open Description

module DG = Discovered_graph
module Item = DG.Item

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


module Dump = struct

  let dump_dot t ~status_of_dep ~dot_filename =
    Writer.with_file_atomic dot_filename ~f:(fun w ->

      let visiting_or_visited = DG.Node.Hash_set.create () in

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
        | Item.Scanner _ -> "green"
        | Item.Dep dep ->
          match Dep.case dep with
          | `glob _ -> "cyan"
          | _ -> Status.colour (status_of_dep dep)

      in

      let shape_item = function
        | Item.Gen_key _ -> "circle"
        | Item.Root -> "circle"
        | Item.Target_rule _ -> "box"
        | Item.Scanner _ -> "trapezium"
        | Item.Dep dep ->
          match Dep.case dep with
          | `scan _ -> "trapezium"
          | `glob _ -> "parallelogram"
          | `path _ -> "oval"
          | `alias _ -> "octagon"
      in

      let label_item = function
        | Item.Gen_key _ -> "rule-gen"
        | Item.Root -> ""
        | Item.Target_rule tr -> let _,_,a = Target_rule.triple tr in Action.to_string a
        | Item.Scanner scanner -> sprintf "scanner: %s" (Scanner.to_string scanner)
        | Item.Dep dep ->
          match Dep.case dep with
          | `scan (_,scanner) -> sprintf "scan: %s" (Scanner.to_string scanner)
          | `glob glob -> sprintf "glob: %s" (Fs.Glob.to_string glob)
          | `path path -> Path.to_rrr_string path
          | `alias alias -> sprintf "alias: %s" (Alias.to_string alias)
      in

      let print_item id item =
      (* American spelling of color needed for dot *)
        print_line "%s[shape = %s, style = filled, fillcolor = %s, label = \"(%s) %s\"]"
          id
          (shape_item item)
          (colour_item item)
          id
          (label_item item)
      in

      let print_node _level node ~children = (* node "needs" children *)
        let id = DG.id_string node in
        let item = DG.lookup_item t node in
        print_item id item;
        List.iter children ~f:(fun child ->
          print_line "%s -> %s" (DG.id_string child) id
        )
      in

      let rec walk level node =
        let stop = Hash_set.mem visiting_or_visited node in
        if stop then () else (
          let children = DG.dependencies node in
          print_node level node ~children;
          Hash_set.add visiting_or_visited node;
          List.iter children ~f:(walk (level+1))
        )
      in

      print_line "digraph {";
      List.iter (DG.roots t) ~f:(walk 0);
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

  let dump_graph = dump

end

include Dump

