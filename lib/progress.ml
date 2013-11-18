
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Description

type t = {
  status : Status.t Goal.Table.t;
}

let create () = {
  status = Goal.Table.create();
}

let set_status t = Hashtbl.set t.status

let message_errors config t =
  Hashtbl.iter (t.status) ~f:(fun ~key:goal ~data:status ->
    match (
      match status with
      | Status.Error Reason.Shutdown
      | Status.Error Reason.Error_in_deps
      | Status.Error Reason.Error_in_sibling
        -> None
      | Status.Error reason -> Some reason
      | _ -> None
    ) with
    | None -> ()
    | Some reason -> Reason.message_summary config goal reason
  )

module Snapped = struct

  type t = {

    (* todo *)
    checking  : int;
    jwait     : int;
    running   : int; (* external scanner/action *)
    usercode  : int; (* internal generator/scanner/action *)

    (* done; good *)
    source    : int;
    target    : int;
    alias     : int;

    (* done; bad *)
    error     : int;
    failure   : int; (* error in dep *)

  } with compare, bin_io

  let zero = {
    checking   = 0;
    jwait      = 0;
    running    = 0;
    usercode   = 0;
    source     = 0;
    target     = 0;
    alias      = 0;
    error      = 0;
    failure    = 0;
  }

  let total1 {
    checking;
    jwait;
    running;
    usercode;
    source;
    target;
    alias;
    error;
    failure;
  } =
    checking + jwait + running + usercode
    + source + target + alias
    + error + failure

  let todo t = t.checking + t.jwait + t.running + t.usercode
  let good t = t.source + t.target + t.alias
  let bad t = t.error + t.failure

  let total2 t = (todo t) + (good t) + (bad t)
  let total t  = let n1,n2 = total1 t,total2 t in assert (Int.(n1=n2)); n1

  let fraction t = (good t) , (total t)
  let completed t = Int.equal (good t) (total t)

  let to_labelled_triple {
    checking;
    jwait;
    running;
    usercode;
    source;
    target;
    alias;
    error;
    failure;
  } =
    [ (*todo*)

      ("c", checking,
       "target and its dependencies are being checked to ensure they are up to date");

      ("w", jwait,
       "target is ready to run an external command, but is limited by the -j threshold");

      ("j", running,
       "target is running an external build command");

      ("u", usercode,
       sprintf "target is running user code defined in %s" Misc.jenga_root_basename);

    ], [(* good *)

      ("s", source,
       "target is a source file");

      ("b", target,
       "target file is built and up to date");

      ("a", alias,
       "alias is built and up to date");

    ], [ (* bad *)

      ("error", error,
       "leaf build error; i.e. non-zero exit or target not created");

      ("failure", failure,
       "unable to build because of errors in dependencies");
    ]

  let labelled xs =
    String.concat ~sep:", "
      (List.map xs ~f:(fun (s,n,_) -> sprintf "%s=%d" s n))

  let to_string ~todo_breakdown ~good_breakdown t =
    let (good,total) = fraction t in
    let todo = todo t in
    let err = if Int.(t.error = 0) then "" else sprintf " !%d ~%d" t.error t.failure in
    let s = sprintf "todo: %d (%d / %d)%s" todo good total err in
    let todo,good,__bad = to_labelled_triple t in
    s
    ^ (if not todo_breakdown then "" else sprintf ", todo:[ %s ]" (labelled todo))
    ^ (if not good_breakdown then "" else sprintf ", good:[ %s ]" (labelled good))

end

let snap t =
  let checking = ref 0 in
  let jwait = ref 0 in
  let running = ref 0 in
  let usercode = ref 0 in
  let source = ref 0 in
  let target = ref 0 in
  let alias = ref 0 in
  let error = ref 0 in
  let failure = ref 0 in
  Hashtbl.iter (t.status)
    ~f:(fun ~key:_ ~data:status ->
      let x =
        match status with
        | Status.Checking                         -> checking
        | Status.Jwait                            -> jwait
        | Status.Running                          -> running
        | Status.Usercode                         -> usercode
        | Status.Built What.Source                -> source
        | Status.Built What.Target                -> target
        | Status.Built What.Alias                 -> alias
        | Status.Error Reason.Error_in_deps       -> failure
        | Status.Error Reason.Error_in_sibling    -> failure
        | Status.Error _                          -> error
      in incr x
    );
  {Snapped.
   checking   = !checking;
   jwait      = !jwait;
   running    = !running;
   usercode   = !usercode;
   source     = !source;
   target     = !target;
   alias      = !alias;
   error      = !error;
   failure    = !failure;
  }

let readme_lab labelled_help =
  let labelled_help = List.map labelled_help ~f:(fun (lab,_,help) -> (lab,help)) in
  let justify =
    let max =
      List.fold ~init:0 ~f:(fun x y -> if Int.(x > y) then x else y)
        (List.map labelled_help ~f:(fun (lab,_) -> String.length lab))
    in
    fun s ->
      let i = max - String.length s in
      assert(Int.(i >= 0));
      let spaces = String.make i ' ' in
      spaces
  in
  List.map labelled_help ~f:(fun (lab,help) ->
    sprintf "- %s %s: %s" lab (justify lab) help
  )

let readme () =
  let todo,good,bad = Snapped.to_labelled_triple Snapped.zero in
  let readme_todo = readme_lab todo in
  let readme_good = readme_lab good in
  let readme_bad = readme_lab bad in
  String.concat ~sep:"\n" (List.concat [
["Jem connects to the jenga instance running in the current repo,
(or waits until one is started), and displays progress reports:

    todo (good / total) !error ~failure

Where:
- todo = total - good - error - failure
- (good / total) is the omake style fraction
(The error/failure counts are show only when non-zero)"];
    readme_bad;
["
The line is optionally suffixed by an estimated finish time if the
build is still proceeding, or an indication that jenga is finished and
polling for file-system changes. Note: \"finished\" does not necessary
mean that the build was successful. Trailing \"?\"s indicate jem has not
heard from jenga for more than half a second. With the -todo and -good flags,
a more detailed breakdown for targets in that state is shown.

Todo:"];
    readme_todo;
["
Good:"];
    readme_good;
])
