
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Path = Path
module Kind = Fs.Kind

module Glob = struct

  include Fs.Glob

  let create ~dir ?kinds glob_string = create ~dir ~kinds ~glob_string

  let exec glob =
    let fs = For_user.fs() in
    Tenacious.exec (Fs.list_glob fs glob) >>| fun (res,_heart) ->
    match res with
    | `listing listing -> Fs.Listing.paths listing
    | _ -> failwith "Glob.exec"

end

module Alias = Description.Alias

let installed_internal_action_lookup : (Sexp.t -> unit Deferred.t) ref =
  ref (fun _ -> failwith "internal_action_lookup not installed")

module Action = struct

  include Description.Action

  let internal sexp =
    internal
      ~tag:sexp
      ~func:(fun () -> (!installed_internal_action_lookup) sexp)

end


module Depends = Description.Depends
let ( *>>= ) = Depends.bind
let ( *>>| ) = Depends.map

module Scanner = struct
  module Local_deps = struct
    type t =  {
      dir : Path.t;
      action : Action.t;
    }
  end
  type t = Local_deps.t
  let local_deps ~dir action = { Local_deps. dir; action}
end


module Dep = struct

  module Dep1 = Description.Dep1

  type t = unit Depends.t

  let installed_scanner_lookup : (Sexp.t -> t list Deferred.t) ref =
    ref (fun _ -> failwith "scanner_lookup not installed")

  let path x = Depends.path x
  let glob x = Depends.glob x *>>| fun (_:Path.t list) -> ()
  let alias x = Depends.alias x
  let absolute ~path = Depends.absolute ~path
  let parse_string ~dir x = Depends.dep1s [Dep1.parse_string ~dir x]

  let scan ts sexp =
    Depends.all_unit ts *>>= fun () ->
    Depends.deferred (fun () -> (!installed_scanner_lookup) sexp) *>>= fun ts ->
    Depends.all_unit ts

  let scanner ts scanner =
    let {Scanner.Local_deps. dir; action} = scanner in
    Depends.action_stdout (
      Depends.all_unit ts *>>| fun () -> action
    ) *>>= fun string ->
    Depends.dep1s (Dep1.parse_string_as_deps ~dir string)

end

module Rule = struct

  type t = Description.Rule.t

  let create ~targets ~deps ~action =
    Description.Rule.Target (
      Description.Target_rule.create ~targets (
        Depends.all_unit deps *>>| fun () -> action)
    )

  let create_new ~targets action_depends =
    Description.Rule.Target (Description.Target_rule.create ~targets action_depends)

  let alias a deps =
    Description.Rule.Alias (a, Depends.all_unit deps)

  let alias_new a dep = Description.Rule.Alias (a, dep)

  let default ~dir deps =
    alias (Alias.default ~dir) deps

  let targets = Description.Rule.targets

end

module Rule_scheme = Description.Rule_scheme

module Rule_generator = struct

  type t = Description.Rule_generator.t

  let create ~deps ~gen =
    Description.Rule_generator.create (
      Depends.all_unit deps *>>= fun () ->
      Depends.deferred gen
    )

  let create_new = Description.Rule_generator.create

end

module Env = struct

  type t = Description.Env.t

  let create
      ?version ?putenv ?command_lookup_path ?action ?scan ?build_begin ?build_end schemes
      =

    let () =
      match scan with
      | None -> ()
      | Some scan -> Dep.installed_scanner_lookup := scan
    in

    let () =
      match action with
      | None -> ()
      | Some action -> installed_internal_action_lookup := action
    in

    Description.Env.create
      ?version ?putenv ?command_lookup_path
      ?build_begin ?build_end schemes

end

module Version = Version

let verbose() = Config.verbose (For_user.config ())

let load_sexp_for_jenga = For_user.load_sexp_for_jenga
let load_sexps_for_jenga = For_user.load_sexps_for_jenga

exception Run_now_of_internal_action_not_supported
exception Non_zero_status_from_action_run_now of Action.t


let run_action_now_output ~output action =
  match Action.case action with
  | `iaction _ -> raise Run_now_of_internal_action_not_supported
  | `xaction xaction ->
    let config = For_user.config() in
    let need = "run_now" in
    let rel_path_semantics = Forker.Rel_path_semantics.New_wrt_working_dir in
    let putenv = [] in
    Job.run ~config ~need ~rel_path_semantics ~putenv ~xaction ~output >>= function
    | Error `non_zero_status     -> raise (Non_zero_status_from_action_run_now action)
    | Error (`other_error exn)   -> raise exn
    | Ok x                       -> Deferred.return x


let run_action_now =
  run_action_now_output ~output:Job.Output.ignore

let run_action_now_stdout =
  run_action_now_output ~output:Job.Output.stdout

let enqueue_file_access = File_access.enqueue
