
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
module Action = Description.Action
module Xaction = Description.Xaction

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

  module Depends = Description.Depends
  module Dep1 = Description.Dep1

  type t = unit Depends.t

  let lift dep = Depends.need [dep]

  let need ts = Depends.all_unit ts

  let path x = lift (Dep1.path x)
  let glob x = lift (Dep1.glob x)
  let alias x = lift (Dep1.alias x)
  let parse_string ~dir x = lift (Dep1.parse_string ~dir x)
  let absolute ~path = lift (Dep1.absolute ~path)

  let return = Depends.return
  let ( *>>= ) = Depends.bind

  let scan ts sexp =
    Depends.scan_id (
      need ts *>>= fun () ->
      return (Description.Scan_id.of_sexp sexp)
    )

  let scanner ts scanner =
    let {Scanner.Local_deps. dir; action} = scanner in
    Depends.scan_local_deps (
      need ts *>>= fun () ->
      return (dir,action)
    )

end

module Depends = struct

  module Depends = Description.Depends
  include Depends
  let need deps = Dep.need deps

end

module Rule = struct

  module Rule = Description.Rule
  type t = Rule.t

  let create_new ~targets action_depends =
    Rule.Target (Description.Target_rule.create_new ~targets action_depends)

  let create ~targets ~deps ~action =
    create_new ~targets (
      Depends.bind (Depends.need deps) (fun () ->
        Depends.return action
      )
    )

  let alias a deps =
    Rule.Alias (a, Depends.need deps)

  let default ~dir deps =
    alias (Alias.default ~dir) deps

  let targets = Rule.targets

end

module Rule_scheme = Description.Rule_scheme

module Rule_generator = struct

  type t = Description.Rule_generator.t

  let create ~deps ~gen =
    Description.Rule_generator.create_new
      (Depends.need deps)
      ~gen

end

module Env = struct

  type t = Description.Env.t

  let create
      ?version ?putenv ?command_lookup_path ?action ?scan ?build_begin ?build_end schemes
      =
    let scan =
      match scan with
      | None -> None
      | Some f -> Some (fun sexp -> f sexp >>| Depends.need)
    in
    Description.Env.create
      ?version ?putenv ?command_lookup_path ?action ?scan ?build_begin ?build_end schemes

end

module Version = Version

let verbose() = Config.verbose (For_user.config ())

let load_sexp_for_jenga = For_user.load_sexp_for_jenga
let load_sexps_for_jenga = For_user.load_sexps_for_jenga

exception Run_now_of_internal_action_not_supported of Description.Action_id.t
exception Non_zero_status_from_action_run_now of Action.t


let run_action_now_output ~output action =
  match Action.case action with
  | `id id -> raise (Run_now_of_internal_action_not_supported id)
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
