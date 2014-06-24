
open Core.Std
open Async.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Path = Path
module Kind = Fs.Kind

module Glob = struct

  include Fs.Glob

  let create ~dir ?kinds glob_string = Fs.Glob.create ~dir ~kinds ~glob_string

  let create_absolute ~absolute_dir ?kinds:_ _string =
    failwithf "Glob.create_absolute: %s" absolute_dir ()

end

module Alias = Description.Alias

module Action = struct

  include Description.Action

  let bash ~dir command_string =
    shell ~dir ~prog:"bash" ~args:["-c"; command_string]

end

module Dep = struct
  include Description.Depends

  let is_absolute string = String.(string <> "") && Char.(string.[0] = '/')

  let of_path_string ~dir string =
    if is_absolute string
    then absolute ~path:string
    else path (Path.relative ~dir string)

  let contents_cutoff p =
    cutoff ~equal:String.equal (contents p)

  module List = struct
    let concat_map xs ~f = map (all (List.map xs ~f)) List.concat
  end

  let glob_listing = glob_listing_exn

end

module Rule = struct

  type t = Description.Rule.t

  let create ~targets action_depends =
    Description.Rule.Target (Description.Target_rule.create ~targets action_depends)

  let alias a deps = Description.Rule.Alias (a, Dep.all_unit deps)

  let default ~dir deps = alias (Alias.default ~dir) deps

end

module Scheme = struct
  type t = Description.Rule_scheme.t
  let create ~tag f =
    Description.Rule_scheme.create ~tag (fun ~dir ->
      Description.Rule_generator.create (f ~dir)
    )
end

module Env = Description.Env

let verbose() = Config.verbose (For_user.config ())

exception Run_now_of_internal_action_not_supported with sexp
exception Non_zero_status_from_action_run_now of Action.t with sexp

let run_action_now_output ~output action =
  match Action.case action with
  | `iaction _ -> raise Run_now_of_internal_action_not_supported
  | `xaction xaction ->
    let config = For_user.config() in
    let need = "run_now" in
    let putenv = [] in
    Job.run ~config ~need ~putenv ~xaction ~output >>= function
    | Error (`non_zero_status _) -> raise (Non_zero_status_from_action_run_now action)
    | Error (`other_error exn)   -> raise exn
    | Ok x                       -> Deferred.return x

let run_action_now =
  run_action_now_output ~output:Job.Output.ignore

let run_action_now_stdout =
  run_action_now_output ~output:Job.Output.stdout
