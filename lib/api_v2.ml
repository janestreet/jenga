
open Core.Std
open Async.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Path = Path
module Kind = Fs.Kind

module Glob = struct
  include Fs.Glob
  let create ~dir ?kinds glob_string = Fs.Glob.create ~dir ~kinds ~glob_string
end

module Alias = Description.Alias

module Action = struct

  include Description.Action

  let bash ~dir command_string =
    shell ~dir ~prog:"bash" ~args:["-c"; command_string]

end


module Depends = Description.Depends

module Rule = struct

  type t = Description.Rule.t

  let create ~targets action_depends =
    Description.Rule.Target (Description.Target_rule.create ~targets action_depends)

  let alias a deps = Description.Rule.Alias (a, deps)

  let default ~dir deps = alias (Alias.default ~dir) deps

  let targets = Description.Rule.targets

end

module Generator = Description.Rule_generator
module Scheme = Description.Rule_scheme
module Env = Description.Env

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
    let putenv = [] in
    Job.run ~config ~need ~putenv ~xaction ~output >>= function
    | Error `non_zero_status     -> raise (Non_zero_status_from_action_run_now action)
    | Error (`other_error exn)   -> raise exn
    | Ok x                       -> Deferred.return x

let run_action_now =
  run_action_now_output ~output:Job.Output.ignore

let run_action_now_stdout =
  run_action_now_output ~output:Job.Output.stdout
