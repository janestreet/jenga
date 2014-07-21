
open Core.Std
open Async.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Path = Path
module Kind = Fs.Kind
module Glob = Fs.Glob
module Alias = Alias
module Action = Action
module Dep = Dep
module Reflected = Reflected
module Reflect = Reflect

module Rule = Rule
module Generator = Description.Generator
module Scheme = Description.Scheme
module Env = Env

let verbose() = Config.verbose (Description.For_user.config ())

exception Non_zero_status_from_action_run_now

let run_action_now_output ~output action =
  let job = Action.job action in
  let config = Description.For_user.config() in
  let need = "run_now" in
  let putenv = [] in
  Job.run job ~config ~need ~putenv ~output >>= function
  | Error (`non_zero_status _) -> raise (Non_zero_status_from_action_run_now)
  | Error (`other_error exn)   -> raise exn
  | Ok x                       -> Deferred.return x

let run_action_now =
  run_action_now_output ~output:Job.Output.ignore

let run_action_now_stdout =
  run_action_now_output ~output:Job.Output.stdout
