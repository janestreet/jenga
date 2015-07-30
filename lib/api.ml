
open Core.Std
open Async.Std
open! No_polymorphic_compare

module Path = Path
module Kind = Fs.Kind
module Glob = Fs.Glob
module Alias = Alias

module Action = Action

module Dep = Dep
module Reflected = Reflected
module Reflect = Reflect

module Rule = Rule
module Scheme = Scheme
module Env = Env

module Artifact_policy = Artifact_policy

module Shell = struct
  let escape = Message.Q.shell_escape
  let check arg result = String.(escape arg = result)
  TEST = (check "hello" "hello")
  TEST = (check "foo bar" "'foo bar'")
  TEST = (check "foo'bar" "'foo'\\''bar'")
end

let verbose() = Config.verbose (Run.For_user.config ())

exception Action_run_now_failed

let run_action_now_output ~output action =
  let job = Action.job action in
  let config = Run.For_user.config() in
  let need = "run_now" in
  let putenv = [] in
  Job.run job ~config ~need ~putenv ~output >>= function
  | Error (`command_failed _) -> raise (Action_run_now_failed)
  | Error (`other_error exn)   -> raise exn
  | Ok x                       -> Deferred.return x

let run_action_now =
  run_action_now_output ~output:Job.Output.ignore

let run_action_now_stdout =
  run_action_now_output ~output:Job.Output.stdout

let _ = Run.main
