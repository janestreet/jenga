
open Core.Std
open Async.Std
open! Int.Replace_polymorphic_compare

module Path = Path
module Kind = Fs.Kind
module Glob = Fs.Glob
module Alias = Alias

module Action = struct
  include Action
  let process ?(ignore_stderr = false) ~dir ~prog ~args () =
    process ~dir ~prog ~args ~ignore_stderr
  ;;
end

module Var = Var
module Dep = Dep
module Reflected = Reflected
module Reflect = Reflect

module Rule = Rule
module Scheme = Scheme
module Env = Env

module Shell = struct
  let escape = Job_summary.Q.shell_escape
  let check arg result = String.(escape arg = result)
  let%test _ = (check "hello" "hello")
  let%test _ = (check "foo bar" "'foo bar'")
  let%test _ = (check "foo'bar" "'foo'\\''bar'")
end

let printf = Message.printf
let printf_verbose = Message.printf_verbose

exception Action_run_now_failed

let run_action_now_output ~output action =
  let job = Action.job action in
  let need = "run_now" in
  let putenv = [] in
  Job.run job ~need ~putenv ~output >>= function
  | Error (`command_failed _) -> raise (Action_run_now_failed)
  | Error (`other_error exn)   -> raise exn
  | Ok x                       -> Deferred.return x

let run_action_now =
  run_action_now_output ~output:Job.Output.ignore

let run_action_now_stdout =
  run_action_now_output ~output:Job.Output.stdout
