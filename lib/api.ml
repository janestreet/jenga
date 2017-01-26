
open Core
open! Int.Replace_polymorphic_compare

module Located_error = Located_error
module Path = Path
module Kind = Fs.Kind
module Glob = Fs.Glob
module Alias = Alias

module Sandbox = struct
  type t = Sandbox.kind option [@@deriving sexp]
  let none = Some Sandbox.No_sandbox
  let hardlink = Some Sandbox.Hardlink
  let hardlink_ignore_targets = Some Sandbox.Hardlink_ignore_targets
  let copy = Some Sandbox.Copy
  let copy_ignore_targets = Some Sandbox.Copy_ignore_targets
  let default = None
end

module Action = struct
  include Action
  let process ?(ignore_stderr = false) ?(sandbox = Sandbox.default) ~dir ~prog ~args () =
    process ~dir ~prog ~args ~sandbox ~ignore_stderr
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

let run_action_now =
  Action.run_now ~output:Action.Output.ignore

let run_action_now_stdout =
  Action.run_now ~output:Action.Output.stdout
