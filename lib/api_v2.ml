
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
module Action = Description.Action

module Depends = Description.Depends
let ( *>>= ) = Depends.bind

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

module Env = struct
  type t = Description.Env.t

  let create ?putenv ?command_lookup_path ?build_begin ?build_end schemes =
    fun () ->
      Deferred.return (
        Description.Env.create
          ?putenv ?command_lookup_path ?build_begin ?build_end schemes
      )

end

let verbose() = Config.verbose (For_user.config ())

let load_sexp_for_jenga f path =
  Depends.path path *>>= fun () ->
  Depends.deferred (fun () ->
    For_user.load_sexp_for_jenga f path
  )

let load_sexps_for_jenga f path =
  Depends.path path *>>= fun () ->
  Depends.deferred (fun () ->
    For_user.load_sexps_for_jenga f path
  )
