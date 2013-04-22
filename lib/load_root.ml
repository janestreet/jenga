
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module P = Ocaml_plugin.Std

module Plugin = P.Ocaml_dynloader.Make(struct
  type t = (module Jenga_root_interface.S)
  let t_repr = "Jenga_lib.Jenga_root_interface.S"
  let univ_constr = Jenga_root_interface.univ_constr
  let univ_constr_repr = "Jenga_lib.Jenga_root_interface.univ_constr"
end)

let get_env path_lr =
  let plugin_cache =
    P.Plugin_cache.Config.create
      ~dir:"/tmp/jenga-plugin-cache"
      ()
  in
  Message.load_jenga_root path_lr;
  let start_time = Time.now() in
  (*Ocaml_plugin.Shell.set_defaults ~echo:true ~verbose:true ();*)
  P.Ocaml_compiler.with_compiler ~use_cache:plugin_cache () ~f:fun compiler ->
    let loader = P.Ocaml_compiler.loader compiler in
    let filename = Path.LR.to_absolute_string path_lr in
    Plugin.load_ocaml_src_files loader [filename] >>= function
    | Error e ->
      Message.error "Plugin failed: %s " (Error.to_string_hum e);
      return (Error e)
    | Ok plugin ->
      let module M = (val plugin : Jenga_root_interface.S) in
      M.setup() >>= fun env ->
      let duration = Time.diff (Time.now()) start_time in
      Message.load_jenga_root_done path_lr duration;
      return (Ok env)
