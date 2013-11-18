
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module P = Ocaml_plugin.Std

module Plugin = P.Ocaml_compiler.Make(struct
  type t = (module Jenga_root_interface.S)
  let t_repr = "Jenga_lib.Jenga_root_interface.S"
  let univ_constr = Jenga_root_interface.univ_constr
  let univ_constr_repr = "Jenga_lib.Jenga_root_interface.univ_constr"
end)

let loading_count = ref 0
let is_loading () = Int.(!loading_count > 0)

let begin_loading () =
  if is_loading() then (
    Message.message "Warning: jengaroot being loaded before previous load finished";
  );
  incr loading_count

let finish_loading () = assert(is_loading()); decr loading_count

let track_loading f =
  begin_loading();
  f () >>= fun res ->
  finish_loading();
  return res

let get_env path_lr =
  let plugin_cache_dir =
    Path.to_absolute_string (Path.root_relative Path.plugin_cache_basename)
  in
  let plugin_cache =
    P.Plugin_cache.Config.create
      ~dir:plugin_cache_dir
      ~try_old_cache_with_new_exec:true
      ()
  in
  Message.load_jenga_root path_lr;
  let start_time = Time.now() in
  (*Ocaml_plugin.Shell.set_defaults ~echo:true ~verbose:true ();*)
  let filename = Path.X.to_absolute_string path_lr in
  track_loading (fun () ->
    Plugin.load_ocaml_src_files
      ~persistent_archive_dirpath:plugin_cache_dir
      ~use_cache:plugin_cache
      [filename]
    >>= function
    | Error e ->
      Message.error "Plugin failed: %s " (Error.to_string_hum e);
      return (Error e)
    | Ok plugin ->
      let module M = (val plugin : Jenga_root_interface.S) in
      M.setup() >>= fun env ->
      let duration = Time.diff (Time.now()) start_time in
      Message.load_jenga_root_done path_lr duration;
      return (Ok env)
  )
