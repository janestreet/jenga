
open Core
open! Int.Replace_polymorphic_compare
open Async

module Plugin = Ocaml_plugin.Compiler.Make(struct
  type t = (module Jenga_root_interface.S)
  let t_repr = "Jenga_lib.Jenga_root_interface.S"
  let univ_constr = Jenga_root_interface.univ_constr
  let univ_constr_repr = "Jenga_lib.Jenga_root_interface.univ_constr"
end)

let loading_count = ref 0
let is_loading () = !loading_count > 0

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

module Spec = struct

  type t =
  | SingleML of Path.t
  | Conf of Path.t * Path.t list

  let ml_file ~ml = SingleML ml
  let config_file ~conf ~mls = Conf (conf,mls)

  let ml_paths_to_load = function
    | SingleML ml -> [ml]
    | Conf (_,mls) -> mls

  let path_for_message = function
    | SingleML ml -> ml
    | Conf (conf,_) -> conf
end

let get_env spec =
  let plugin_cache_dir =
    Path.to_absolute_string (Path.of_relative Special_paths.Dot_jenga.plugin_cache)
  in
  let plugin_cache =
    Ocaml_plugin.Plugin_cache.Config.create
      ~dir:plugin_cache_dir
      ~try_old_cache_with_new_exec:true
      ()
  in
  let path_for_message = Spec.path_for_message spec in
  Message.load_jenga_root path_for_message;
  let start_time = Time.now() in
  track_loading (fun () ->
    Var.clear_all_registrations ();
    Plugin.load_ocaml_src_files_without_running_them
      ~persistent_archive_dirpath:plugin_cache_dir
      ~use_cache:plugin_cache
      (List.map ~f:Path.to_absolute_string (Spec.ml_paths_to_load spec))
    >>= function
    | Error _ as e -> return e
    | Ok run_plugin ->
      match Result.try_with run_plugin with
      | Error e -> return (Ok (`Toplevel_exn e))
      | Ok (module M : Jenga_root_interface.S) ->
        M.setup() >>= fun env ->
        let duration = Time.diff (Time.now()) start_time in
        Message.load_jenga_root_done path_for_message duration;
        return (Ok (`Env env))
  )
