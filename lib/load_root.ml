
open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

module P = Ocaml_plugin.Std

module Plugin = P.Ocaml_compiler.Make(struct
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

  let modules_for_message = function
    | SingleML _ -> []
    | Conf (_,mls) ->
      List.map mls ~f:(fun ml ->
        let s = Path.basename ml in
        match String.lsplit2 ~on:'.' s with | Some (s,_) -> s | None -> s
      )

end

let get_env config spec =
  let plugin_cache_dir =
    Path.to_absolute_string (Path.of_relative Special_paths.Dot_jenga.plugin_cache)
  in
  let plugin_cache =
    P.Plugin_cache.Config.create
      ~dir:plugin_cache_dir
      ~try_old_cache_with_new_exec:true
      ()
  in
  let path_for_message = Spec.path_for_message spec in
  Message.load_jenga_root path_for_message ~modules:(Spec.modules_for_message spec);
  let start_time = Time.now() in
  (*Ocaml_plugin.Shell.set_defaults ~echo:true ~verbose:true ();*)
  let code_style = if config.Config.deprecated_camlp4 then Some `Camlp4_style else None in
  track_loading (fun () ->
    Var.clear_all_registrations ();
    Plugin.load_ocaml_src_files
      ?code_style
      ~persistent_archive_dirpath:plugin_cache_dir
      ~use_cache:plugin_cache
      (List.map ~f:Path.to_absolute_string (Spec.ml_paths_to_load spec))
    >>= function
    | Error e ->
      Message.error "Plugin failed: %s " (Error.to_string_hum e);
      return (Error e)
    | Ok plugin ->
      let module M = (val plugin : Jenga_root_interface.S) in
      M.setup() >>= fun env ->
      let duration = Time.diff (Time.now()) start_time in
      Message.load_jenga_root_done path_for_message duration;
      return (Ok env)
  )
