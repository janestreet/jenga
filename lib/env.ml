
open Core.Std
open Async.Std

let putenv_for_path =
  (* lookup once and remember to avoid repeated pushing on to the front *)
  let orig_path = match (Core.Std.Sys.getenv "PATH") with | None -> "" | Some s -> s in

  let minimal_path_kept_even_after_replacement = "/bin:/usr/bin:/usr/local/bin" in
  (* Without this minimal path, user replacement can terminally break Jenga!
     For example, the ocaml_plugin module replies on the path to find "objcopy"
  *)
  let colonize xs = String.concat (List.map xs ~f:(fun x -> x ^ ":")) in
  fun spec ->
    let replacement_path  =
      match spec with
      | `Extend xs -> colonize xs ^ orig_path
      | `Replace xs -> colonize xs ^ minimal_path_kept_even_after_replacement
    in
    replacement_path

type t = {
  putenv : (string * string option) list;
  build_begin : unit -> unit Deferred.t;
  build_end : unit -> unit Deferred.t;
  artifacts_policy : Artifact_policy.t;
  scheme_for_dir : (dir:Path.t -> Scheme.t);
} [@@deriving fields]

let create
    ?(putenv=[])
    ?command_lookup_path
    ?(build_begin=(fun () -> Deferred.return ()))
    ?(build_end=(fun () -> Deferred.return ()))
    ?artifacts
    scheme_for_dir =
  let putenv = putenv @
    match command_lookup_path with
    | None -> []
    | Some spec -> [("PATH", Some (putenv_for_path spec))]
  in
  let artifacts_policy =
    match artifacts with
    | None -> Artifact_policy.Use_persistent_state
    | Some artifacts -> Artifact_policy.Artifacts artifacts
  in
  {
    putenv; build_begin; build_end; artifacts_policy; scheme_for_dir
  }

let get_scheme t ~dir = t.scheme_for_dir ~dir
