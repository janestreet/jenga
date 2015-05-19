open! Core.Std
open! Async.Std

(* aliases for custom printf *)
let escaped = Message.Q.shell_escape
let escaped_list = Message.Q.shell_escape_list

let maybe_sandbox =
  let module M = Unique_id.Int() in
  fun ~sandbox action ~deps ~targets ->
    if not sandbox
    then action
    else begin
      let tmpdir =
        Path.root_relative (sprintf ".jenga/separate-dir%d" (M.create () :> int))
      in
      let reach_from_root file = Path.reach_from ~dir:Path.the_root file in
      let relative_to_tmpdir file =
        sprintf "%s/%s" (reach_from_root tmpdir) (reach_from_root file)
      in
      let cd_tmpdir, cd_out_of_tmpdir =
        sprintf !"cd %{escaped}" (reach_from_root tmpdir), sprintf "cd ../.."
      in
      let job = Action.job action in
      let `Dirs dirs, `Files files, `Arbitrary_files empty_files =
        Db.Proxy_map.filesystem_assumptions deps
      in
      let cleanup = sprintf !"rm -rf -- %{escaped}" (reach_from_root tmpdir) in
      let sh_prelude =
        let mkdirs =
          Hash_set.add dirs (Db.Job.dir job);
          sprintf !"mkdir -p -- %{escaped_list}"
            (List.map (Hash_set.to_list dirs) ~f:relative_to_tmpdir)
        in
        let copies =
          (* We can't express depending shallowly on a symlink, so it's ok to do a deep
             copy. *)
          List.map (Hash_set.to_list files) ~f:(fun file ->
            sprintf !"cp -L -- {,%{escaped}/}%{escaped}"
              (reach_from_root tmpdir) (reach_from_root file))
        in
        let touches =
          match Hash_set.to_list empty_files with
          | [] -> []
          | _ :: _ as empty_files ->
            [ sprintf !"touch -- %{escaped_list}"
                (List.map empty_files ~f:relative_to_tmpdir) ]
        in
         String.concat ~sep:";" (cleanup :: mkdirs :: copies @ touches @ [cd_tmpdir])
      in
      let sh_postlude =
        let copies_back =
          List.map targets ~f:(fun target ->
            let target = Path.of_relative target in
            (* --remove-destination is to overwrite the destination if it is a symlink. -P
               is to copy a symlink if we have a symlink. *)
            sprintf !"cp --remove-destination -P -- {%{escaped}/,}%{escaped} 2> /dev/null || \
                      rm -f -- %{escaped} 2>/dev/null || true"
              (reach_from_root tmpdir) (reach_from_root target) (reach_from_root target))
        in
        String.concat ~sep:";" (cd_out_of_tmpdir :: copies_back @ [cleanup])
      in
      (* We go over the command line size limits when running rules with tens of
         thousands of dependencies. These are rare so let's just skip them. *)
      if String.length sh_prelude + String.length sh_postlude > 300_000
      then action
      else
        let new_job = Job.bracket job ~sh_prelude ~sh_postlude in
        Action.of_job new_job
    end
;;
