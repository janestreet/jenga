(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

let dispatch = function
  | Before_options ->
    Options.make_links := false
  | After_rules ->
    let env = BaseEnvLight.load () in
    let stdlib = BaseEnvLight.var_get "standard_library" env in
    rule "standalone"
      ~deps:["lib/jenga_lib.cmi"]
      ~prod:"bin/jenga_archive.c"
      (fun env build ->
        let ocaml_embed_compiler = Command.search_in_path "ocaml-embed-compiler" in
        let ocamlopt = Command.search_in_path "ocamlopt.opt" in
        let camlp4o = Command.search_in_path "camlp4o.opt" in

        (* Build the list of explicit dependencies. *)
        let packages =
          Tags.fold
            (fun tag packages ->
              if String.is_prefix "pkg_" tag then
                let idx = try String.index tag '.' with Not_found -> String.length tag in
                StringSet.add (String.sub tag 4 (idx - 4)) packages
              else
                packages)
            (tags_of_pathname "bin/jenga_archive.c")
            StringSet.empty
        in

        (* Build the list of dependencies. *)
        let deps =
          Findlib.query "type_conv" (* Hack since the findlib interface in myocamlbuild is
                                       too limited and does not allow to pass predicates
                                       to findlib. *)
          :: List.filter (fun pkg -> pkg.Findlib.name <> "type_conv")
            (Findlib.topological_closure
               (List.map Findlib.query (StringSet.elements packages)))
        in
        (* Directories to search for .cmi and .cmxs (for camlp4o): *)
        let directories =
          stdlib
          :: (stdlib / "threads")
          :: List.filter ((<>) stdlib) (List.map (fun pkg -> pkg.Findlib.location) deps)
        in
        (* List of .cmi and .cmxs (for camlp4o.opt): *)
        let cmi_list, cmxs_list =
          List.fold_right
            (fun directory acc ->
              List.fold_left
                (fun (cmi_list, cmxs_list) fname ->
                  if Pathname.check_extension fname "cmi" then
                    ((directory / fname) :: cmi_list, cmxs_list)
                  else if Pathname.check_extension fname "cmxs"
                      && String.is_prefix "pa_" fname then
                    (cmi_list, (directory / fname) :: cmxs_list)
                  else
                    (cmi_list, cmxs_list))
                acc
                (Array.to_list (Pathname.readdir directory)))
            directories ([], [])
        in
        List.iter print_endline cmxs_list;
        let cmi_list = "lib/jenga_lib.cmi" :: cmi_list in
        let camlp4 =
          match cmxs_list with
          | [] ->
            S []
          | _ :: _ ->
            S [A "-pp"; P camlp4o;
               S (List.map (fun cmxs -> S [A "-pa-cmxs"; P cmxs]) cmxs_list)]
        in
        Cmd (S [P ocaml_embed_compiler;
                camlp4;
                A "-cc"; A ocamlopt;
                S (List.map (fun cmi -> A cmi) cmi_list);
                A "-o"; A "bin/jenga_archive.c"]))

  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
