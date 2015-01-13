(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

(* This list is copied from the jenga/root.ml file used at Jane Street. It should be
   enough for most cases. *)
let stdlib_cmi_to_embed =
  [ "pervasives.cmi"
  ; "camlinternalLazy.cmi"
  ; "camlinternalMod.cmi"
  ; "camlinternalOO.cmi"
  ; "camlinternalFormatBasics.cmi"
  ; "lexing.cmi"
  ; "printf.cmi"
  ; "digest.cmi"
  ]

let dispatch = function
  | Before_options ->
    Options.make_links := false
  | After_rules ->
    let env = BaseEnvLight.load () in
    let stdlib = BaseEnvLight.var_get "standard_library" env in
    rule "gen-cat_api.ml"
      ~deps:["lib/gen-cat_api.sh";"lib/api.mli"]
      ~prod:"lib/cat_api.ml"
      (fun _env _build ->
        Cmd (S [A "/bin/sh";
                P "lib/gen-cat_api.sh";
                A "lib/api.mli";
                Sh ">";
                A "lib/cat_api.ml"]));
    rule "standalone"
      ~deps:["lib/jenga_lib.cmi"]
      ~prod:"bin/jenga_archive.c"
      (fun env build ->
        let ocaml_embed_compiler = Command.search_in_path "ocaml-embed-compiler" in
        let ocamlopt = Command.search_in_path "ocamlopt.opt" in
        let camlp4o = Command.search_in_path "camlp4o.opt" in
        let cmi_list =
          List.concat
            [ List.map (fun fn -> stdlib / fn) stdlib_cmi_to_embed
            ; [ "lib/jenga_lib.cmi" ]
            ; List.map (fun pkg -> (Findlib.query pkg).Findlib.location / pkg ^ ".cmi")
                [ "core"
                ; "core_kernel"
                ; "async"
                ; "async_kernel"
                ; "async_unix"
                ; "sexplib"
                ; "fieldslib"
                ; "ocaml_plugin"
                ]
            ]
        in
        let cmxs_list =
          (stdlib / "bigarray.cmxs") ::
          List.map (fun (pkg, pa) -> (Findlib.query pkg).Findlib.location / pa ^ ".cmxs") [
            "type_conv", "pa_type_conv";
            "sexplib", "sexplib";
            "sexplib", "pa_sexp_conv";
            "bin_prot", "pa_bin_prot";
            "fieldslib", "pa_fields_conv";
            "variantslib", "pa_variants_conv";
            "comparelib", "pa_compare";
            "pa_pipebang", "pa_pipebang";
            "herelib", "pa_herelib";
            "custom_printf", "pa_custom_printf";
            "pa_test", "pa_test_lib";
          ]
        in
        let camlp4 =
          match cmxs_list with
          | [] ->
            N
          | _ :: _ ->
            S [A "-pp"; P camlp4o;
               S (List.map (fun cmxs -> S [A "-pa-cmxs"; P cmxs]) cmxs_list)]
        in
        Cmd (S [P ocaml_embed_compiler;
                camlp4;
                A "-cc"; A ocamlopt;
                S (List.map (fun cmi -> A cmi) cmi_list);
                A "-o"; A "bin/jenga_archive.c"]));


    let hack = "ugly_hack_to_workaround_ocamlbuild_nightmare" in
    mark_tag_used hack;
    dep [hack] [hack];

    let lib_core_mods =
      [ "heart_interface"
      ; "heart"
      ; "interface"
      ; "ring"
      ; "tenacious"
      ; "test_hearts"
      ; "test_ring"
      ; "test_tenacious"
      ; "weak_ref"
      ]
    in

    let add_exts l exts =
      List.concat (List.map (fun fn ->
        let fn = "tenacious/lib/" ^ fn in
        List.map (fun ext -> fn ^ ext)  exts)
        l)
    in

    rule hack
      ~prod:hack
      ~deps:(add_exts lib_core_mods [".cmx"; ".cmi"; ".cmo"])
      (fun _ _ ->
         let to_remove =
           add_exts lib_core_mods [ ".cmx"
                                  ; ".cmi"
                                  ; ".cmo"
                                  ; ".ml"
                                  ; ".mli"
                                  ; ".ml.depends"
                                  ; ".mli.depends"
                                  ; ".o"
                                  ]
         in
         Seq
           [ Seq (List.map rm_f to_remove)
           ; Echo ([], hack) ])

  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
