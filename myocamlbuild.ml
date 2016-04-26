(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let dispatch = function
  | Before_options ->
    Options.make_links := false
  | After_rules ->
    rule "gen-cat_api.ml"
      ~deps:["lib/gen-cat_api.sh";"lib/api.mli"]
      ~prod:"lib/cat_api.ml"
      (fun _env _build ->
        Cmd (S [A "/bin/sh";
                P "lib/gen-cat_api.sh";
                A "lib/api.mli";
                Sh ">";
                A "lib/cat_api.ml"]));

    Ocaml_plugin_ocamlbuild.embed
      ~program:"bin/jenga.native"
      ~libraries:[ "core"
                 ; "core_kernel"
                 ; "async"
                 ; "async_kernel"
                 ; "async_unix"
                 ; "sexplib"
                 ; "fieldslib"
                 ]
      ~local_cmi_files:["lib/jenga_lib.cmi"]
      ~ppx:"ppx-jane"
      ();

    let hack = "ugly_hack_to_workaround_ocamlbuild_nightmare" in
    mark_tag_used hack;
    dep [hack] [hack];

    let lib_core_mods =
      [ "dlist"
      ; "heart"
      ; "heart_intf"
      ; "heart_unit_tests"
      ; "ring"
      ; "ring_unit_tests"
      ; "tenacious"
      ; "tenacious_intf"
      ; "tenacious_unit_tests"
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

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)
