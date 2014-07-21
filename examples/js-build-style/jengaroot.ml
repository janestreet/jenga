
module Wrap_style = struct
  type t = Pack | Prefix
  let _ = (Pack,Prefix)
  let select = Pack
  let packing = match select with Pack -> true | Prefix -> false
end

open Core.Std
open Async.Std
open Jenga_lib.Api

module List = struct
  include List

  let concat_cartesian_product l1 l2 =
    List.map (List.cartesian_product l1 l2) ~f:(fun (x, y) -> x ^ y)
end

let put fmt = ksprintf (fun s -> Printf.printf "%s\n%!" s) fmt
let message fmt = ksprintf (fun s -> Printf.printf "!!JengaRoot.ml : %s\n%!" s) fmt

let return = Dep.return
let ( *>>= ) = Dep.bind
let ( *>>| ) = Dep.map

module Rule = struct (* planned changes for API v3 *)

  include Rule

  (* simple(old-style) rule creation; fixed deps/action *)
  let create1 ~targets ~deps ~action =
    Rule.create ~targets (
      Dep.all_unit deps *>>| fun () -> action
    )

  let create_relative ~dir ~targets ~deps ~non_relative_deps monadic_action =
    let targets = List.map targets ~f:(fun name -> Path.relative ~dir name) in
    let deps =
      non_relative_deps @
      List.map deps ~f:(fun name -> Dep.path (Path.relative ~dir name))
    in
    create ~targets (Dep.all_unit deps *>>= fun () -> monadic_action)

end

let relative = Path.relative
let dotdot = Path.dotdot
let root_relative = Path.root_relative
let basename = Path.basename
let dirname = Path.dirname
let suffixed ~dir name suf = relative ~dir (name ^ suf)

let at_head v ls =
  v :: (List.filter ls ~f:(fun x -> not (x = v)))
let list_at_head head ls =
  List.fold_right ~init:ls head ~f:at_head

let lines_of_string string =
  let lines = String.split string ~on:'\n' in
  let lines = List.filter lines ~f:(function | "" -> false | _ -> true) in
  lines

let words_of_string string =
  let string = String.tr string ~target:'\n' ~replacement:' ' in
  let words = String.split string ~on:' ' in
  let words = List.filter words ~f:(function | "" -> false | _ -> true) in
  words

let uncomment_line s =
  match String.lsplit2 s ~on:'#' with None -> s | Some (s,_comment) -> s

let uncomment s =
  String.concat ~sep:"\n" (List.map ~f:uncomment_line (String.split ~on:'\n' s))

let file_words path =
  Dep.contents path *>>| words_of_string

let file_words_allow_commments path =
  Dep.contents path *>>| uncomment *>>| words_of_string

module Alias = struct
  include Alias
  let default ~dir = Alias.create ~dir "DEFAULT"
  let runtest ~dir = Alias.create ~dir "runtest"
  let qtest ~dir = Alias.create ~dir "qtest"
  let pp ~dir = Alias.create ~dir "pp"
  let runbench ~dir = Alias.create ~dir "runbench"
  let libdeps ~dir = Alias.create ~dir "libdeps"
  let merlin ~dir = Alias.create ~dir "merlin"
  let mycaml ~dir = Alias.create ~dir "mycaml"
  let utop ~dir = Alias.create ~dir "utop"
  (* aliases not intended to be recursive.. *)
  let lib_artifacts ~dir = Alias.create ~dir "lib_artifacts"
  let submodule_cmis ~dir = Alias.create ~dir "submodule_cmis"
end

let read_sexp_throttle =
  (* The throttle is to avoid exceeding fd limits. *)
  assert (Thread_safe.am_holding_async_lock ());
  Throttle.create ~continue_on_error:true ~max_concurrent_jobs:32

let read_then_convert_string_via_reader :
    (
      path : Path.t ->
      contents : (Path.t -> string Dep.t) ->
      do_read : (Reader.t -> 'a Deferred.t) ->
      'a Dep.t
    ) =
  fun ~path ~contents ~do_read ->
    contents path *>>= fun string ->
    Dep.deferred (fun () ->
      try_with (fun () ->
        let outcome_ivar = Ivar.create () in
        Throttle.enqueue read_sexp_throttle (fun () ->
        (* This trick enables the reading of a sexp from a string using [Reader]'s
           parsing functions.  This is easier than using the low-level [Sexplib]
           interface (partially since we don't want to use the lex/yacc-based
           implementation, since it blocks, and the interface not using that is more
           involved). *)
          let info = Info.of_string ("Description.convert_using_reader " ^ Path.to_string path) in
          let pipe = Pipe.init (fun writer -> Pipe.write writer string) in
          Reader.of_pipe info pipe >>= fun reader ->
          do_read reader >>= fun outcome ->
          Reader.close reader >>| fun () ->
          Ivar.fill outcome_ivar outcome
        ) >>= fun () ->
        Ivar.read outcome_ivar
      ) >>| function
      | Ok x -> x
      | Error exn ->
        put "File \"%s\", line 1, characters 1-1:" (Path.to_string path);
        put "Error: sexp conversion\n%s" (Exn.to_string exn);
        failwithf "%s: sexp conversion" (Path.to_string path) ()
    )


let glob_ml = Glob.create "*.ml"
let glob_mli = Glob.create "*.mli"

let wrap_arg_for_linker arg = [ "-cclib"; "-Xlinker"; "-cclib";  arg; ]

(*----------------------------------------------------------------------
 getenv...
----------------------------------------------------------------------*)

let getenv ~of_string ~to_string varname ~default =
  let value =
    match Core.Std.Sys.getenv varname with
    | None -> default
    | Some s -> of_string s
  in
  if verbose() then (
    message "%s = %s" varname (to_string value);
  );
  value

let getenv_bool = getenv
  ~of_string:(function | "false" | "0" -> false | _ -> true)
  ~to_string:Bool.to_string

let getenv_args = getenv
  ~of_string:(String.split ~on:' ')
  ~to_string:(String.concat ~sep:" ")

let __getenv_enumeration varname ~choices ~default ~to_string =
  let err() = failwithf "getenv_enumeration: %s" varname () in
  let override =
    match Core.Std.Sys.getenv varname with
    | None -> to_string default
    | Some s -> s
  in
  let choices_string = String.concat ~sep:" " (List.map choices ~f:to_string) in
  if verbose() then (
    message "%s = %s # choices: %s" varname override choices_string
  );
  let of_string_opt =
    let h = Hashtbl.Poly.create () in
    List.iter choices ~f:(fun choice ->
      let string = to_string choice in
      match (Hashtbl.add h ~key:string ~data:choice) with | `Ok -> () | `Duplicate ->
        (* error in the rules *)
        message "Multiple choices for `%s' selected by string `%s'" varname string;
        err()
    );
    Hashtbl.find h
  in
  match of_string_opt override with
  | Some value -> value
  | None ->
    (* user error *)
    message "%s should not be %s but one of: %s." varname override choices_string;
    message "If you are running OMake from emacs, do C-c C-o %s to switch to %s=valid-value,"
      varname varname;
    message "and then C-c C-c to restart your build.";
    err()

let version_util_support =
  getenv_bool "VERSION_UTIL_SUPPORT" ~default:false

let link_executables =
  getenv_bool "LINK_EXECUTABLES" ~default:true

let x_library_inlining =
  getenv_bool "X_LIBRARY_INLINING" ~default:false

let with_mycaml =
  getenv_bool "WITH_MYCAML" ~default:false

let with_utop =
  getenv_bool "WITH_UTOP" ~default:false

module Ordered_set_lang : sig

  (* This is a representation for an ordered list of strings, plus some set like
     operations. *)
  type t with of_sexp

  val eval_with_standard : t option -> standard:string list -> string list

end = struct

  type t = Sexp.t with of_sexp

  let eval t ~special_values =
    let rec of_sexp = function
      | Sexp.Atom "\\" -> failwith "unexpected \\"
      | Sexp.Atom s ->
        begin match String.chop_prefix s ~prefix:":" with
        | None -> [s]
        | Some name ->
          match List.Assoc.find special_values name with
          | Some l -> l
          | None -> failwithf "undefined symbol %s" s ()
        end
      | Sexp.List sexps -> of_sexps [] sexps
    and of_sexps acc = function
      | Sexp.Atom "\\" :: sexps -> of_sexps_negative acc sexps
      | elt :: sexps ->
        let elts = of_sexp elt in
        of_sexps (List.rev_append elts acc) sexps
      | [] -> List.rev acc
    and of_sexps_negative acc = function
      | Sexp.Atom "\\" :: sexps -> of_sexps_negative acc sexps
      | elt :: sexps ->
        let elts = of_sexp elt in
        let acc = List.filter acc ~f:(fun acc_elt -> not (List.mem elts acc_elt)) in
        of_sexps_negative acc sexps
      | [] -> List.rev acc
    in
    of_sexp t

  let eval_with_standard t_opt ~standard =
    match t_opt with
    | None -> standard
    | Some t -> eval t ~special_values:[("standard", standard)]

end

let transitive_runners =
  getenv_bool "TRANSITIVE_RUNNERS" ~default:false

let alias_for_inline_runners =
  if transitive_runners then Alias.lib_artifacts else Alias.default

module Ocaml_version = struct

  let home =
    match Sys.getenv "HOME" with
    | None -> assert false
    | Some s -> s

  let root = home ^ "/.opam"

  let __name = "4.02.0+trunk"
  let name = "4.01.0"

  let disabled_warnings =
    (* Below are the warnings, as reported by -warn-help.  Warnings 40 and beyond are
       new with OCaml 4.01.

       4 Fragile pattern matching: matching that will remain complete even if additional
       constructors are added to one of the variant types matched.

       29 Unescaped end-of-line in a string constant (non-portable code).

       41 Ambiguous constructor or label name.

       44 Open statement shadows an already defined identifier.

       45 Open statement shadows an already defined label or constructor.

       48 implicit elimination of optional arguments

    *)

    [ 3; 4; 29; 41; 44; 45; 48 ]
  ;;

  let ocamloptflags = []

  let ocamlflags = ["-short-paths"]

  let cflags = [ "-O2"; "-fomit-frame-pointer" ]

end

module Top = struct

  let ocamlcflags =
    getenv_args "OCAMLCFLAGS" ~default:["-g"]

  let ocamloptflags =
    getenv_args "OCAMLOPTFLAGS" ~default:(
      ["-inline"; "20"; "-nodynlink"; "-g"] @ Ocaml_version.ocamloptflags
    )

  let default_common_flags ~disabled_warnings =
    let ocamlwarnings =
      "@a" ^ String.concat (List.map disabled_warnings ~f:(fun n -> "-" ^ Int.to_string n))
    in
    [
      "-I"; "+camlp4";
      "-w"; ocamlwarnings;
      "-strict-sequence";
    ]

  let default_merlinflags = default_common_flags

  let default_ocamlflags ~disabled_warnings =
    List.concat [
      default_common_flags ~disabled_warnings;
      [
        "-thread";
        "-bin-annot"
      ];
      Ocaml_version.ocamlflags
    ]
end

let debug_cflags = [
  "-pipe";
  "-g";
  "-fPIC";
  "-DPIC";
  "-O0";
  "-Wall";
  "-pedantic";
  "-Wextra";
  "-Wunused";
  "-Werror";
  "-Wno-long-long";
  "-DCAML_NAME_SPACE";
]

let cc_prog         = "gcc"
let cxx_prog        = "g++"
let ocamlopt_prog   = "ocamlopt.opt"
let ocamlc_prog     = "ocamlc.opt"
let ocamldep_prog   = "ocamldep.opt"

let default_cflags = debug_cflags @ Ocaml_version.cflags
let default_cxxflags = default_cflags

let ocaml_bin   = Ocaml_version.root ^ "/" ^ Ocaml_version.name ^ "/bin"
let ocaml_where = Ocaml_version.root ^ "/" ^ Ocaml_version.name ^ "/lib/ocaml"

let dot_ocaml_bin = root_relative ".omake-ocaml-bin"
let ocaml_bin_dep = Dep.path dot_ocaml_bin

let ocamlpacks = ["nums"; "unix"; "threads"; "bigarray"; "str"]

let pa_jane = [
  "pa_type_conv";
  "pa_sexp_conv";
  "pa_bin_prot";
  "pa_fields_conv";
  "pa_variants_conv";
  "pa_typerep_conv";
  "pa_compare";
  "pa_pipebang";
  "pa_here";
  "pa_custom_printf";
  "pa_test";
  "pa_enumerate";
]

let pa_sexp_conv = [
  "pa_type_conv";
  "pa_sexp_conv";
]

let expand_predefined_pa_sets names =
  List.concat_map names ~f:(function
  | "JANE" -> pa_jane
  | "SEXP_CONV" -> pa_sexp_conv
  | name -> [name]
  )

let expand_dollar_vars_between ~left ~right ~lookup orig =
  match String.lsplit2 orig ~on:'$' with
  | None -> orig (* no dollars, do nothing *)
  | Some (before_first_dollar, after_first_dollar) ->
    let translate after_dollar =
      match (
        match String.chop_prefix after_dollar ~prefix:(String.make 1 left) with
        | None -> None
        | Some after_lp ->
          match String.lsplit2 after_lp ~on:right with
          | None -> None
          | Some (var_name, after_rp) ->
            match (lookup ~var_name) with
            | None -> None
            | Some x -> Some (x, after_rp)
      ) with
      | None -> "$" ^ after_dollar (* cant translate - leave the string as it is*)
      | Some (expansion, after_rp) -> expansion ^ after_rp
    in
    let rec loop acc = function
      | [] -> assert false
      | [last] -> String.concat (List.rev (translate last::acc))
      | after_dollar::xs -> loop (translate after_dollar :: acc) xs
    in
    loop [before_first_dollar] (String.split after_first_dollar ~on:'$')

let expand_dollar_vars ~lookup s =
  let s = expand_dollar_vars_between ~left:'(' ~right:')' ~lookup s in
  let s = expand_dollar_vars_between ~left:'{' ~right:'}' ~lookup s in
  s

let table_to_lookup ~table =
  let h = Hashtbl.Poly.create () in
  let () =
    List.iter table ~f:(fun (key,data) ->
      match (Hashtbl.add h ~key ~data) with
      | `Ok -> ()
      | `Duplicate -> failwithf "duplicate binding for $-var: %s" key ()
    )
  in
  fun ~var_name -> Hashtbl.Poly.find h var_name

(* Exapnd some $-vars within action strings of rules defined in jbuild files *)
let root_var_table = [
  "-verbose"            , "";
  "CPP"                 , "cpp";
  "CC"                  , cc_prog;
  "CXX"                 , cxx_prog;
  "PROCESSOR"           , "x86_64";
  "OCAMLC"              , ocamlc_prog;
  "OCAMLOPT"            , ocamlopt_prog;
  "OCAMLCFLAGS"         , String.concat ~sep:" " Top.ocamlcflags;
  "OCAMLOPTFLAGS"       , String.concat ~sep:" " Top.ocamloptflags;
  "ocaml_version"       , Ocaml_version.name;
  "ocaml_where"         , ocaml_where;
]

let root_var_lookup = table_to_lookup ~table:root_var_table

let expand_vars_root = expand_dollar_vars ~lookup:root_var_lookup

let shell_escape s =
  "'" ^ String.concat_map s ~f:(function
    | '\'' -> "'\\''"
    | c -> String.make 1 c
  ) ^ "'"

(*----------------------------------------------------------------------
 bash
----------------------------------------------------------------------*)

let bash ~dir command_string =
  Action.shell ~dir ~prog:"bash" ~args:[
    "-e"; "-u"; "-o"; "pipefail";
    "-c"; command_string
  ]

let bashf ~dir fmt =
  ksprintf (fun str -> bash ~dir str) fmt

module Bash : sig

  type t
  val create : prog:string -> args:string list -> target:string option -> t
  val action: dir:Path.t -> t list -> Action.t

end = struct

  type t = string

  let create ~prog ~args ~target =
    let com = String.concat ~sep:" "
      (prog :: List.map args ~f:shell_escape)
    in
    match target with
    | None -> com
    | Some target -> sprintf "%s > %s" com target

  let action ~dir ts =
    let command_string = String.concat ~sep:"; " ts in
    bash ~dir command_string

end

let bash1 ?target prog args = Bash.create ~prog ~args ~target

let echo_string ~target string =
  bash1 ~target "echo" ["-e"; string]

(* basic action for writing strings to files *)
let write_string_action ?chmod_x string ~target =
  let dir = dirname target in
  Bash.action ~dir ([
    echo_string ~target:(basename target) string
  ] @ (
    match chmod_x with | None -> [] | Some () ->
      [bash1 "chmod" ["+x"; basename target]]
  ))

let write_string_rule ?chmod_x string ~target =
  Rule.create1 ~deps:[] ~targets:[target]
    ~action:(write_string_action ?chmod_x string ~target)

let write_names_rule names ~target =
  write_string_rule (String.concat ~sep:" " names) ~target

(*----------------------------------------------------------------------
 jswrap for Centos release 5
----------------------------------------------------------------------*)

module Centos = struct

  type t = Release_5 | Release_6 with sexp

  let __parse_redhat_release_file_contents string =
    if String.is_prefix string ~prefix:"CentOS release 5" then Release_5 else
    if String.is_prefix string ~prefix:"CentOS release 6" then Release_6 else
    failwith "Unknown CentOS release.  Giving up."

  let version_where_building = return Release_6

  let has_wrap_rpm =
    false

  let wrap_funs =
    if has_wrap_rpm then [
    "timerfd_settime";
    "timerfd_gettime";
    "timerfd_create";
    "recvmmsg";
  ] else []

  let c5compatroot = "/usr/jane"

  let c5_compat_root_dirs =
    List.map ~f:(fun x -> c5compatroot ^ "/" ^ x) [
      "lib64";
      "usr/lib64";
      "usr/lib64/mysql";
    ]

  let wrap_rpath_for_ocaml x =
    ["-ccopt"; "-Xlinker"; "-ccopt"; "-rpath"; "-ccopt"; "-Xlinker"; "-ccopt"; x ]

  let wrap_wrap_for_ocaml x =
    ["-ccopt"; "-Xlinker"; "-ccopt"; "--wrap"; "-ccopt"; "-Xlinker"; "-ccopt"; x ]

  let ocamllibflags =
    version_where_building *>>| function
    | Release_5 -> []
    | _ ->
      List.concat_map c5_compat_root_dirs ~f:wrap_rpath_for_ocaml
      @ List.concat_map wrap_funs ~f:wrap_wrap_for_ocaml
      @ (if has_wrap_rpm then ["-ccopt"; "-ljswrap"] else [])

end

(*----------------------------------------------------------------------
 jbuild-ignore
----------------------------------------------------------------------*)

let ignore_filter ~dir =
  let path = relative ~dir "jbuild-ignore" in
  Dep.file_exists path *>>= function
  | false -> return (fun _ -> false) (* no subdir is ignored *)
  | true ->
    file_words_allow_commments path *>>= fun words ->
    List.iter words ~f:(fun word ->
      if String.mem word '/' then
        failwithf "%s: %s can't be a directory basename"
          (Path.to_string path) word ()
    );
    let set = String.Hash_set.of_list words in
    return (fun path ->
      Hash_set.mem set (basename path)
    )

let unignored_subdirs ~dir =
  Dep.subdirs ~dir *>>= fun paths ->
  ignore_filter ~dir *>>| fun ignore_p ->
  List.filter paths ~f:(fun path -> not (ignore_p path))

let deep_unignored_subdirs ~dir =
  let rec traverse dir =
    unignored_subdirs ~dir *>>= fun dirs ->
    (Dep.all (List.map dirs ~f:traverse) *>>| List.concat) *>>| fun dirs ->
    dir::dirs
  in
  traverse dir

(*----------------------------------------------------------------------
 clean external when the compiler version changes
----------------------------------------------------------------------*)

let tmpdir = ".jenga.tmp"
let delete_tmpdir () =
  (* We want to clean the tmpdir when jenga restarts, but not every time the build
     restarts, or that the jengaroot is changed. This is why we don't run this action
     in build_begin, but instead run it once and add a dependency on the pid. *)
  let pid = Unix.getpid () in
  Dep.action
    (return
       (bash ~dir:Path.the_root
          (sprintf "rm -rf %s; mkdir -p %s # %d"
             tmpdir tmpdir (Pid.to_int pid))))

let setup_dot_ocaml_bin =
  Scheme.create ~tag:".omake-ocaml-bin" (fun ~dir ->
    assert (dir = Path.the_root);
    delete_tmpdir () *>>| fun () ->
    [write_string_rule ocaml_bin ~target:dot_ocaml_bin]
  )

(*----------------------------------------------------------------------
 recursive aliases
----------------------------------------------------------------------*)

let recursive_alias_list = [
  Alias.default;
  Alias.runtest;
  Alias.runbench;
  Alias.qtest;
  Alias.pp;
  Alias.libdeps;
  Alias.merlin;
  Alias.mycaml;
  Alias.utop;
]

let gen_recursive_aliases ~dir =
  unignored_subdirs ~dir *>>| fun subdirs ->
  List.map recursive_alias_list ~f:(fun make_alias ->
    Rule.alias (make_alias ~dir) (
      List.map subdirs ~f:(fun subdir -> Dep.alias (make_alias ~dir:subdir))
    )
  )

(*----------------------------------------------------------------------
 Translation OMakefile -> jbuild.gen
----------------------------------------------------------------------*)

let generate_jbuild_from_omakefile ~dir =
  let gen_jbuild = root_relative "bin/gen-jbuild.sh" in
  let target = relative ~dir "jbuild.gen" in
  let deps = [
    Dep.path gen_jbuild;
    Dep.path (root_relative "bin/fake-OMakeroot");
    Dep.path (root_relative "bin/fixup-omake-rule-string.sh");
    Dep.path (relative ~dir "OMakefile");
    Dep.glob_change (glob_ml ~dir);
  ]
  in
  Rule.create1 ~targets:[target] ~deps
    ~action:(Action.shell ~dir ~prog:(dotdot ~dir gen_jbuild)
               ~args:["jbuild.gen"]
    )

let setup_jbuild_generated =
  Scheme.create ~tag:"jbuild.gen" (fun ~dir ->
    let omakefile = relative ~dir "OMakefile" in
    Dep.file_exists omakefile *>>| function
    | true -> [generate_jbuild_from_omakefile ~dir]
    | false -> []
  )

(*----------------------------------------------------------------------
 Libmap
----------------------------------------------------------------------*)

module Libmap : sig

  type t
  val create_exn : (string * Path.t) list -> t
  val look_exn : t -> string -> Path.t
  val exists : t -> string -> bool

end = struct

  type t = {
    find : (string -> Path.t option);
  }

  let create_exn xs =
    let h = Hashtbl.Poly.create () in
    List.iter xs ~f:(fun (lib, where) ->
      match Hashtbl.add h ~key:lib ~data:where with
      | `Ok -> ()
      | `Duplicate ->
        let previous_loc = Hashtbl.find_exn h lib in
        failwithf !"Duplicate definition of library %s in %{Path} and %{Path}"
          lib previous_loc where ()
    );
    { find = fun lib -> Hashtbl.Poly.find h lib }

  let look_exn t name =
    match t.find name with
    | None -> failwithf "dont know about library: %s" name ()
    | Some x -> x

  let exists t name =
    Option.is_some (t.find name)

end

(*----------------------------------------------------------------------
 prefix
----------------------------------------------------------------------*)

let library_prefix_sep = "__" (* double underscore for clarity *)
let prefix_for_lib ~libname = libname ^ library_prefix_sep

let prefix_name ~libname ~name =
  assert (String. (not (name = "")));
  if Wrap_style.packing
  then name (* no prefixing done when packing *)
  else
    if not (name = libname)
    then
      prefix_for_lib ~libname ^ String.capitalize name
    else
      name

let unprefix_name ~libname name =
  assert (String. (not (name = "")));
  if Wrap_style.packing
  then name (* no prefixing done when packing *)
  else
    let prefix = prefix_for_lib ~libname in
    String.uncapitalize (String.chop_prefix_exn ~prefix name)


let map_prefix_name ~libname names =
  List.map names ~f:(fun name -> prefix_name ~libname ~name)


let prefix_or_pack_args ~wrapped ~libname ~name =
  if Wrap_style.packing
  then
    if wrapped
    then ["-for-pack"; String.capitalize libname]
    else []
  else
    if not (name = libname)
    then ["-o"; prefix_for_lib ~libname ^ String.capitalize name]
    else []

let dash_ml_wrap = ".ml-wrap"

(*----------------------------------------------------------------------
 replacement for -pack (no-pack)...
----------------------------------------------------------------------*)

let gen_alias_file ~dir ~libname ~modules ~target =
  let ml_text =
    String.concat (List.map modules ~f:(fun name ->
      let prefixed_name = prefix_name ~libname ~name in
      sprintf "module %s = %s\n"
        (String.capitalize name)
        (String.capitalize prefixed_name)
    ))
  in
  Rule.create ~targets:[target] (
    return (
      Bash.action ~dir [
        echo_string ml_text ~target:(basename target)
      ]
    )
  )

let gen_library_wrap ~dir ~libname ~modules =
  let target = relative ~dir (libname ^ dash_ml_wrap) in
  gen_alias_file ~dir ~libname ~modules ~target


let knot_base = "implicit_aliases"

let gen_knot_file ~dir ~libname ~modules =
  let prefixed_name = prefix_name ~libname ~name:knot_base in
  let mli = suffixed ~dir prefixed_name ".mli-gen" in
  let target = mli in
  gen_alias_file ~dir ~libname ~modules ~target


let compile_knot ~libname ~dir =
  let prefixed_name = prefix_name ~libname ~name:knot_base in
  let mli_gen = suffixed ~dir prefixed_name ".mli-gen" in
  let cmi = suffixed ~dir prefixed_name ".cmi" in
  let targets = [cmi] in
  Rule.create ~targets (
    Dep.path mli_gen *>>| fun () ->
    Action.shell
      ~dir
      ~prog:ocamlopt_prog
      ~args:(List.concat [
        ["-w"; "@a-49"];
        (*["-no-alias-deps"];*)
        ["-o"; prefix_for_lib ~libname ^ String.capitalize knot_base];
        [ "-c";];
        ["-intf"];
        [basename mli_gen];
      ])
  )

let knot_rules ~dir ~libname ~modules =
  [
    gen_knot_file ~dir ~libname ~modules;
    compile_knot ~libname ~dir;
  ]


(*----------------------------------------------------------------------
 external lib locations - better named: builtin
----------------------------------------------------------------------*)

let external_locations = [
  "dynlink", ocaml_where;
  "str", ocaml_where;
]

let is_external_lib ~lib =
  List.mem (List.map ~f:fst external_locations) lib

let find_external_library =
  let h = Hashtbl.Poly.create () in
  List.iter external_locations ~f:(fun (key,where) ->
    match (Hashtbl.add h ~key ~data:where) with
    | `Ok -> ()
    | `Duplicate -> failwithf "duplicate location for external library: %s" key ()
  );
  fun key ->
    match Hashtbl.Poly.find h key with
    | None -> failwithf "dont know about external library: %s" key ()
    | Some x -> x

let link_to_abs_remote ~remote ~local =
  let dir = dirname local in
  Rule.create1 ~targets:[local] ~deps:[]
    ~action:(
      Bash.action ~dir [
        bash1 "rm" ["-f"; basename local];
        bash1 "ln" ["-s"; remote; basename local];
      ]
    )

let external_liblink_rules ~dir ~lib =
  let suffixes = [".cmi";".cma";".a";".cmxa";".cmx"] in
  List.map suffixes ~f:(fun suf ->
    let name = lib ^ suf in
    let remote = find_external_library lib ^ "/" ^ name in
    let local = relative ~dir name  in
    link_to_abs_remote ~remote ~local
  )
    @
  List.map [".stub.names"; ".libdeps"; ".interface.deps"] ~f:(fun suf ->
    let name = lib ^ suf in
    let target = relative ~dir name in
    write_names_rule [] ~target
  )
    @
    [
      Rule.alias (Alias.submodule_cmis ~dir) []
    ]


(*----------------------------------------------------------------------
 liblinks
----------------------------------------------------------------------*)

let the_root_lib_dir = root_relative "lib"

let link_to_remote ~remote ~local ~x_hack_deps =
  let dir = dirname local in
  let deps = [Dep.path remote] @ x_hack_deps in
  Rule.create ~targets:[local] (
    Dep.all_unit deps *>>= fun () ->
    return (
      Bash.action ~dir [
        bash1 "rm" ["-f"; basename local];
        bash1 "ln" ["-s"; dotdot ~dir remote; basename local];
      ]
    )
  )

let pack_order_file ~dir ~libname =
  relative ~dir (libname ^ ".pack-order")

module LL : sig

  val liblink_dir : lib:string -> Path.t
  val liblink_refname : lib:string -> name:string -> Path.t
  val liblink_deps : libs:string list -> suffixes:string list -> unit Dep.t list
  val liblink_default : libs:string list -> unit Dep.t list
  val liblink_submodule_cmis : libs:string list -> unit Dep.t list
  val liblinks_stubs : libs:string list -> unit Dep.t
  val liblink_includes : dir:Path.t -> libs:string list -> string list
  val liblink_interfaces : lib:string -> string list Dep.t

  val rules : dir:Path.t -> Libmap.t -> Rule.t list Dep.t

end = struct

  let liblink_dir ~lib = relative ~dir:the_root_lib_dir lib
  let liblink_refname ~lib ~name = relative ~dir:(liblink_dir ~lib) name
  let liblink_ref ~lib ~suf = liblink_refname ~lib ~name:(lib ^ suf)

  let liblink_deps ~libs ~suffixes =
    List.concat_map libs ~f:(fun lib ->
      List.map suffixes ~f:(fun suf ->
        Dep.path (liblink_ref ~lib ~suf)
      )
    )

  let liblink_default ~libs =
    List.map libs ~f:(fun lib ->
      Dep.alias (Alias.default ~dir:(liblink_dir ~lib))
    )

  let liblink_submodule_cmis ~libs =
    List.map libs ~f:(fun lib ->
      Dep.alias (Alias.submodule_cmis ~dir:(liblink_dir ~lib))
    )

  let liblink_interfaces ~lib =
    file_words (liblink_ref ~lib ~suf:".interface.deps")

  let liblinks_stubs ~libs =
    Dep.all_unit (
      List.map libs ~f:(fun lib ->
        file_words (liblink_ref ~lib ~suf:".stub.names") *>>= fun stubs ->
        Dep.all_unit (List.map stubs ~f:(fun stub ->
          Dep.path (liblink_refname ~lib ~name:("lib" ^ stub ^ "_stubs.a"))
        ))
      ))

  let liblink_includes ~dir ~libs =
    List.concat_map libs ~f:(fun lib ->
      ["-I"; dotdot ~dir (liblink_dir ~lib)]
    )

  let libname_from_liblink_path ~dir =
    match String.rsplit2 (Path.to_string dir) ~on:'/' with
    | None -> failwith "libname_from_liblink_path"
    | Some (_,name) -> name

  let make_liblink_rule ~remote_dir ~lib ~name =
    let remote = relative ~dir:remote_dir name in
    let local = liblink_refname ~lib ~name in
    link_to_remote ~remote ~local ~x_hack_deps:[]

  let make_liblink_rules ~remote_dir ~lib ~names =
    List.map names ~f:(fun name -> make_liblink_rule ~remote_dir ~lib ~name)

  let make_liblink_stubs_rules ~remote_dir ~lib =
    let name1 = lib^".stub.names" in
  (*let name1 = "stub.names" in*)
    let name2 = sprintf "lib%s_stubs.a" lib in
    let remote1 = relative ~dir:remote_dir name1 in
    let local1 = liblink_refname ~lib ~name:name1 in
    let remote2 = relative ~dir:remote_dir name2 in
    let local2 = liblink_refname ~lib ~name:name2 in
  (* hack - make dep on _stubs, depen on lib%s_stubs.a *)
    [
      link_to_remote ~remote:remote1 ~local:local1 ~x_hack_deps:[Dep.path local2];
      link_to_remote ~remote:remote2 ~local:local2 ~x_hack_deps:[];
    ]

  let the_liblink_suffixes =
    (if x_library_inlining then [".cmx"] else [])
    @ [".cmxs"; ".cmi";".cmo";".cma";".cmxa";".a";".libdeps";".interface.deps"]

  let additional_liblink_names ~lib =
    match lib with
    | "typehash" -> [ "dll_typehash_stubs.so"; "typehash_dynload.cma" ]
    | _ -> []

  let liblink_rules1 ~remote_dir ~lib =
    let names1 = List.map the_liblink_suffixes ~f:(fun suf -> lib ^ suf) in
    let additional_names = additional_liblink_names ~lib in
    let rules1 = make_liblink_rules ~remote_dir ~lib ~names:(names1 @ additional_names) in
    let rules2 = make_liblink_stubs_rules ~remote_dir ~lib in
    let rules = rules1 @ rules2 in
    rules

  let rules ~dir libmap =
    let lib = libname_from_liblink_path ~dir in
    if is_external_lib ~lib
    then
      return (
        Rule.default ~dir:(liblink_dir ~lib) []
        :: external_liblink_rules ~dir ~lib
      )
    else
      let find_library = Libmap.look_exn libmap in
      let remote_dir = find_library lib in
      let remote_modules_file = pack_order_file ~dir:remote_dir ~libname:lib in
      file_words remote_modules_file *>>= fun modules ->
      (* dont need the module names to be ordered; but it doesn't matter *)
      let submodule_cmi_link_rules =
        if Wrap_style.packing
        then []
        else
          List.map modules ~f:(fun mod_ ->
            let name = mod_ ^".cmi" in
            make_liblink_rule ~remote_dir ~lib ~name
          )
      in
      let submodule_cmi_alias_rule =
        if Wrap_style.packing
        then
          Rule.alias (Alias.submodule_cmis ~dir) []
        else
          Rule.alias (Alias.submodule_cmis ~dir) (
            List.map modules ~f:(fun mod_ ->
              let name = mod_ ^".cmi" in
              Dep.path (relative ~dir name)
            )
          )
      in
      let default_rule =
        (* Setup a .DEFAULT alias in the lib-links directory, to indirect to the
           .lib_artifacts alias in the directory where the library code is actually found.
           This .DEFAULT alias is for the use of remote references to the library (i.e. when
           linking .exe) to force the entire default build, including inline_tests *)
        Rule.default ~dir:(liblink_dir ~lib) [
          (*Dep.alias (Alias.submodule_cmis ~dir);*)
          Dep.alias (Alias.lib_artifacts ~dir:remote_dir);
        ]
      in
      return (
        List.concat [
          liblink_rules1 ~remote_dir ~lib;
          submodule_cmi_link_rules;
          [submodule_cmi_alias_rule;
           default_rule];
        ]
      )

end

(*----------------------------------------------------------------------
 directory context (dc)
----------------------------------------------------------------------*)

module DC = struct
  type t = {
    dir : Path.t;
    ocamllibflags : string list;
    merlinflags : string list;
    ocamlflags : string list;
    ocamlcflags : string list;
    ocamloptflags : string list;
    configured_libs : ([ `Name of string ] * [ `Inline_tests_deps of string list ]) list;
    xlibnames : string list; (* broader set: from preprocessor/library/libraryX configs
                                and includes "bin" if there is an executables config *)

    ocaml_plugin_libraries : (string -> string list option);
    no_mycaml_alias : bool;
    no_utop_alias : bool;
    libmap : Libmap.t;
    autogen : string list;
    impls : string list;
    intfs : string list;
    impl_is_buildable : (string -> bool);
    intf_is_buildable : (string -> bool);
  }
end

let libdeps_for dc name =
  let libmap = dc.DC.libmap in
  let path = relative ~dir:(Libmap.look_exn libmap name) (name ^ ".libdeps") in
  file_words path *>>| fun libs ->
  libs @ [ name ]

(*----------------------------------------------------------------------
 Dfile_deps
----------------------------------------------------------------------*)

module Dfile_deps : sig

  val local_dependencies_ml : DC.t -> libname:string -> dfile:Path.t -> unit Dep.t
  val local_dependencies_mli : dfile:Path.t -> unit Dep.t

end = struct

  let local_dependencies_ml dc ~libname ~dfile =
    let dir = dirname dfile in
    let {DC.impl_is_buildable;_} = dc in
    file_words dfile *>>= fun names ->
    Dep.all_unit (
      List.concat_map names ~f:(fun prefixed_name ->
        let suffixes =
          let name = unprefix_name ~libname prefixed_name in
          let dep_cmx = impl_is_buildable name in
          (*if not dep_cmx then (
            message "impl is not buildable for module: %s, in: %s"
              name (Path.to_string dir);
          );*)
          if dep_cmx
          then [".cmi"; ".cmx"]
          else [".cmi"]
        in
        List.map suffixes ~f:(fun suf ->
          Dep.path (suffixed ~dir prefixed_name suf)
        )
      )
    )

  let local_dependencies_mli ~dfile =
    let dir = dirname dfile in
    file_words dfile *>>= fun names ->
    Dep.all_unit (
      List.concat_map names ~f:(fun name ->
        let suffixes = [".cmi"] in
        List.map suffixes ~f:(fun suf ->
          Dep.path (suffixed ~dir name suf)
        )
      )
    )

end

(*----------------------------------------------------------------------
 objdeps/libdeps
----------------------------------------------------------------------*)

let remove_dups_preserve_order xs =
  let set = String.Hash_set.create ()  in
  let rec loop acc = function
    | [] -> List.rev acc
    | x::xs ->
      if Hash_set.mem set x
      then loop acc xs
      else (Hash_set.add set x; loop (x::acc) xs)
  in
  loop [] xs

let remove_dups_and_sort xs =
  String.Set.to_list (String.Set.of_list xs)

let gen_transitive_deps : (
  dir : Path.t ->
  one_step : string list Dep.t ->
  dep_wrt_dir : Path.t ->
  template : (string -> string) ->
  target : string ->
  Rule.t
) =
  fun ~dir ~one_step ~dep_wrt_dir ~template ~target ->
    let path_of_name = (fun name -> relative ~dir:dep_wrt_dir (template name)) in
    let target = (relative ~dir target) in
    Rule.create ~targets:[target] (
      one_step *>>= fun names1 ->
      let paths = List.map names1 ~f:path_of_name in
      Dep.List.concat_map paths ~f:file_words *>>| fun namesM ->
      let names = remove_dups_preserve_order (namesM @ names1) in
      write_string_action (String.concat ~sep:" " names) ~target
    )

let gen_objdeps ~libname ~dir name =
  let prefixed_name = prefix_name ~libname ~name in
  let suf = ".objdeps" in
  gen_transitive_deps ~dir
    ~one_step:(file_words (relative ~dir (prefixed_name^".ml.d")))
    ~dep_wrt_dir:dir
    ~template:(fun x -> x^suf)
    ~target:(prefixed_name^suf)

let need_ounit ~libname =
  match libname with
  | "pa_bench"
  | "pa_bench_lib"
  | "pa_ounit"
  | "pa_ounit_lib"
  | "oUnit"
    -> false
  | _ -> true

let get_inferred_1step_deps ~dir ~libname =
  file_words (relative ~dir (libname ^ ".inferred-1step.deps")) *>>| fun libs ->
  if need_ounit ~libname then "pa_bench_lib" :: "pa_ounit_lib" :: libs else libs

module Objinfo : sig

  type t
  val interface_names : t -> string list
  val parse : string -> t

end = struct

  type crc = Crc of string | Weak
  type name = string
  type import = crc * name
  type t = {
    interfaces : import list;
  } with fields

  let interface_names t = List.map t.interfaces ~f:snd

  let read_import_line ~line =
    match String.split line ~on:'\t' with
      ["";crc;name] ->
        assert (String.length crc = 32);
        let is_weak = crc.[0] = '-' in
        let crc =
          if is_weak then (
            assert (List.for_all (String.to_list crc) ~f:(fun x -> Char.(x = '-')));
            Weak
          ) else Crc crc
        in
        (crc,name)
    | _ -> failwith "read_import_line"

  let rec skip_to_interface_banner = function
    | [] -> failwith "skip_to_interface_banner"
    | line::lines ->
      if String.(line = "Interfaces imported:")
      then lines
      else skip_to_interface_banner lines

  let rec collect_interfaces acc = function
    | [] -> List.rev acc
    | line::lines -> collect_interfaces (read_import_line ~line :: acc) lines

  let parse stdout =
    let lines = lines_of_string stdout in
    let lines = skip_to_interface_banner lines in
    let interfaces = collect_interfaces [] lines in
    { interfaces }

end

let gen_interface_deps_from_objinfo dc ~dir ~libname =
  let self = libname in
  let in_libmap x = Libmap.exists dc.DC.libmap x in
  let target = suffixed ~dir libname ".interface.deps" in
  Rule.create ~targets:[target] (
    let modules_file = pack_order_file ~dir ~libname in
    file_words modules_file *>>= fun modules ->
    let names = libname :: modules in
    Dep.List.concat_map names ~f:(fun name ->
      let cmi = suffixed ~dir name ".cmi" in
      Dep.action_stdout (
        Dep.path cmi *>>| fun () ->
        bashf ~dir "ocamlobjinfo %s" (basename cmi)
      ) *>>| fun stdout ->
      let obi = Objinfo.parse stdout in
      let words = Objinfo.interface_names obi in
      let words = List.map ~f:String.uncapitalize words in
      words
    ) *>>| fun words ->
    let words = List.filter words ~f:(fun x -> in_libmap x && not (x = self)) in
    let words = remove_dups_preserve_order words in
    bashf ~dir "echo %s > %s" (String.concat ~sep:" " words) (basename target)
  )

let gen_libdeps dc ~dir ~libs name =
  [
    gen_interface_deps_from_objinfo dc ~dir ~libname:name;

    gen_transitive_deps ~dir
      ~one_step:(return libs)
      ~dep_wrt_dir:the_root_lib_dir
      ~template:(fun x -> sprintf "%s/%s.interface.deps" x x)
      ~target:(name ^ ".inferred-1step.deps");

    gen_transitive_deps ~dir
      ~one_step:(file_words (relative ~dir (name ^ ".inferred-1step.deps")))
      ~dep_wrt_dir:the_root_lib_dir
      ~template:(fun x -> sprintf "%s/%s.libdeps" x x)
      ~target:(name ^ ".libdeps");

    Rule.alias (Alias.libdeps ~dir) [
      Dep.path (suffixed ~dir name ".libdeps")
    ];
  ]


(*----------------------------------------------------------------------
 merlin rules
----------------------------------------------------------------------*)

let merlin_1step_libs dc ~dir =
  Dep.List.concat_map dc.DC.xlibnames ~f:(fun libname ->
    get_inferred_1step_deps ~dir ~libname
  ) *>>= fun libs ->
  let libs = remove_dups_preserve_order libs in
  let libs = List.filter libs ~f:(fun lib -> not (is_external_lib ~lib)) in
  return libs

let merlin_rules dc ~dir =
  let target = relative ~dir ".merlin" in
   (* don't create .merlin files in all the directories when we don't need them *)
  if dc.DC.xlibnames = [] && dir <> Path.the_root then [] else
  [
    Rule.create ~targets:[target] (
      merlin_1step_libs dc ~dir *>>| fun libs ->
      let libmap = dc.DC.libmap in
      let find_library = Libmap.look_exn libmap in
      let dot_merlin_contents =
        String.concat ~sep:"\n" (
          [
            "EXT here";
            "EXT ounit";
            "EXT nonrec";
            "FLG " ^ (String.concat ~sep:" " dc.DC.merlinflags);
            "B .";
          ] @
          (* Now we tell merlin to load cmi from the lib dir to avoid clashes between
             cmis from different libs (before packing), but we tell merlin to load cmt
             from the source dir since they aren't installed, and merlin has some
             heuristic to try to figure out the right cmt to load. *)
          List.concat_map libs ~f:(fun lib ->
            let cmt = dotdot ~dir (find_library lib) in
            let cmi = dotdot ~dir (LL.liblink_dir ~lib) in
            [sprintf "CMT %s" cmt;
             sprintf "CMI %s" cmi]
          )
        )
      in
      write_string_action dot_merlin_contents ~target
    );
    Rule.alias (Alias.lib_artifacts ~dir) [Dep.path target];
    (* Sadly names given on the command line which begin with a dot (i.e. ".merlin") are
       currently always interpreted as a reference to an alias. Workaround this problem
       for the specifc case of .merlin, by creating an alias .merlin *)
    Rule.alias (Alias.merlin ~dir) [Dep.path target];
  ]

(*----------------------------------------------------------------------
 * Jbuild (config variants)
----------------------------------------------------------------------*)

module Names_spec = struct
  type t =
  | List of string list
  | All
  with sexp
end

module Preprocess_kind = struct
  type t = [ `command of string | `pps of string list ]
  with of_sexp
end

module Preprocess_spec = struct
  type t = Preprocess_kind.t * Names_spec.t
  with of_sexp
end

let preprocess_default = [
  (`pps pa_jane, Names_spec.All)
]

module Preprocessor_conf = struct
  type t = {
    name : string;
    libraries : string sexp_list;
    preprocess : Preprocess_spec.t list with default(preprocess_default);
  } with of_sexp, fields
end

module Dep_conf = struct

  type t =
    | File of string
    | Files_recursively_in of string
  with of_sexp

  let t_of_sexp = function
    | Sexp.Atom s -> File s
    | Sexp.List _ as sexp -> t_of_sexp sexp

  let to_depends ~dir = function
    | File s -> Dep.path (Path.relative_or_absolute ~dir s)
    | Files_recursively_in spec_dir ->
      (* Add deps on the recursive list of files, *and* their contents. Only works
         if the directory only contains source files, otherwise we wouldn't depend
         on what is buildable but not built. *)
      deep_unignored_subdirs ~dir:(relative ~dir spec_dir) *>>= fun dirs ->
      Dep.all_unit (
        List.map dirs ~f:(fun dir ->
          Dep.glob_listing (Glob.create ~dir ~kinds:[`File] "*") *>>= fun paths ->
          Dep.all_unit (List.map paths ~f:Dep.source_if_it_exists)
        )
      )

  let list_to_depends ~dir ts =
    List.map ts ~f:(to_depends ~dir)

  let only_plain_file = function
    | File s -> Some s
    | Files_recursively_in _ -> None
end

module StubsX_conf = struct
  type t = {
    deps : Dep_conf.t list;
    action : string;
  } with of_sexp,fields
end

module Rule_conf = struct

  type t = {
    targets : string list;
    deps : Dep_conf.t list;
    action : string;
  } with of_sexp,fields

end

module Alias_conf = struct

  type t = {
    name : string;
    deps : Dep_conf.t list;
    action : string sexp_option;
  } with of_sexp,fields

end

module Compile_c_conf = struct

  type t = {
    names : string list;
    extra_cflags : string sexp_list;
    replace_cflags : string sexp_list;
  } with of_sexp

end

module LibraryX_conf = struct (* external library setup *)

  type t = {
    name : string;
    targets : string list; (* with default([]);*)
    deps : Dep_conf.t list; (* with default([]); *)
    action : string;
  } with of_sexp, fields

end

module Embed_conf = struct
  type t = {
    names : string list;
    libraries : string sexp_list;
    cmis : string sexp_list;
    pps : string sexp_list;
  } with sexp
end

(*----------------------------------------------------------------------
 user flag control - replace/extend
----------------------------------------------------------------------*)

let combine_replace_extra ~tag ~replace ~extra xs =
  match replace,extra with
  | [],[] -> xs
  | [],_::_ -> xs @ extra
  | _::_,[] -> replace
  | _::_,_::_ ->
    failwithf "%s: both extra_%s & replace_%s are defined" tag tag tag ()

(*----------------------------------------------------------------------
 Rule_conf, Alias_conf (user rules)
----------------------------------------------------------------------*)

let prefixed_includes ~dir =
  let includes = [ "."; ocaml_where; dotdot ~dir (root_relative "include") ] in
  List.concat_map includes ~f:(fun path -> ["-I"; path])

let expand_vars_in_rule dc ~dir ~targets ~deps ~cflags orig =
  let {DC. ocamlflags; _} = dc in
  let dep_exn name = function
    | Some dep -> dep
    | None -> failwithf "Cannot use ${%s} with files_recursively_in" name ()
  in
  let lookup ~var_name =
    match var_name with
    | "@" -> Some (String.concat ~sep:" " targets)
    | "<" -> Some (match deps with [] -> "" | dep1::_ -> dep_exn var_name dep1)
    | "^" ->
      let deps = List.map deps ~f:(dep_exn var_name) in
      Some (String.concat ~sep:" " deps)
    | "CFLAGS"  -> Some (String.concat ~sep:" " cflags)
    | "OCAMLFLAGS"  -> Some (String.concat ~sep:" " ocamlflags)
    | "PREFIXED_INCLUDES"  -> Some (String.concat ~sep:" " (prefixed_includes ~dir))
    | "ROOT" -> Some (dotdot ~dir Path.the_root)
    | _ -> root_var_lookup ~var_name
  in
  expand_dollar_vars ~lookup orig

let expanded_action dc ~dir ~targets ~cflags deps action =
  let command_string =
    let deps = List.map deps ~f:Dep_conf.only_plain_file in
    expand_vars_in_rule dc ~dir ~targets ~deps ~cflags action
  in
  bash ~dir command_string

let rule_conf_to_rule dc ~dir ~cflags conf =
  let {Rule_conf. targets; deps; action} = conf in
  let action = expanded_action dc ~dir ~targets ~cflags deps action in
  Rule.create1
    ~targets:(List.map targets ~f:(relative ~dir))
    ~deps:(Dep_conf.list_to_depends ~dir deps)
    ~action

let alias_conf_to_rule dc ~dir ~cflags conf =
  let {Alias_conf. name; deps; action} = conf in
  let action = Option.map action ~f:(expanded_action dc ~dir ~targets:[] ~cflags deps) in
  let deps = Dep_conf.list_to_depends ~dir deps in
  let deps =
    match action with
    | None -> deps
    | Some action -> [Dep.action (Dep.all_unit deps *>>| fun () -> action)]
  in
  Rule.alias (Alias.create ~dir name) deps

(*----------------------------------------------------------------------
 libraryX_rules
----------------------------------------------------------------------*)

let c_stubs ~dir ~libname ~stub_names =
  let basename = libname ^ ".stub.names" in
  let target = relative ~dir basename in
  write_names_rule stub_names ~target

let libraryX_rules dc ~dir ~cflags conf =
  let { LibraryX_conf. name; targets; deps; action; } = conf in
  let rule =
    rule_conf_to_rule dc ~dir ~cflags
      { Rule_conf. targets; deps; action; }
  in
  let targets = List.map targets ~f:(relative ~dir) in
  let default_rule =
    Rule.default ~dir (List.map targets ~f:Dep.path)
  in
  let stub_names = [name] in
  let stubs_rule = c_stubs ~dir ~libname:name ~stub_names in
  [
    rule;
    default_rule;
    stubs_rule;
  ]

(*----------------------------------------------------------------------
 c/cxx compilation
----------------------------------------------------------------------*)

let compile_c_1 ~cflags ~autogen ~dir name =
  let deps =
    List.map ~f:Dep.path [relative ~dir (name ^ ".c")]
    @ List.map autogen ~f:Dep.path in
  let targets = [relative ~dir (name ^ ".o")] in
  let args =
    cflags
    @ prefixed_includes ~dir
    @ ["-c"; name ^ ".c"; "-o"; name ^ ".o"]
  in
  Rule.create1 ~deps ~targets ~action:(
    Action.shell ~dir ~prog:cc_prog ~args
  )

let compile_c ~cflags ~autogen ~dir names =
  List.map names ~f:(fun name ->
    compile_c_1 ~cflags ~autogen ~dir name
  )

let compile_cxx ~cxxflags ~autogen ~dir names ~cxx_suf =
  List.map names ~f:(fun name ->
    let deps =
      List.map ~f:Dep.path [relative ~dir (name ^ cxx_suf)]
      @ List.map autogen ~f:Dep.path in
    let targets = [relative ~dir (name ^ ".o")] in
    let args =
      cxxflags
      @ prefixed_includes ~dir
      @ ["-c"; name ^ cxx_suf; "-o"; name ^ ".o"]
    in
    Rule.create1 ~deps ~targets ~action:(
      Action.shell ~dir ~prog:cxx_prog ~args
    )
  )

let static_archive_c ~dir ~o_names ~target =
  let o_files = List.map o_names ~f:(fun file -> file ^ ".o") in
  let deps = List.map ~f:(fun x -> Dep.path (relative ~dir x)) o_files in
  Rule.create1 ~deps ~targets:[relative ~dir target]
    ~action:(Bash.action ~dir [
      Bash.create ~prog:"rm" ~args:["-f"; target] ~target:None;
      Bash.create ~prog:"ar" ~args:(["Drc"; target] @ o_files) ~target:None;
      Bash.create ~prog:"ranlib" ~args:[target] ~target:None;
    ])

let user_configured_compile_c_rules ~dir conf =
  let { Compile_c_conf.names;
        Compile_c_conf.extra_cflags;
        Compile_c_conf.replace_cflags;
      } = conf
  in
  let cflags =
    combine_replace_extra
      ~tag:"cflags"
      ~replace:replace_cflags
      ~extra:extra_cflags
      default_cflags
  in
  let autogen = [] in (* ? *)
  List.map names ~f:(fun name ->
    compile_c_1 ~cflags ~autogen ~dir name
  )

(*----------------------------------------------------------------------
 ocamllex/ocamlyacc
----------------------------------------------------------------------*)

let ocamllex_rule ~dir name =
  let suf x = suffixed ~dir name x in
  let ml = suf ".ml" in
  let mll = suf ".mll" in
  Rule.create1
    ~deps:[ocaml_bin_dep; Dep.path mll]
    ~targets:[ml]
    ~action:(
      Action.shell ~dir ~prog:"ocamllex" ~args:["-q"; basename mll]
    )

let ocamlyacc_rule ~dir name =
  let suf x = suffixed ~dir name x in
  let ml = suf ".ml" in
  let mli = suf ".mli" in
  let mly = suf ".mly" in
  Rule.create1
    ~deps:[Dep.path mly ; ocaml_bin_dep]
    ~targets:[ml;mli]
    ~action:(
      Action.shell ~dir ~prog:"ocamlyacc" ~args:["-q"; basename mly]
    )


(*----------------------------------------------------------------------
 ML compilation context (mc)
----------------------------------------------------------------------*)

module MC = struct
  type mc = {
    dc : DC.t;
    dir : Path.t;
    libname : string;
    pp_libs : string list;
    pp_deps : unit Dep.t list;
    pp_com_opt : string option;
    exists_mli : bool;
    wrapped : bool;
    must_be_sharable : bool;
  }
end


(*----------------------------------------------------------------------
 ocamldep / .d files
----------------------------------------------------------------------*)

let gen_dfile mc ~name ~suf ~dsuf =
  let {MC. dc; dir; libname; pp_deps; pp_com_opt; _ } = mc in
  let source = suffixed ~dir name suf in
  let prefixed_name = prefix_name ~libname ~name in
  let dfile = suffixed ~dir prefixed_name dsuf in
  let targets = [dfile] in
  let is_ml_wrap = String.(suf = dash_ml_wrap) in
  Rule.create ~targets
    (let pp_opt =
      match pp_com_opt with
      | None -> []
      | Some pp_com -> ["-pp"; pp_com]
     in
     Dep.action_stdout
       (Dep.all_unit (ocaml_bin_dep :: Dep.path source :: pp_deps)
        *>>| fun () -> Action.shell ~dir ~prog:ocamldep_prog
                         ~args:("-modules" :: pp_opt @
                                   (if is_ml_wrap then ["-impl"] else []) @
                                   [basename source]))
     *>>| fun output ->
     let is_actual_dep =
       let self = prefix_name ~libname ~name in
       let actual_modules =
         let names = dc.DC.impls @ dc.DC.intfs in
         let names = map_prefix_name ~libname names in
         String.Set.of_list names
       in
       fun name ->
         not (name = self) &&
         Set.mem actual_modules name
     in
     let potential_dependencies =
       output
       |> String.lsplit2_exn ~on:':'
       |> snd
       |> words_of_string
       |> List.sort ~cmp:String.compare
       (* can depend on both a.ml and A.ml, depending on which one exists *)
       |> List.concat_map ~f:(fun x -> [x; String.uncapitalize x])
     in
     let potential_dependencies =
       if is_ml_wrap
       (* special case for .ml-wrap:
          Don't add the prefix because the prefxi is explicit in the generated ml *)
       then potential_dependencies
       else map_prefix_name ~libname potential_dependencies
     in
     let dependencies = List.filter potential_dependencies ~f:is_actual_dep in
     let dir = dirname dfile in
     bashf ~dir "echo %s > %s"
       (String.concat ~sep:" " dependencies)
       (basename dfile))

(*----------------------------------------------------------------------
 ocaml compilation
----------------------------------------------------------------------*)

let compile_mli mc ~name =
  let {MC. dc; dir; libname; pp_libs; pp_deps; pp_com_opt; wrapped;_ } = mc in
  let {DC. ocamlflags; ocamlcflags; _} = dc in
  let prefix_args = prefix_or_pack_args ~wrapped ~libname ~name in
  let prefixed_name = prefix_name ~libname ~name in
  let flags = ocamlflags @ ocamlcflags in
  let dfile = suffixed ~dir prefixed_name ".mli.d" in
  let mli = suffixed ~dir name ".mli" in
  let cmi = suffixed ~dir prefixed_name ".cmi" in
  let targets = [cmi] in
  let targets =
    if (List.mem flags "-bin-annot")
    then
      let cmti = suffixed ~dir prefixed_name ".cmti" in
      targets @ [cmti]
    else targets
  in
  Rule.create ~targets (
    get_inferred_1step_deps ~dir ~libname *>>= fun libs ->
    let libs = pp_libs @ libs in
    let libdeps =
      LL.liblink_deps ~libs ~suffixes:[".cmi"]
      @ LL.liblink_submodule_cmis ~libs
    in
    let deps = [Dep.path mli; Dfile_deps.local_dependencies_mli ~dfile] @ pp_deps @ libdeps in
    let deps = ocaml_bin_dep :: deps in

    let deps,open_knot_args =
      if Wrap_style.packing
      then deps,[]
      else
        let knot_name = prefix_name ~libname ~name:knot_base in
        let knot_cmi = suffixed ~dir knot_name ".cmi" in
        let deps = Dep.path knot_cmi :: deps in
        let open_knot_args = ["-open"; String.capitalize knot_name] in
        deps,open_knot_args
    in

    Dep.all_unit deps *>>= fun () ->
    return (
      Action.shell
        ~dir
        ~prog:ocamlc_prog
        ~args:(List.concat [
          (* In .mli's, we disable warning 33: "Unused open statement".  This is a new
             warning reported by 4.01, and we would like to be able to work around it by
             using [open!] to squelch the warning.  But [open!] isn't properly supported
             by 4.01 yet.  So, we're leaving the warning off until a stock OCaml is
             released that supports [open!]. *)
          flags; ["-w"; "-33"];
          (match pp_com_opt with None -> [] | Some pp_com -> ["-pp"; pp_com]);
          LL.liblink_includes ~dir ~libs;
          prefix_args;
          (*["-no-alias-deps"];*)
          open_knot_args;
          [ "-c"; basename mli]
        ])
    )
  )

let extra_libs_for_mlname =
  function
  | "inline_benchmarks_runner" -> ["inline_benchmarks"]
  | _ -> []

let remove_nodynlink =
  List.filter ~f:(fun x -> x <> "-nodynlink")

let native_compile_ml mc ~name =
  let {MC. dc; dir; libname; pp_libs; pp_deps; pp_com_opt;
       wrapped; exists_mli;
       must_be_sharable; _ } = mc in
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let is_ml_wrap = wrapped && String.(name = libname) in
  let prefix_args = prefix_or_pack_args ~wrapped ~libname ~name in
  let prefixed_name = prefix_name ~libname ~name in
  let ml =
    if is_ml_wrap
    then suffixed ~dir name dash_ml_wrap
    else suffixed ~dir name ".ml"
  in
  let dfile = suffixed ~dir prefixed_name ".ml.d" in
  let o = suffixed ~dir prefixed_name ".o" in
  let cmx = suffixed ~dir prefixed_name ".cmx" in
  let cmi = suffixed ~dir prefixed_name ".cmi" in
  let targets =
    if exists_mli
    then [o;cmx]
    else [o;cmx;cmi]
  in
  let flags = ocamlflags @ ocamloptflags in
  let flags = if must_be_sharable then remove_nodynlink flags else flags in
  let targets =
    if (List.mem flags "-bin-annot")
    then
      let cmt = suffixed ~dir prefixed_name ".cmt" in
      targets @ [cmt]
    else targets
  in
  Rule.create ~targets (
    get_inferred_1step_deps ~dir ~libname *>>= fun libs ->
    let libs = remove_dups_preserve_order (pp_libs @ libs) in
    let libs = extra_libs_for_mlname name @ libs in
    let libdeps =
      let suffixes =
        if x_library_inlining
        then [".cmi"; ".cmx"]
        else [".cmi"]
      in
      LL.liblink_deps ~libs ~suffixes
      @ LL.liblink_submodule_cmis ~libs
    in
    let deps = [Dep.path ml; Dfile_deps.local_dependencies_ml dc ~libname ~dfile] @ pp_deps @ libdeps in
    let deps = if exists_mli then deps @ [Dep.path cmi] else deps in
    let deps = ocaml_bin_dep :: deps in

    let deps,open_knot_args =
      if Wrap_style.packing
      then deps,[]
      else
        let knot_name = prefix_name ~libname ~name:knot_base in
        let knot_cmi = suffixed ~dir knot_name ".cmi" in
        let deps = Dep.path knot_cmi :: deps in
        let open_knot_args = ["-open"; String.capitalize knot_name] in
        deps,open_knot_args
    in

    Dep.all_unit deps *>>= fun () ->
    return (
      Action.shell
        ~dir
        ~prog:ocamlopt_prog
        ~args:(List.concat [
          flags;
          (match pp_com_opt with None -> [] | Some pp_com -> ["-pp"; pp_com]);
          LL.liblink_includes ~dir ~libs;
          prefix_args;
          (*["-no-alias-deps"];*)
          open_knot_args;
          [ "-c";];
          (if is_ml_wrap then ["-impl"] else []);
          [basename ml];
        ])
    )
  )

let byte_compile_ml mc ~name =
  let {MC. dc; dir; libname; pp_libs; pp_deps; pp_com_opt;
       wrapped;
       exists_mli; _ } = mc in
  let {DC. ocamlflags; ocamlcflags; _} = dc in
  let is_ml_wrap = wrapped && String.(name = libname) in
  let prefix_args = prefix_or_pack_args ~wrapped ~libname ~name in
  let prefixed_name = prefix_name ~libname ~name in
  let ml =
    if is_ml_wrap
    then suffixed ~dir name dash_ml_wrap
    else suffixed ~dir name ".ml"
  in
  let dfile = suffixed ~dir prefixed_name ".ml.d" in
  let cmo = suffixed ~dir prefixed_name ".cmo" in
  (* If there is no .mli file, then the default behaviour of the byte-compiler (ocamlc) is
     to generate a .cmi file. We DONT want this, because our rules are setup so the .cmi
     is generated from the native-compiler.

     If the native & byte compilers run at the same time, we may get a corrupted .cmi file

     To prevent the byte-compiler from writing the .cmi we use a -o directive to get
     generated outputs with a different basename & then rename the output we want (the
     .cmo) back to the original basename.

     Even if there is an .mli, the same problem happens because of cmt files, so the
     bytecode compilation always happens in a subdirectory. *)
  let name_for_bc = name ^ ".for-byte-compile" in
  Rule.create ~targets:[cmo] (
    get_inferred_1step_deps ~dir ~libname *>>= fun libs ->
    let libs = remove_dups_preserve_order (pp_libs @ libs) in
    let libs = extra_libs_for_mlname name @ libs in
    let libdeps =
      LL.liblink_deps ~libs ~suffixes:[".cmi"]
      @ LL.liblink_submodule_cmis ~libs
    in
    let deps = [Dep.path ml; Dfile_deps.local_dependencies_ml dc ~libname ~dfile] @ pp_deps @ libdeps in
    let deps =
      deps @ (if exists_mli
        then
          let cmi = suffixed ~dir prefixed_name ".cmi" in
          [Dep.path cmi]
        else [])
    in
    let deps = ocaml_bin_dep :: deps in
    Dep.all_unit deps *>>= fun () ->
    return (
      Bash.action ~dir [
        bash1 ocamlc_prog (List.concat [
          ocamlflags; ocamlcflags;
          (match pp_com_opt with None -> [] | Some pp_com -> ["-pp"; pp_com]);
          LL.liblink_includes ~dir ~libs;
          prefix_args;
          [ "-o"; name_for_bc ^ ".cmo" ];
          [ "-c"; basename ml ];
        ]);
        bash1 "mv" [name_for_bc ^ ".cmo" ; basename cmo ];
      ]
    )
  )

let generate_pp mc ~name =
  let {MC. dir; pp_deps; pp_com_opt; exists_mli; _} = mc in
  match pp_com_opt with
  | None -> []
  | Some pp_com ->
    let pp_command =
      if String.is_prefix pp_com ~prefix:"camlp4" then
        fun source target ->
          sprintf "%s pr_o.cmo %s > %s" pp_com source target
      else
        fun source target ->
          sprintf "%s %s > %s" pp_com source target
    in
    let make_rule ~exists ~ext =
      if exists
      then
        let file = sprintf "%s.%s" name ext in
        let pp = sprintf "%s.%s.pp" name ext in
        (* generate the pp-file *)
        [Rule.create1
            ~deps:(Dep.path (relative ~dir file) :: pp_deps)
            ~targets:[relative ~dir pp]
            ~action:(bash ~dir (pp_command file pp));
         (* add to .pp alias *)
         Rule.alias (Alias.pp ~dir) [Dep.path (relative ~dir pp)];
        ]
      else
        []
    in
    make_rule ~exists:true ~ext:"ml" @ make_rule ~exists:exists_mli ~ext:"mli"

(*----------------------------------------------------------------------
 preprocessing ocaml file (pps)
----------------------------------------------------------------------*)

let libs_for_code_generated_by_pp = function
  | "pa_sexp_conv"      -> [ "sexplib" ]
  | "pa_bin_prot"       -> [ "bin_prot" ]
  | "pa_fields_conv"    -> [ "fieldslib" ]
  | "pa_variants_conv"  -> [ "variantslib" ]
  | "pa_typerep_conv"   -> [ "typerep_lib" ]
  | "pa_test"           -> [ "pa_test_lib" ]
  | _                   -> []


let eval_pps ~dir ~libname ~ounit ~macro names =
  let names = expand_predefined_pa_sets names in
  let names =
    if ounit then list_at_head ["pa_type_conv"; "pa_ounit"; "pa_bench"] names else names
  in
  let names = if macro then names @ ["pa_macro"] else names in
  let deps = ref [] in
  let path ~lib ~name =
    let path = LL.liblink_refname ~lib ~name in
    deps := Dep.path path :: !deps;
    dotdot ~dir path
  in
  let command =
    "camlp4o" ::
    List.concat_map names ~f:(function
      | "pa_macro" -> ["pa_macro.cmo"]
      | "pa_here" as lib ->
        [path ~lib ~name:(lib ^ ".cmo"); "-pa-here-dirname"; Path.to_string dir]
      | "pa_ounit" as lib ->
        [path ~lib ~name:(lib ^ ".cmo"); "-pa-ounit-lib"; libname]
      | "pa_typehash" as lib ->
        ignore (path ~lib:"typehash" ~name:"dll_typehash_stubs.so" : string); (* add a dep *)
        [path ~lib:"typehash" ~name:"typehash_dynload.cma"; path ~lib ~name:(lib ^ ".cmo")]
      | lib ->
        if String.is_prefix lib ~prefix:"-" then [lib]
        else [path ~lib ~name:(lib ^ ".cmo")])
  in
  let pp_libs = List.concat_map names ~f:libs_for_code_generated_by_pp in
  pp_libs, !deps, Some (String.concat command ~sep:" ")

let pa_cpp = "cpp -undef -traditional -Werror -DARCH_SIXTYFOUR"

let eval_preprocess_kind ~dir ~libname ~ounit ~macro kind =
  match kind with
  (*| `no_command -> [],None*)
  | `command "PA_CPP" -> [], [], Some pa_cpp
  | `command string -> [], [], Some string
  | `pps names -> eval_pps ~dir ~libname ~ounit ~macro names

let eval_names_spec ~dc names_spec =
  let {DC.intfs; intf_is_buildable; impls; impl_is_buildable; _} = dc in
  match names_spec with
  | Names_spec.All ->
    let filter_out_inline_modules l =
      List.filter l ~f:(function
        | "inline_tests_runner"
        | "inline_benchmarks_runner" -> false
        | _ -> true)
    in
    let intfs = filter_out_inline_modules intfs in
    let impls = filter_out_inline_modules impls in
    intfs,
    impls,
    remove_dups_preserve_order (impls @ intfs)
  | Names_spec.List xs ->
    List.filter xs ~f:intf_is_buildable,
    List.filter xs ~f:impl_is_buildable,
    (* Do not filter all names given by user, to allow detection of any names for which
       there is neither a .ml or .mli *)
    xs

let get_pp_of_name dc ~dir ~libname ~default_pp ~ounit ~macro ~preprocess_spec =
  let pp_of_name =
    let h = Hashtbl.Poly.create() in
    List.iter preprocess_spec ~f:(fun (kind, names_spec) ->
      let _, _, modules = eval_names_spec ~dc names_spec in
      List.iter modules ~f:(fun name ->
        Hashtbl.add_exn h ~key:name ~data:kind
      )
    );
    fun name -> Hashtbl.find h name
  in
  fun name ->
    match (pp_of_name name) with
    | None -> [], [], default_pp
    | Some kind -> eval_preprocess_kind ~dir ~libname ~ounit ~macro kind

(*----------------------------------------------------------------------
 rules for a directory of ml
----------------------------------------------------------------------*)

let mem_of_list l =
  let set = String.Hash_set.of_list l in
  fun x -> Hash_set.mem set x

let setup_ml_compile_rules
    dc ~dir ~libname ~preprocessor_deps ~preprocess_spec ~wrapped ~for_executable ~ounit
    ~macro ~names_spec =
  let default_pp = None in
  let names_spec_intfs, names_spec_impls, names_spec_modules =
    eval_names_spec ~dc names_spec
  in
  let names_spec_has_impl = mem_of_list names_spec_impls in
  let names_spec_has_intf = mem_of_list names_spec_intfs in
  let pp_of_name =
    get_pp_of_name dc ~dir ~libname ~default_pp ~ounit ~macro ~preprocess_spec
  in

  let names_spec_modules =
    (if wrapped then [libname] else []) @ names_spec_modules
  in

  let compile_rules = List.concat_map names_spec_modules ~f:(fun name ->
    let pp_libs, pp_deps, pp_com_opt = pp_of_name name in
    let pp_deps =
      List.map preprocessor_deps ~f:(fun s ->
        Dep.path (Path.relative_or_absolute ~dir s)
      ) @ pp_deps
    in
    let exists_ml = names_spec_has_impl name in
    let exists_mli = names_spec_has_intf name in

    let is_ml_wrap = wrapped && String.(name = libname) in
    if not exists_ml && not exists_mli && not is_ml_wrap
    then (
      let mes = sprintf "no .ml OR .mli for %s in %s" name
        (Path.to_string dir)
      in
      message "%s" mes;
      failwith mes;
    )
    else
      let must_be_sharable = false in
      let mc = {MC.
        dc;
        dir;
        libname;
        pp_libs;
        pp_deps;
        pp_com_opt;
        exists_mli;
        wrapped;
        must_be_sharable;
      }
      in
      List.concat [
        (if not Wrap_style.packing && is_ml_wrap then [
          gen_dfile mc ~name ~suf:dash_ml_wrap ~dsuf:".ml.d";
          native_compile_ml mc ~name;
         ]
         else []);

        (if exists_ml then [
          gen_dfile mc ~name ~suf:".ml" ~dsuf:".ml.d";
          byte_compile_ml mc ~name;
          native_compile_ml mc ~name ;
         ]
            @ (if for_executable then [gen_objdeps ~libname ~dir name] else [])
            @ generate_pp mc ~name
         else []);
        (if exists_mli then [
          gen_dfile mc ~name ~suf:".mli" ~dsuf:".mli.d";
          compile_mli mc ~name;
        ] else []);
      ]
  )
  in
  compile_rules

(*----------------------------------------------------------------------
 preprocessor_conf
----------------------------------------------------------------------*)

let share_preprocessor dc ~dir name = (* .cmx/.o -> .cmxs *)
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let file suf = suffixed ~dir name suf in
  let cmx = file ".cmx" in
  let o = file ".o" in
  let cmxs = file ".cmxs" in
  let must_be_sharable = true in
  let flags = ocamlflags @ ocamloptflags in
  let flags = if must_be_sharable then remove_nodynlink flags else flags in
  Rule.create1 ~deps:[ocaml_bin_dep; Dep.path cmx; Dep.path o] ~targets:[cmxs] ~action:(
    Action.shell ~dir ~prog:ocamlopt_prog ~args:(
      flags @ ["-shared"; "-o"; basename cmxs; basename cmx]
    )
  )

let empty_pack_order ~dir ~libname =
  let target = pack_order_file ~dir ~libname in
  write_names_rule [] ~target

let preprocessor_rules dc ~dir preprocessor_conf =
  let libname = Preprocessor_conf.name preprocessor_conf in
  let ounit = need_ounit ~libname in
  let macro = false in
  let preprocess_spec = Preprocessor_conf.preprocess preprocessor_conf in
  let default_pp = Some "camlp4orf" in
  let pp_of_name =
    get_pp_of_name dc ~dir ~libname ~default_pp ~ounit ~macro ~preprocess_spec
  in
  let name = libname in
  let pp_libs, pp_deps, pp_com_opt = pp_of_name name in
  let wrapped = false in
  let {DC.intf_is_buildable;_} = dc in
  let exists_mli = intf_is_buildable name in
  let must_be_sharable = true in
  let mc = {MC.
    dc;
    dir;
    libname;
    pp_libs;
    pp_deps;
    pp_com_opt;
    exists_mli;
    wrapped;
    must_be_sharable;
  }
  in
  assert (name = libname);
  let default_rule =
    let suffixes = [".cmi";".cmo"] in
    Rule.default ~dir (
      List.map suffixes ~f:(fun suf ->
        Dep.path (suffixed ~dir name suf))
    )
  in
  List.concat [
    knot_rules ~dir ~libname ~modules:[];
    [empty_pack_order ~dir ~libname];
    [
      gen_dfile mc ~name ~suf:".ml" ~dsuf:".ml.d";
      byte_compile_ml mc ~name;
      native_compile_ml mc ~name ;
      share_preprocessor dc ~dir name;
      default_rule;
    ];
    generate_pp mc ~name;
    if exists_mli then [
      gen_dfile mc ~name ~suf:".mli" ~dsuf:".mli.d";
      compile_mli mc ~name;
    ] else [];
  ]

(*----------------------------------------------------------------------
 pack ordering
----------------------------------------------------------------------*)

module Ordering = struct
  let find_shortest_cycle_using_floyd_warshal ~dir graph =
    (* cycles (especially in core) are already confusing enough that there is no need to
       put unrelated modules in there *)
    let to_int, of_int =
      let h1 = Hashtbl.Poly.create () in
      let h2 = Hashtbl.Poly.create () in
      List.iteri graph ~f:(fun i (name, _) ->
        Hashtbl.add_exn h1 ~key:i ~data:name;
        Hashtbl.add_exn h2 ~key:name ~data:i;
      );
      (fun name ->
         match Hashtbl.find h2 name with
         | Some x -> x
         | None ->
           failwithf "The library in %s doesn't contain the module %s but depends on it" dir name ()),
      (fun i -> Hashtbl.find_exn h1 i)
    in
    let n = List.length graph in
    let dist = Array.init n ~f:(fun _ -> Array.create ~len:n 100000) in
    let next = Array.init n ~f:(fun _ -> Array.create ~len:n None) in
    List.iter graph ~f:(fun (name, deps) ->
      List.iter deps ~f:(fun dep ->
        dist.(to_int name).(to_int dep) <- 1
      )
    );
    for k = 0 to n - 1 do
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if dist.(i).(k) + dist.(k).(j) < dist.(i).(j) then begin
            dist.(i).(j) <- dist.(i).(k) + dist.(k).(j);
            next.(i).(j) <- Some k
          end
        done;
      done;
    done;
    let min_index = ref (-1) in
    let min_len = ref 100000000 in
    for i = 0 to n - 1; do
      if dist.(i).(i) < !min_len then begin
        min_len := dist.(i).(i);
        min_index := i
      end
    done;
    let rec build_cycle acc i j =
      match next.(i).(j) with
      | None -> acc
      | Some k -> build_cycle (of_int k :: build_cycle acc i k) k j
    in
    build_cycle [of_int !min_index] !min_index !min_index

  let pack ~libname ~module_and_dfiles ~target =
    let rec loop acc = function
      | [] -> return (List.rev acc)
      | (name, dfile) :: tl ->
        file_words dfile *>>= fun words ->
        loop ((name, words) :: acc) tl
    in
    loop [] module_and_dfiles *>>| fun l ->
    let alist_from_module_to_pack_dep =
      (* regroup the dependencies from the mli.d and the ml.d together *)
      Map.to_alist (String.Map.of_alist_fold l ~init:[] ~f:(@)) in
    let rec sort rev_acc alist =
      (* returns a deterministic list: the smallest permutation of the list of modules
         wrt to lexicographic comparison on the list of string that respects the
         dependencies *)
      if List.is_empty alist then List.rev rev_acc else begin
        match List.find_map alist ~f:(fun (mod_, deps) ->
          if List.is_empty deps then Some mod_ else None)
        with
        | None ->
          let target = Path.to_string target in
          let dir = Filename.dirname target in
          let base = Filename.basename target in
          let cycle = find_shortest_cycle_using_floyd_warshal ~dir alist in
          (* putting the output if whatever format will please the omake-mode *)
          Print.printf "\n- build %s %s\n" dir base;
          Print.printf "File \"OMakefile\", line 1, characters 1-1:\n";
          Print.printf "Error: dependency cycle: %s\n" (String.concat ~sep:" " cycle);
          Print.printf "- exit %s %s\n%!" dir base;
          raise Exit
        | Some mod_ ->
          let alist = List.filter alist ~f:(fun (mod', _) -> String.(<>) mod_ mod') in
          let alist = List.map alist ~f:(fun (mod1, deps) ->
            (mod1, List.filter deps ~f:(fun dep -> String.(<>) mod_ dep))
          ) in
          let rev_acc = mod_ :: rev_acc in
          sort rev_acc alist
      end
    in
    let sorted_modules = sort [] alist_from_module_to_pack_dep in

    let sorted_modules =
      if Wrap_style.packing
      then sorted_modules
      else
        let knot_name = prefix_name ~libname ~name:knot_base in
        let sorted_modules = knot_name :: sorted_modules in
        sorted_modules
    in
    write_string_action (String.concat ~sep:" " sorted_modules) ~target
  ;;

  let comparison order_file =
    file_words order_file *>>| fun ordered_list ->
    let _, map =
      List.fold ~init:(0, String.Map.empty) ordered_list ~f:(fun (count, map) elt ->
        (count + 1, Map.add map ~key:elt ~data:count)
      ) in
    fun elt1 elt2 ->
      match Map.find map elt1 with
      | None ->
        failwithf "can't find %s in %s" elt1 (String.concat ~sep:" " ordered_list) ()
      | Some count1 ->
        match Map.find map elt2 with
        | None ->
          failwithf "can't find %s in %s" elt2 (String.concat ~sep:" " ordered_list) ()
        | Some count2 -> Int.compare count1 count2

  let sort order_file unsorted_cmxs =
    comparison order_file *>>| fun comparison ->
    List.sort unsorted_cmxs ~cmp:(fun cmx1 cmx2 ->
      let mod1 = Filename.chop_extension cmx1 in
      let mod2 = Filename.chop_extension cmx2 in
      comparison mod1 mod2)

end

(* rules that generate libname.pack-order, that contains the modules of the libraries
   sorted in topological order *)
let pack_order ~dir ~impls ~intfs ~libname =
  let impls = List.filter impls ~f:(fun name -> not (String.(name = libname))) in
  let intfs = List.filter intfs ~f:(fun name -> not (String.(name = libname))) in
  let impls = map_prefix_name ~libname impls in
  let intfs = map_prefix_name ~libname intfs in
  let module_and_dfiles =
    List.map impls ~f:(fun impl -> impl, relative ~dir (impl ^ ".ml.d"))
    @ List.map intfs ~f:(fun intf -> intf, relative ~dir (intf ^ ".mli.d"))
  in
  let target = pack_order_file ~dir ~libname in
  Rule.create ~targets:[target] (Ordering.pack ~libname ~module_and_dfiles ~target)

(*----------------------------------------------------------------------
 building ocaml libraries - packing/arching
----------------------------------------------------------------------*)

let ocaml_archive dc ~dir ~impls ~libname =
  let {DC. ocamlflags; ocamlcflags; ocamloptflags; ocamllibflags; _} = dc in
  let impls =
    List.filter impls ~f:(fun name -> not (String.(name = libname)))
  in
  let impls = map_prefix_name ~libname impls in
  List.map
    [ ocamlc_prog, ocamlcflags, ".cma", [], ".cmo", []
    ; ocamlopt_prog, ocamloptflags, ".cmxa", [".a"], ".cmx", [".o"]
    ] ~f:(fun (ocamlcomp, ocamlcompflags, lib, lib_implicit, mod_, mod_implicit) ->
      let targets = List.concat_cartesian_product [libname] (lib :: lib_implicit) in
      let deps =
        let impls = impls @ [libname] in
        List.concat_cartesian_product impls (mod_ :: mod_implicit)
      in
      let unsorted_mod_args = List.concat_cartesian_product impls [mod_] in
      Rule.create_relative ~dir ~targets ~deps ~non_relative_deps:[ocaml_bin_dep] (
        (begin match unsorted_mod_args with
         | [] | [_] -> return unsorted_mod_args
         | _ :: _ :: _ -> Ordering.sort (pack_order_file ~dir ~libname) unsorted_mod_args
         end *>>| fun sorted_mod_args ->
         let sorted_mod_args = sorted_mod_args @ [libname ^ mod_] in
         Action.shell ~dir ~prog:ocamlcomp ~args:(
           ocamlflags @ ocamlcompflags
           @ sorted_mod_args
           @ ocamllibflags
           @ ["-a"; "-o"; libname ^ lib ]))))

let pack =
  let native_action ~dir ~prog ~args ~target:_ = Action.shell ~dir ~prog ~args in
  let tmpdir = ".tempdir_for_cmo_packing" in
  let bytecode_action ~dir ~prog ~args ~target =
    (* Build the packed .cmo in a temporary sub-dir to avoid clashing with native
       compilation.  Name the subdir with a leading "." to avoid triggering jenga subdir
       build-setup *)
    Bash.action ~dir [
      bash1 "rm" ["-rf"; tmpdir];
      bash1 "mkdir" [tmpdir];
      bash1 prog args;
      bash1 "mv" [tmpdir ^ "/" ^ target; target];
      bash1 "rm" ["-rf"; tmpdir];
    ]
  in
  fun dc ~dir ~impls ~libname ~exists_libname_cmi ->
  let {DC. ocamlflags; ocamloptflags; ocamlcflags; _} = dc in
  List.map
    [ ocamlc_prog, ocamlcflags, ".cmo", [], Some (tmpdir ^ "/"), bytecode_action
    ; ocamlopt_prog, ocamloptflags, ".cmx", [".o"], None, native_action
    ] ~f:(fun (ocamlcomp, ocamlcompflags, mod_, mod_implicit, target_subdir, action) ->
      let unsorted_mod_args = List.concat_cartesian_product impls [mod_] in
      let mod_deps = List.concat_cartesian_product impls (".cmi" :: mod_ :: mod_implicit) in
      let target_arg = libname ^ mod_ in
      let targets = List.concat_cartesian_product [libname] (mod_ :: mod_implicit) in
      let mod_deps, targets =
        let intf = libname ^ ".cmi" in
        if exists_libname_cmi
        then intf :: mod_deps, targets
        else mod_deps, (if Option.is_none target_subdir then intf :: targets else targets)
      in
      let flags = ocamlflags @ ocamlcompflags in
      let targets =
        let cmt = libname ^ ".cmt" in
        if target_subdir = None && List.mem flags "-bin-annot" then cmt :: targets else targets
      in
      Rule.create_relative
        ~dir
        ~targets
        ~deps:mod_deps
        ~non_relative_deps:[ocaml_bin_dep]
        (Ordering.sort (pack_order_file ~dir ~libname) unsorted_mod_args
         *>>| fun sorted_mod_args ->
         let args =
           flags
           @ ["-g"]
           @ ["-pack"; "-o"; (Option.value target_subdir ~default:"") ^ target_arg]
           @ sorted_mod_args
         in
         action ~dir ~prog:ocamlcomp ~args ~target:target_arg
        )
    )

let hg_version_out = root_relative "hg_version.out"
let hg_version_sh = root_relative "bin/hg_version.sh"

let hg_version_out_rule ~dir =
  Rule.create ~targets:[hg_version_out] (
    let hg_paths = [] in
    let deps = (Dep.path hg_version_sh :: List.map hg_paths ~f:Dep.path) in
    Dep.all_unit deps *>>= fun () ->
    let action =
      bashf ~dir "%s %s > %s"
        (Path.to_string hg_version_sh)
        (if version_util_support then "" else "--no-version-util")
        (basename hg_version_out)
    in
    return action
  )

let hg_version_base ~base = base ^ ".hg_version"

let generate_static_string_c_code_sh = root_relative "bin/generate_static_string_c_code.sh"

let hg_version_rules ~dir ~exe =
  let base = hg_version_base ~base:exe in
  let c = relative ~dir (base ^ ".c") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    Rule.create1 ~targets:[o] ~deps:[ocaml_bin_dep; Dep.path c]
      ~action:(
        Action.shell ~dir ~prog:ocamlc_prog ~args:[basename c; "-o"; basename o;]
      )
  in
  let c_rule =
    Rule.create1 ~targets:[c]
      ~deps:[Dep.path generate_static_string_c_code_sh; Dep.path hg_version_out]
      ~action:(
        bashf ~dir "%s __wrap_generated_hg_version < %s > %s"
          (dotdot ~dir generate_static_string_c_code_sh)
          (dotdot ~dir hg_version_out)
          (basename c))
  in
  [c_rule; o_rule]

(*----------------------------------------------------------------------
 build_info
----------------------------------------------------------------------*)

let build_info_base ~base = base ^ ".build_info"

let build_info_sh = root_relative "bin/build_info.sh"

let link_deps_of_libs ~dir ~libname ~name ~libs ~objs =
  let prefixed_name = prefix_name ~libname ~name in
  let main_cmx = relative ~dir (prefixed_name ^ ".cmx") in
  let main_o = relative ~dir (prefixed_name ^ ".o") in
  let libdeps =
    LL.liblink_deps ~libs ~suffixes:[".cmxa"; ".a"]
    @ LL.liblink_default ~libs
  in
  let objdeps =
    List.concat_map objs ~f:(fun prefixed_name -> [
      Dep.path (relative ~dir (prefixed_name ^ ".cmx"));
      Dep.path (relative ~dir (prefixed_name ^ ".o"));
    ]
    )
  in
  let link_deps =
  (* Both the rules which generate the .exe and which generate the .build_info
     depend on these link_deps *)
    libdeps @ objdeps @
      [Dep.path (relative ~dir (hg_version_base ~base:(name ^ ".exe") ^ ".o"));
       Dep.path main_cmx;
       Dep.path main_o
      ]
  in
  link_deps

let build_info_rules ~dir ~name ~ext ~sexp_dep =
  let base = build_info_base ~base:(name ^ ext) in
  let sexp_file = relative ~dir (base ^ ".sexp") in
  let c = relative ~dir (base ^ ".c") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    Rule.create1 ~targets:[o] ~deps:[ocaml_bin_dep; Dep.path c]
      ~action:(
        Action.shell ~dir ~prog:ocamlc_prog ~args:[basename c; "-o"; basename o;]
      )
  in
  let c_rule =
    Rule.create1 ~targets:[c]
      ~deps:[Dep.path generate_static_string_c_code_sh; Dep.path sexp_file]
      ~action:(
        bashf ~dir "%s __wrap_generated_build_info < %s > %s"
          (dotdot ~dir generate_static_string_c_code_sh)
          (basename sexp_file)
          (basename c))
  in
  let fullname_exe = Path.to_string (relative ~dir (name ^ ext)) in
  let sexp_rule =
    Rule.create ~targets:[sexp_file] (
      sexp_dep *>>= fun () ->
      let action =
        bashf ~dir "X_LIBRARY_INLINING=%b %s %s %s jenga > %s"
          x_library_inlining
          (dotdot ~dir build_info_sh)
          Ocaml_version.name
          fullname_exe
          (basename sexp_file)
      in
      return action
    )
  in
  [sexp_rule; c_rule; o_rule]

(*----------------------------------------------------------------------
 top levels - mycaml
----------------------------------------------------------------------*)

let mycaml_rules dc ~dir ~libname =
  let {DC. ocamlflags; no_mycaml_alias; _ } = dc in
  let targets = [relative ~dir "mycaml"; relative ~dir ".mydeps";] in
  let hg_version = relative ~dir ("mycaml.bc.hg_version.o") in
  let build_info = relative ~dir ("mycaml.bc.build_info.o") in
  let mycaml_rule =
    Rule.create ~targets (
      file_words (relative ~dir (libname ^ ".libdeps")) *>>= fun libs ->
      let libs = at_head "dynlink" libs in
      let libs = at_head "pa_ounit_lib" libs in
      let libs = at_head "pa_bench_lib" libs in
      let libs = libs @ [libname] in
      libdeps_for dc "auto_top" *>>= fun x_libs ->
      let libs = remove_dups_preserve_order (libs @ x_libs) in
      Dep.all_unit [
        Dep.all_unit (LL.liblink_deps ~libs ~suffixes:[".cmi";".cma";".stub.names"]);
        Dep.all_unit (LL.liblink_submodule_cmis ~libs);
        LL.liblinks_stubs ~libs;
        Dep.path hg_version;
        Dep.path build_info;
      ] *>>| fun () ->
      let standard_cmas =
        List.map (ocamlpacks @ ["ocamlcommon"; "ocamlbytecomp"; "ocamltoplevel"])
          ~f:(fun name -> name ^ ".cma")
      in
      let lib_cmas =
        List.concat_map libs ~f:(fun lib -> [
          "-I"; dotdot ~dir (LL.liblink_dir ~lib);
          lib ^ ".cma";
        ])
      in
      let args = List.concat [
        ["-linkall"; "-cc"; "g++ -Wl,-E"; "-custom";];
        ("-g" :: ocamlflags);
        ["-I"; "+compiler-libs"];
        ["-I"; "+ocamldoc"];
        standard_cmas;
        wrap_arg_for_linker "--wrap=generated_build_info"; [basename hg_version];
        wrap_arg_for_linker "--wrap=generated_hg_version"; [basename build_info];
        lib_cmas;
        ["-o"; "mycaml";];
      ]
      in
      let mydeps_string =
        String.concat ~sep:" " (
          List.map libs ~f:(fun lib ->
            dotdot ~dir (LL.liblink_refname ~lib ~name:(lib ^ ".cmo"))
          )
        )
      in
      Bash.action ~dir [
        echo_string mydeps_string ~target:".mydeps";
        bash1 ocamlc_prog args
      ]
    )
  in
  List.concat [
    hg_version_rules ~dir ~exe:("mycaml.bc");
    build_info_rules ~dir ~name:"mycaml" ~ext:".bc" ~sexp_dep:(return ());
    [
      mycaml_rule;
      Rule.alias (Alias.mycaml ~dir) (
        if no_mycaml_alias then [] else
          [Dep.path (relative ~dir "mycaml")]
      );
    ]
  ]

(*----------------------------------------------------------------------
 top levels - utop
----------------------------------------------------------------------*)

(* UTop dependencies *)
module UTop = struct
  let opam_lib = Ocaml_version.root ^/ Ocaml_version.name ^/ "lib"

  let includes = [
    "+compiler-libs";
    "+ocamldoc";
    "+camlp4";
    opam_lib ^/ "findlib";
    opam_lib ^/ "camomile";
    opam_lib ^/ "react";
    opam_lib ^/ "zed";
    opam_lib ^/ "lwt";
    opam_lib ^/ "lambda-term";
    opam_lib ^/ "utop";
  ]

  let packs = ocamlpacks @ [
    "ocamlcommon";
    "ocamlbytecomp";
    "ocamltoplevel";
    "dynlink";
    "camlp4o";
    "findlib";
    "findlib_top";
    "camomile";
    "react";
    "zed";
    "lwt";
    "lwt-react";
    "lwt-log";
    "lwt-unix";
    "lambda-term";
    "utop";
    "utop-camlp4";
  ]

  let syntaxes = pa_jane
end

let utopdeps_rules ~dir ~libname =
  let base = "utop.bc.deps" in
  let c = relative ~dir (base ^ ".c") in
  let o = relative ~dir (base ^ ".o") in
  let o_rule =
    Rule.create1 ~targets:[o] ~deps:[ocaml_bin_dep; Dep.path c]
      ~action:(
        Action.shell ~dir ~prog:ocamlc_prog ~args:[basename c; "-o"; basename o;]
      )
  in
  let c_rule =
    Rule.create ~targets:[c] (
      file_words (relative ~dir (libname ^ ".libdeps")) *>>= fun libs ->
      Dep.path generate_static_string_c_code_sh *>>| fun () ->
      bashf ~dir "echo -n %S | %s generated_utop_deps > %s"
        (String.concat (libs @ [libname]) ~sep:" ")
        (dotdot ~dir generate_static_string_c_code_sh)
        (basename c)
      )
  in
  [c_rule; o_rule]

let utop_rules dc ~dir ~libname =
  let {DC. ocamlflags; no_utop_alias; _ } = dc in
  let targets = [relative ~dir "utop"] in
  let hg_version = relative ~dir "utop.bc.hg_version.o" in
  let build_info = relative ~dir "utop.bc.build_info.o" in
  let utopdeps   = relative ~dir "utop.bc.deps.o"       in
  let utop_rule =
    Rule.create ~targets (
      file_words (relative ~dir (libname ^ ".libdeps")) *>>= fun libs ->
      let libs = at_head "pa_ounit_lib" libs in
      let libs = at_head "pa_bench_lib" libs in
      let libs = libs @ [libname] in
      libdeps_for dc "js_utop" *>>= fun x_libs ->
      let libs = remove_dups_preserve_order (libs @ x_libs) in
      let libs = List.filter libs ~f:(fun lib -> lib <> "dynlink") in
      Dep.all_unit [
        Dep.all_unit (LL.liblink_deps ~libs ~suffixes:[".cmi";".cma";".stub.names"]);
        Dep.all_unit (LL.liblink_submodule_cmis ~libs);
        LL.liblinks_stubs ~libs;
        Dep.path hg_version;
        Dep.path build_info;
        Dep.path utopdeps;
        Dep.all_unit (LL.liblink_deps ~libs:UTop.syntaxes ~suffixes:[".cmo"]);
      ] *>>| fun () ->
      let standard_cmas = List.map UTop.packs ~f:(fun name -> name ^ ".cma") in
      let lib_cmas =
        List.concat_map libs ~f:(fun lib -> [
            "-I"; dotdot ~dir (LL.liblink_dir ~lib);
            lib ^ ".cma";
          ])
      in
      let syntax_cmos =
        List.concat_map UTop.syntaxes ~f:(fun syntax -> [
            "-I"; dotdot ~dir (LL.liblink_dir ~lib:syntax);
            syntax ^ ".cmo";
          ])
      in
      let includes =
        List.concat_map UTop.includes ~f:(fun dir -> ["-I"; dir])
      in
      let args = List.concat [
        ["-linkall"; "-cc"; "g++ -Wl,-E"; "-custom";];
        ("-g" :: ocamlflags);
        includes;
        standard_cmas;
        wrap_arg_for_linker "--wrap=generated_build_info"; [basename hg_version];
        wrap_arg_for_linker "--wrap=generated_hg_version"; [basename build_info];
        [basename utopdeps];
        syntax_cmos;
        lib_cmas;
        ["-o"; "utop";];
      ]
      in
      Bash.action ~dir [bash1 ocamlc_prog args]
    )
  in
  List.concat [
    hg_version_rules ~dir ~exe:"utop.bc";
    build_info_rules ~dir ~name:"utop" ~ext:".bc" ~sexp_dep:(return ());
    utopdeps_rules ~dir ~libname;
    [
      utop_rule;
      Rule.alias (Alias.utop ~dir) (
        if no_utop_alias then [] else
          [Dep.path (relative ~dir "utop")]
      );
    ]
  ]

(*----------------------------------------------------------------------
  Rules that check if libraries define tests or benchs
----------------------------------------------------------------------*)

let fgrep_rule ~dir ~filename ~macros ~impls =
  let target = relative ~dir filename in
  let sources = List.map impls ~f:(fun impl -> relative ~dir (impl ^ ".ml")) in
  Rule.create1
    ~targets:[target]
    ~deps:(List.map sources ~f:Dep.path)
    ~action:(
      bashf ~dir "\
 (cat %s | fgrep -w -f <(%s) || true) > %s"
        (String.concat ~sep:" " (List.map ~f:basename sources))
        (String.concat ~sep:"; " (List.map macros ~f:(sprintf "echo %s")))
        (basename target)
    )

let fgrep_test_filename = "fgrep_tests.out"
let test_macros = ["TEST"; "TEST_UNIT"; "TEST_MODULE"]

let fgrep_bench_filename = "fgrep_bench.out"
let bench_macros = ["BENCH"; "BENCH_FUN"; "BENCH_MODULE"; "BENCH_INDEXED"]

(*----------------------------------------------------------------------
 library_rules
----------------------------------------------------------------------*)

module Library_conf = struct

  type t = {
    name : string;
    libraries : string sexp_list;
    interfaces : string sexp_list;
    extra_disabled_warnings : int sexp_list;
    flags : Ordered_set_lang.t sexp_option;
    ocamlc_flags : Ordered_set_lang.t sexp_option;
    ocamlopt_flags : Ordered_set_lang.t sexp_option;
    ocaml_includes : string sexp_list;
    extra_cflags : string sexp_list;
    avoid_cflags : string sexp_list;
    replace_cflags : string sexp_list;
    extra_cxxflags : string sexp_list;
    avoid_cxxflags : string sexp_list;
    includes : string sexp_list;
    library_flags : string sexp_list;
    modules : Names_spec.t with default(Names_spec.All);
    c_names : string sexp_list;
    cxx_names : string sexp_list;
    o_names : string sexp_list;
    cxx_suf : string option with default(None);
    stubs_conf : StubsX_conf.t option with default(None);
    packed : bool with default(true);
      (* The interpretation of the "packed" flag (which still defaults to true)
         is now determined by Wrap_style.select / Wrap_style.packing

         Pack -> Continue to use the -for-pack / -pack compiler flags

         Prefix ->
             Dont use -pack / -for-pack
             Generate wrapper module (.ml-wrap) from list of modules
             Generate alias file
             Use combination of -open and (newly fixed) -o arg of 4.02 compiler
      *)

    preprocess : Preprocess_spec.t list with default(preprocess_default);
    preprocessor_deps : string sexp_list;
    inline_tests_deps : string sexp_list;
    ocamllex : string sexp_list;
    ocamlyacc : string sexp_list;
    cclibs : string sexp_list;
    skip_from_default : bool with default(false);
  } with of_sexp, fields

  let disabled_warnings t =
    Ocaml_version.disabled_warnings @ t.extra_disabled_warnings

  let ocaml_includes t =
    List.concat_map t.ocaml_includes ~f:(fun path -> ["-I"; expand_vars_root path])

  let ocamlflags t =
    let ocamlflags =
      ocaml_includes t @ Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t)
    in
    Ordered_set_lang.eval_with_standard t.flags ~standard:ocamlflags

  let ocamlcflags t =
    Ordered_set_lang.eval_with_standard t.ocamlc_flags ~standard:Top.ocamlcflags

  let ocamloptflags t =
    Ordered_set_lang.eval_with_standard t.ocamlopt_flags ~standard:Top.ocamloptflags

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    let ocamlcflags = ocamlcflags t in
    let ocamloptflags = ocamloptflags t in
    {dc with DC.
      ocamlflags;
      ocamlcflags;
      ocamloptflags
    }

  let cflags t =
    match t.replace_cflags with _::_ as xs -> xs | [] ->
      let avoid_set = String.Hash_set.of_list t.avoid_cflags in
      List.filter (default_cflags @ t.extra_cflags)
        ~f:(fun flag -> not (Hash_set.mem avoid_set flag))

  let cxxflags t =
    let avoid_set = String.Hash_set.of_list t.avoid_cxxflags in
    List.filter (default_cxxflags @ t.extra_cxxflags)
      ~f:(fun flag -> not (Hash_set.mem avoid_set flag))

end

let library_rules dc ~dir ~cflags library_conf =
  let dc = Library_conf.extend_dc library_conf dc in
  let libname = Library_conf.name library_conf in
  let ounit = need_ounit ~libname  in
  let wrapped = Library_conf.packed library_conf in
  let preprocessor_deps = Library_conf.preprocessor_deps library_conf in
  let ocamllex_rules =
    List.map (Library_conf.ocamllex library_conf) ~f:(ocamllex_rule ~dir)
  in
  let ocamlyacc_rules =
    List.map (Library_conf.ocamlyacc library_conf) ~f:(ocamlyacc_rule ~dir)
  in
  let macro = false in
  let autogen =
    let {DC. autogen; _} = dc in
    List.map autogen ~f:(relative ~dir)
  in
  let preprocess_spec = Library_conf.preprocess library_conf in
  let names_spec = Library_conf.modules library_conf in
  let for_executable = false in
  let compile_rules =
    setup_ml_compile_rules dc ~dir ~libname ~preprocess_spec ~wrapped ~for_executable
      ~ounit ~macro ~preprocessor_deps
      ~names_spec
  in
  let c_names = Library_conf.c_names library_conf in
  let cxx_names = Library_conf.cxx_names library_conf in
  let cxxflags = Library_conf.cxxflags library_conf in
  let includes = Library_conf.includes library_conf in
  let includes = List.map includes ~f:expand_vars_root  in
  let i_includes = List.concat_map includes ~f:(fun path -> ["-I"; path]) in
  let cflags = List.map cflags ~f:expand_vars_root @ i_includes in
  let cxxflags = cxxflags @ i_includes in
  let cxx_suf =
    match Library_conf.cxx_suf library_conf with
    | None -> ".cpp"
    | Some suf -> "." ^ suf
  in
  let compile_c_rules = compile_c ~cflags ~autogen ~dir c_names in
  let compile_cxx_rules = compile_cxx ~cxxflags ~cxx_suf ~autogen ~dir cxx_names in
  let o_names = c_names @ cxx_names @
    Library_conf.o_names library_conf (*objects to link but not compile*)
  in
  let intfs, impls, modules = eval_names_spec ~dc names_spec in
  let stubs_conf = Library_conf.stubs_conf library_conf in
  let stub_names =
    match stubs_conf with
    | Some _ -> [libname]
    | None -> match o_names with [] -> [] | _::_ -> [libname]
  in
  let stub_rules =
    let target_a = sprintf "lib%s_stubs.a" libname in
    match stubs_conf with
    | Some conf ->
      let targets = [target_a] in
      let deps = StubsX_conf.deps conf in
      let action = StubsX_conf.action conf in
      let conf = { Rule_conf. targets; deps; action; } in
      let rule = rule_conf_to_rule dc ~dir ~cflags conf in
      [rule]
    | None -> [
        static_archive_c ~dir ~o_names ~target:target_a;
      ]
  in
  let ocamllibflags =
    List.concat [
      List.concat_map stub_names ~f:(fun name ->
        ["-cclib"; "-l" ^ name ^ "_stubs"]
      );
      List.map (Library_conf.library_flags library_conf) ~f:expand_vars_root;
      List.concat_map (Library_conf.cclibs library_conf) ~f:(fun lib ->
        ["-cclib"; sprintf "-l%s" lib]
      );
      dc.DC.ocamllibflags;
    ]
  in
  let dc = {dc with DC. ocamllibflags} in
  let order_rules =
    [pack_order ~dir ~impls ~intfs ~libname]
  in

  let pack_maybe_archive_rules =
    if Wrap_style.packing
    then
      if wrapped
      then
        List.concat [
          pack dc ~dir ~impls ~libname ~exists_libname_cmi:false;
          ocaml_archive dc ~dir ~libname ~impls:[libname];
        ]
      else
        ocaml_archive dc ~dir ~libname ~impls
    else
      List.concat [
        knot_rules ~dir ~libname ~modules;
        (if wrapped then [gen_library_wrap ~dir ~libname ~modules] else []);
        ocaml_archive dc ~dir ~libname ~impls;
      ]
  in

  let default_targets =
    (if Library_conf.skip_from_default library_conf then []
     else
        let suffixes = [".cmi";".cmxa";".a"] in
        List.map suffixes ~f:(fun suf -> suffixed ~dir libname suf)
    ) @
      (List.map stub_names ~f:(fun name ->
         relative ~dir (sprintf "lib%s_stubs.a" name)))
      @
      (if with_mycaml then [relative ~dir "mycaml"] else [])
      @
      (if with_utop then [relative ~dir "utop"] else [])
  in
  let default_rules =
    [Rule.alias (Alias.lib_artifacts ~dir) (List.map default_targets ~f:Dep.path)]
  in
  List.concat [
    order_rules;
    pack_maybe_archive_rules;
    compile_c_rules;
    compile_cxx_rules;
    ocamllex_rules;
    ocamlyacc_rules;
    [c_stubs ~dir ~libname ~stub_names];
    stub_rules;
    compile_rules;
    default_rules;
    mycaml_rules dc ~dir ~libname;
    utop_rules dc ~dir ~libname;
    [fgrep_rule ~dir ~filename:fgrep_test_filename ~macros:test_macros ~impls];
    [fgrep_rule ~dir ~filename:fgrep_bench_filename ~macros:bench_macros ~impls];
  ]

(*----------------------------------------------------------------------
 check_ldd_deps
----------------------------------------------------------------------*)

module Check_ldd_dependencies : sig

  val check :
    allowed:Ordered_set_lang.t
    -> target:Path.t -> (Path.t -> Action.t Dep.t) -> Rule.t list

end = struct

  let standard = [
    "libz.so.1";
    "libpthread.so.0";
    "libpcre.so.0";
    "librt.so.1";
    "libdl.so.2";
    "libstdc++.so.6";
    "libm.so.6";
    "libgcc_s.so.1";
    "libc.so.6";
    "libnsl.so.1";
    "linux-vdso.so.1";
    "libjswrap.so.1";
    "libtinfo.so.5";
  ]

  let dynamic_lib_deps_sh = root_relative "bin/dynamic-lib-deps.sh"

  let check ~allowed ~target mk_action =
    let allowed = Ordered_set_lang.eval_with_standard (Some allowed) ~standard in
    let is_unexpected =
      let set = String.Hash_set.of_list allowed in
      fun name -> not (Hash_set.mem set name)
    in
    let dir = dirname target in
    let unchecked = suffixed ~dir (basename target) ".unchecked-do-not-deploy" in
    [
      Rule.create ~targets:[unchecked] (mk_action unchecked);
      Rule.create ~targets:[target] (
        (* The unchecked .exe is a dependency for the checking-action AND the cp/rm *)
        Dep.path unchecked *>>= fun () ->
        Dep.action_stdout (
          Dep.all_unit [Dep.path dynamic_lib_deps_sh; Dep.path unchecked] *>>| fun () ->
          Action.shell ~dir:Path.the_root
            ~prog:(Path.to_string dynamic_lib_deps_sh)
            ~args:[Path.to_string unchecked]
        ) *>>| fun stdout ->
        let actual = words_of_string stdout in
        let unexpected = List.filter ~f:is_unexpected actual in
        match unexpected with
        | [] ->
          bashf ~dir "cp %s %s" (basename unchecked) (basename target)
        | _ :: _ ->
          put "Removing executable with unexpected ldd deps: %s" (Path.to_string target);
          put "Allowed   : %s" (String.concat ~sep:" " allowed);
          put "Actual    : %s" (String.concat ~sep:" " actual);
          bashf ~dir "echo Unexpected dynamic dependencies: %s; rm -f %s; exit 1"
            (shell_escape (String.concat ~sep:" " unexpected))
            (basename target)
      );
    ]

end


(*----------------------------------------------------------------------
 link_native
----------------------------------------------------------------------*)

let link_quietly = root_relative "bin/link-quietly"
let ocamlwrapper = root_relative "bin/ocamlwrapper"

let ocaml_modules = Path.root_relative "bin/ocaml-modules"

let ocaml_plugin_handling dc ~dir name exe =
  let ocaml_plugin_o = name ^ ".ocaml_plugin.o" in
  match dc.DC.ocaml_plugin_libraries name with
  | None -> return ("", [])
  | Some libs ->
    let check_libraries_are_linked, paths =
      (* Why embed things if you're not using ocaml_plugin? *)
      sprintf "; %s %s Ocaml_plugin %s"
        (dotdot ~dir ocaml_modules)
        exe (String.concat ~sep:" " (List.map libs ~f:String.capitalize)),
      [ocaml_modules]
    in
    Dep.all_unit (List.map (relative ~dir ocaml_plugin_o :: paths) ~f:Dep.path)
    *>>| fun () ->
    check_libraries_are_linked,
    (ocaml_plugin_o ::
     wrap_arg_for_linker "--wrap=ocaml_plugin_archive"
     @ wrap_arg_for_linker "--wrap=ocaml_plugin_archive_digest")

let get_libs_for_exe ~dir ~libname =
  file_words (relative ~dir (libname ^ ".libdeps")) *>>= fun libs ->
  let libs = at_head "pa_ounit_lib" libs in
  let libs = at_head "pa_bench_lib" libs in
  return libs

let link_native dc ~dir ~libname ~link_flags ~allowed_ldd_dependencies name =
  let prefixed_name = prefix_name ~libname ~name in
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let main_cmx = relative ~dir (prefixed_name ^ ".cmx") in
  let hg_version_o = relative ~dir (hg_version_base ~base:(name ^ ".exe") ^ ".o") in
  let build_info_o = relative ~dir (build_info_base ~base:(name ^ ".exe") ^ ".o") in

  let maybe_ldd_check_rules ~target mk_action =
    match allowed_ldd_dependencies with
    | None -> [Rule.create ~targets:[target] (mk_action target)] (* no check *)
    | Some allowed -> Check_ldd_dependencies.check ~allowed ~target mk_action
  in

  let exe_rules =
    let target = relative ~dir (name ^ ".exe") in
    maybe_ldd_check_rules ~target (fun target ->
      let exe = basename target in
      get_libs_for_exe ~dir ~libname *>>= fun libs ->
      file_words (relative ~dir (prefixed_name ^ ".objdeps")) *>>= fun objs ->
      let deps = link_deps_of_libs ~dir ~libname ~name ~libs ~objs in
      let deps = deps @ [Dep.path build_info_o] in
      let deps = ocaml_bin_dep :: deps in
      Dep.both
         (ocaml_plugin_handling dc ~dir name exe)
         (Dep.both
           (LL.liblinks_stubs ~libs)
           (Dep.all_unit deps))
       *>>= fun ((ocaml_plugin_check, ocaml_plugin_command_line), ((), ())) ->
      let cmxa_for_packs = List.map ocamlpacks ~f:(fun name -> name ^ ".cmxa") in
      let sub_cmxs_in_correct_order = List.map objs ~f:(fun name -> name ^ ".cmx") in
      let lib_cmxas =
        List.concat_map libs ~f:(fun lib -> [
          "-I"; dotdot ~dir (LL.liblink_dir ~lib);
          lib ^ ".cmxa";
        ])
      in
      let action =
        bash ~dir (
          String.concat ~sep:" " (List.concat [
            [dotdot ~dir link_quietly;
             dotdot ~dir ocamlwrapper;
             ocamlopt_prog];
            ocamlflags; ocamloptflags;
            link_flags;
            ["-cc"; "g++"];
            cmxa_for_packs;
            wrap_arg_for_linker "--wrap=generated_build_info"; [ basename build_info_o ];
            wrap_arg_for_linker "--wrap=generated_hg_version"; [ basename hg_version_o ];
            ocaml_plugin_command_line;
            lib_cmxas;
            sub_cmxs_in_correct_order;
            [basename main_cmx;
             "-o"; exe];
            [ocaml_plugin_check];
          ])
        )
      in
      return action
    )
  in

  let sexp_dep =
    get_libs_for_exe ~dir ~libname *>>= fun libs ->
    file_words (relative ~dir (prefixed_name ^ ".objdeps")) *>>= fun objs ->
    let deps = link_deps_of_libs ~dir ~libname ~name ~libs ~objs in
    let deps = [Dep.path build_info_sh] @ deps in
    Dep.all_unit deps
  in

  List.concat [
    exe_rules;
    hg_version_rules ~dir ~exe:(name ^ ".exe");
    build_info_rules ~dir ~name ~ext:".exe" ~sexp_dep;
  ]

(*----------------------------------------------------------------------
 executable_rules
----------------------------------------------------------------------*)

module Executables_conf = struct

  type t = {
    names : string list;
    allowed_ldd_dependencies : Ordered_set_lang.t sexp_option;
    extra_disabled_warnings : int sexp_list;
    flags : Ordered_set_lang.t sexp_option;
    ocamlc_flags : Ordered_set_lang.t sexp_option;
    ocamlopt_flags : Ordered_set_lang.t sexp_option;
    libraries : string sexp_list;
    ocamllex : string sexp_list;
    ocamlyacc : string sexp_list;
    preprocess : Preprocess_spec.t list with default(preprocess_default);
    preprocessor_deps : string sexp_list;
    link_flags : string sexp_list;
    modules : Names_spec.t with default(Names_spec.All);
  } with of_sexp

  let disabled_warnings t =
    Ocaml_version.disabled_warnings @ t.extra_disabled_warnings

  let ocamlflags t =
    let ocamlflags = Top.default_ocamlflags ~disabled_warnings:(disabled_warnings t) in
    Ordered_set_lang.eval_with_standard t.flags ~standard:ocamlflags

  let ocamlcflags t =
    Ordered_set_lang.eval_with_standard t.ocamlc_flags ~standard:Top.ocamlcflags

  let ocamloptflags t =
    Ordered_set_lang.eval_with_standard t.ocamlopt_flags ~standard:Top.ocamloptflags

  let extend_dc t dc =
    let ocamlflags = ocamlflags t in
    let ocamlcflags = ocamlcflags t in
    let ocamloptflags = ocamloptflags t in
    {dc with DC.
      ocamlflags;
      ocamlcflags;
      ocamloptflags;
    }

end

let fake_libname_of_exes names =
  match names with
  | [] -> failwith "executable declarations with no executables aren't allowed"
  | first_name :: _ -> "bin__" ^ first_name

let executables_rules dc ~dir e_conf =
  let { Executables_conf.
        allowed_ldd_dependencies;
        names;
        preprocess = preprocess_spec;
        preprocessor_deps;
        link_flags;
        ocamllex;
        ocamlyacc;
        modules = names_spec;
        _} = e_conf in
  let libname = fake_libname_of_exes names in
  let ounit = false in
  let ocamllex_rules = List.map ocamllex ~f:(ocamllex_rule ~dir) in
  let ocamlyacc_rules = List.map ocamlyacc ~f:(ocamlyacc_rule ~dir) in
  let wrapped = false in
  let macro = false in (*?*)
  let dc = Executables_conf.extend_dc e_conf dc in
  let for_executable = true in
  let compile_rules =
    setup_ml_compile_rules
    dc ~dir ~libname ~preprocess_spec ~wrapped ~for_executable ~ounit ~macro
    ~names_spec ~preprocessor_deps
  in
  let default_rule =
    Rule.default ~dir (
      List.map names ~f:(fun name ->
        let prefixed_name = prefix_name ~libname ~name in
        if link_executables
        then Dep.path (suffixed ~dir name ".exe")
        else Dep.path (suffixed ~dir prefixed_name ".cmx")
      )
    )
  in
  let link_rules =
    List.concat_map names ~f:(fun name ->
      link_native dc ~dir ~libname ~link_flags ~allowed_ldd_dependencies name
    )
  in
  let _intfs, _impls, modules = eval_names_spec ~dc names_spec in
  List.concat [
    knot_rules ~dir ~libname ~modules;
    ocamllex_rules;
    ocamlyacc_rules;
    compile_rules;
    [default_rule];
    link_rules;
  ]


(*----------------------------------------------------------------------
 embedding for ocaml plugin
----------------------------------------------------------------------*)

let time_limit = root_relative "bin/time_limit"

let ocaml_plugin_dir = root_relative "packages/ocaml_plugin"

let embedder = relative ~dir:ocaml_plugin_dir "bin/ocaml_embed_compiler.exe"

let ocamlopt_path = ocaml_bin ^/ ocamlopt_prog
let camlp4o_path = ocaml_bin ^/ "camlp4o.opt"
let ocamldep_path = ocaml_bin ^/ ocamldep_prog
let builtin_cmis =
  List.map [ "pervasives.cmi"
           ; "camlinternalLazy.cmi"
           ; "camlinternalMod.cmi"
           ; "camlinternalOO.cmi"
           ; "lexing.cmi" (* pa_here (and by extension pa_fail) needs this *)
           ]
    ~f:(fun cmi -> ocaml_where ^/ cmi)

let embed_rules ~dir ~cflags conf =
  let autogen = [] in
  let {Embed_conf. names; libraries; cmis; pps} = conf in
  List.concat_map names ~f:(fun prog ->
    let plugin_name = prog ^ ".ocaml_plugin" in
    let pps = expand_predefined_pa_sets pps in
    let pps = List.map pps ~f:(fun lib -> LL.liblink_refname ~lib ~name:(lib ^ ".cmxs")) in
    let call_camlp4 = match pps with | [] -> "" | _::_ ->
      sprintf "-pp %s%s"
        camlp4o_path
        (String.concat (List.map pps ~f:(fun pp -> sprintf " -pa-cmxs %s" (dotdot ~dir pp))))
    in
    let libraries = libraries @ ["ocaml_plugin"] in
    let gen_plugin_c =
      Rule.create
        ~targets:[relative ~dir (plugin_name ^ ".c")] (
        Dep.List.concat_map libraries ~f:(fun lib ->
          LL.liblink_interfaces ~lib *>>| fun deps -> lib :: deps
        ) *>>= fun libraries ->
        let libraries = remove_dups_and_sort libraries in
        let cmis =
          List.map cmis ~f:(fun name -> relative ~dir (name ^ ".cmi"))
          @ List.map libraries ~f:(fun lib -> LL.liblink_refname ~lib ~name:(lib ^ ".cmi"))
        in
        let dep_paths = cmis @ pps @ [
          (*ocamlopt_prog*)
          (*builtin_cmis*)
          time_limit;
          embedder;
        ]
        in
        Dep.all_unit (List.map ~f:Dep.path dep_paths) *>>| fun () ->
        bashf ~dir "%s 300 %s -wrap-symbol %s -cc %s -ocamldep %s %s %s -o %s"
          (dotdot ~dir time_limit)
          (dotdot ~dir embedder)
          call_camlp4
          ocamlopt_path
          ocamldep_path
          (String.concat ~sep:" " builtin_cmis)
          (String.concat ~sep:" " (List.map ~f:(dotdot ~dir) cmis))
          (plugin_name ^ ".c")
      )
    in
    let cflags = List.filter cflags ~f:(fun flag -> flag <> "-pedantic") in
    [
      gen_plugin_c;
      compile_c_1 ~cflags ~autogen ~dir plugin_name;
    ]
  )

(*----------------------------------------------------------------------
 inline_tests & benchmarks
----------------------------------------------------------------------*)

let inline_tests_gen_file_rules ~dir ~libname = [
  (* The [let M = ... ] is to avoid the linker disregarding top-level module
     initialization code (which runs the tests) in an otherwise unused module *)

  write_string_rule ~target:(relative ~dir "inline_tests_runner.ml")
    (sprintf "let () = let module M = %s in Pa_ounit_lib.Runtime.summarize ()"
       (String.capitalize libname));

  write_string_rule ~chmod_x:() ~target:(relative ~dir "inline_tests_runner") ("
#!/bin/sh\\n
# This file was generated, dont edit by hand\\n
cd $(dirname $(readlink -f $0))\\n
exec ./inline_tests_runner.exe inline-test-runner "^libname^" $@
");
]

let inline_bench_gen_file_rules ~dir ~libname = [
  write_string_rule ~target:(relative ~dir "inline_benchmarks_runner.ml")
    (sprintf "
(* Dummy import *)
include (%s : sig end)
let () =
  ignore (Inline_benchmarks.Runner.main ~libname:%S)
" (String.capitalize libname) libname);

  write_string_rule ~chmod_x:() ~target:(relative ~dir "inline_benchmarks_runner") ("
#!/bin/sh\\n
# This file was generated, dont edit by hand\\n
cd $(dirname $(readlink -f $0))\\n
exec ./inline_benchmarks_runner.exe -benchmarks-runner  $@
");
]

let run_inline_action ~dir ~user_deps filename =
  let f = relative ~dir in
  let sources = List.map ~f ([filename; filename ^ ".exe"] @ user_deps) in
  Dep.action
    (Dep.all_unit (List.map sources ~f:Dep.path)
     *>>| fun () ->
     Action.shell ~dir ~prog:(dotdot ~dir time_limit)
       ~args:["300"; "./" ^ filename])

let run_inline_tests_action ~dir ~user_deps =
  run_inline_action ~dir ~user_deps "inline_tests_runner"

let run_inline_bench_action ~dir ~user_deps =
  run_inline_action ~dir ~user_deps "inline_benchmarks_runner"

let compile_inline_runner_rule dc ~dir ~libname name =
  let pp_com_opt = None in
  let wrapped = false in
  let exists_mli = false in
  let pp_libs  = [] in
  let pp_deps = [Dep.path (relative ~dir (libname ^ ".cmx"))] in
  let must_be_sharable = false in
  let mc = {MC.
    dc;
    dir;
    libname;
    pp_libs;
    pp_deps;
    pp_com_opt;
    exists_mli;
    wrapped;
    must_be_sharable;
  }
  in [
    byte_compile_ml mc ~name;
    native_compile_ml mc ~name ;
    gen_dfile mc ~name ~suf:".ml" ~dsuf:".ml.d";
  ] @ generate_pp mc ~name

let compile_inline_tests_runner_rule dc ~dir ~libname =
  compile_inline_runner_rule dc ~dir ~libname "inline_tests_runner"

let compile_inline_benchmarks_runner_rule dc ~dir ~libname =
  compile_inline_runner_rule dc ~dir ~libname "inline_benchmarks_runner"

let non_empty_file ~dir ~filename =
  let path = relative ~dir filename in
  Dep.contents path *>>= fun s ->
  let has_file = match (String.strip s) with "" -> false | _ -> true in
  return has_file

let has_tests ~dir =
  non_empty_file ~dir ~filename:fgrep_test_filename

let has_benchmarks dc ~dir =
  let libmap = dc.DC.libmap in
  if Libmap.exists libmap "inline_benchmarks"
  then non_empty_file ~dir ~filename:fgrep_bench_filename
  else return false (* this is to avoid breaking jaxbuild *)

let link_test_or_bench_exe dc ~dir ~libname ~x_libs name =
  (*
    This rule is used link inline_tests_runner.exe AND inline_benchmarks_runner.exe
    These are special because they are built in the same directory as a library they
    reference
  *)
  let prefixed_name = prefix_name ~libname ~name in
  let {DC. ocamlflags; ocamloptflags; _} = dc in
  let target = relative ~dir (name ^ ".exe") in
  Rule.create ~targets:[target] (
    file_words (relative ~dir (libname ^ ".libdeps")) *>>= fun libs ->

    let default_deps_for_required_libs =
      (* To avoid cycles, this mustn't use x_libs etc which are added below *)
      LL.liblink_default ~libs (* for inline test/bench etc *)
    in

    let libs = libs @ [libname] in (* Add the library built by the current dir *)
    x_libs *>>= fun x_libs ->
    let libs = remove_dups_preserve_order (libs @ x_libs) in
    let libs = at_head "pa_ounit_lib" libs in
    let libs = at_head "pa_bench_lib" libs in

    let main_cmx = relative ~dir (prefixed_name ^ ".cmx") in
    let main_o = relative ~dir (prefixed_name ^ ".o") in

    let suffixes_for_lib = [".cmxa"; ".a"; ".stub.names"] in
    let libdeps = LL.liblink_deps ~libs ~suffixes:suffixes_for_lib in

    let libname_deps =
      Dep.path (relative ~dir (sprintf "lib%s_stubs.a" libname)) :: (*HACK*)
      List.map suffixes_for_lib ~f:(fun suf ->
        Dep.path (suffixed ~dir libname suf)
      )
    in
    let deps =
      default_deps_for_required_libs @
      libname_deps @ libdeps @ [Dep.path main_cmx; Dep.path main_o]
    in
    let deps = ocaml_bin_dep :: deps in
    let cmxa_for_packs = List.map ocamlpacks ~f:(fun name -> name ^ ".cmxa") in
    let lib_cmxas =
      List.concat_map libs ~f:(fun lib ->
        let include_dir =
          if lib = libname (* special handling for this lib *)
          then "."
          else dotdot ~dir (LL.liblink_dir ~lib)
        in
        ["-I"; include_dir; lib ^ ".cmxa";]
      )
    in
    Dep.both
      (ocaml_plugin_handling dc ~dir name (name ^ ".exe"))
      (Dep.both
         (LL.liblinks_stubs ~libs)
         (Dep.all_unit deps))
    *>>= fun ((ocaml_plugin_check, ocaml_plugin_command_line), ((), ())) ->
    return (
      bash ~dir (
        String.concat ~sep:" " (List.concat [
          [dotdot ~dir link_quietly;
           dotdot ~dir ocamlwrapper;
           ocamlopt_prog];
          ocaml_plugin_command_line;
          ocamlflags; ocamloptflags;
          ["-cc"; "g++"];
          cmxa_for_packs;
          lib_cmxas;
          [basename main_cmx;
           "-o"; basename target];
          [ocaml_plugin_check];
        ])
      )
    )
  )

let inline_tests_rules dc ~dir =
  let {DC. configured_libs; _} = dc in
  match configured_libs with
  | _ :: _ :: _ -> failwith "multiple libraries in one directory isn't properly supported"
  | [] -> []
  | [(`Name libname, `Inline_tests_deps user_deps)] ->
    let name = "inline_tests_runner" in
    let prefixed_name = prefix_name ~libname ~name in
    let exe_rule =
      let x_libs = return [] in
      link_test_or_bench_exe dc ~dir ~libname ~x_libs name
    in
    List.concat [
      (* generate .ml *)
      inline_tests_gen_file_rules ~dir ~libname;
      (* compile *)
      compile_inline_tests_runner_rule dc ~dir ~libname;
      (* link *)
      [exe_rule];
      (* aliases *)
      [
        Rule.alias (alias_for_inline_runners ~dir) [
          has_tests ~dir *>>= function
          | false -> return ()
          | true ->
            let names =
              [prefixed_name ^ ".cmx" ]
              @ (if link_executables then [name; name ^ ".exe"] else [])
            in
            Dep.all_unit (List.map names ~f:(fun name -> Dep.path (relative ~dir name)))
        ];
        Rule.alias (Alias.runtest ~dir) [
          has_tests ~dir *>>= function
          | false -> return ()
          | true -> run_inline_tests_action ~user_deps ~dir
        ];
      ]
    ]

let inline_bench_rules dc ~dir =
  let {DC. configured_libs; _} = dc in
  match configured_libs with
  | _ :: _ :: _ -> failwith "multiple libraries in one directory isn't properly supported"
  | [] -> []
  | [(`Name libname, `Inline_tests_deps _)] ->
    let name = "inline_benchmarks_runner" in
    let prefixed_name = prefix_name ~libname ~name in
    let exe_rule =
      let x_libs =
        (* delay the exception thrown by not finding inline_benchmarks
           until something actually needs the rule that uses it *)
        return () *>>= fun () -> libdeps_for dc "inline_benchmarks"
      in
      link_test_or_bench_exe dc ~dir ~libname ~x_libs name
    in
    List.concat [
      (* generate .ml *)
      inline_bench_gen_file_rules ~dir ~libname;
      (* compile *)
      compile_inline_benchmarks_runner_rule dc ~dir ~libname;
      (* link *)
      [ exe_rule ];
      (* aliases *)
      [
        Rule.alias (alias_for_inline_runners ~dir) [
          has_benchmarks dc ~dir *>>= function
          | false -> return ()
          | true ->
            let names =
              [prefixed_name ^ ".cmx" ]
              @ (if link_executables then [name; name ^ ".exe"] else [])
            in
            Dep.all_unit (List.map names ~f:(fun name -> Dep.path (relative ~dir name)))
        ];
        Rule.alias (Alias.runbench ~dir) [
          has_benchmarks dc ~dir *>>= function
          | false -> return ()
          | true -> run_inline_bench_action ~user_deps:[] ~dir
        ];
      ]
    ]

(*----------------------------------------------------------------------
 Jbuild
----------------------------------------------------------------------*)

module Jbuild : sig

  type rep = [
  | `preprocessor of Preprocessor_conf.t
  | `library of Library_conf.t
  | `libraryX of LibraryX_conf.t
  | `executables of Executables_conf.t
  | `embed of Embed_conf.t
  | `compile_c of Compile_c_conf.t
  | `rule of Rule_conf.t
  | `alias of Alias_conf.t
  | `no_mycaml
  | `no_utop
  | `Synced_with_omakefile_with_digest of string
  ]
  with of_sexp
  type t
  val rep : t -> rep
  val load : Path.t -> t list Dep.t

end = struct

  type rep = [
  | `preprocessor of Preprocessor_conf.t
  | `library of Library_conf.t
  | `libraryX of LibraryX_conf.t
  | `executables of Executables_conf.t
  | `embed of Embed_conf.t
  | `compile_c of Compile_c_conf.t
  | `rule of Rule_conf.t
  | `alias of Alias_conf.t
  | `no_mycaml
  | `no_utop
  | `Synced_with_omakefile_with_digest of string
  ]
  with of_sexp
  type t = rep with of_sexp
  let rep t = t

  let load path =
    read_then_convert_string_via_reader
      ~path
      ~contents:Dep.contents
      ~do_read:(fun reader ->
        Pipe.to_list (Reader.read_sexps reader)
        >>| List.map ~f:t_of_sexp
      )

end

module User_or_gen_config : sig

  val load : dir: Path.t -> Jbuild.t list Dep.t
  val libnames : dir: Path.t -> string list Dep.t

end = struct

  let load ~dir =
    let jbuild = relative ~dir "jbuild" in
    Dep.file_exists jbuild *>>= function
    | true -> Jbuild.load jbuild
    | false -> return []

  let the_real_libnames_for_libmap j =
    match Jbuild.rep j with
    | `preprocessor x -> [Preprocessor_conf.name x]
    | `library x -> [Library_conf.name x]
    | `libraryX x -> [LibraryX_conf.name x]
    | `executables _ -> []
    | `embed _ -> []
    | `compile_c _ -> []
    | `rule _ -> []
    | `alias _ -> []
    | `no_mycaml -> []
    | `no_utop -> []
    | `Synced_with_omakefile_with_digest _ -> []

  let libnames ~dir =
    load ~dir *>>| List.concat_map ~f:the_real_libnames_for_libmap

end

(*----------------------------------------------------------------------
 generate_dep_rules
----------------------------------------------------------------------*)

let ocaml_libraries j = match Jbuild.rep j with
  | `preprocessor x -> Preprocessor_conf.libraries x
  | `library x -> Library_conf.libraries x
  | `libraryX _ -> []
  | `executables x -> x.Executables_conf.libraries
  | `embed _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_mycaml -> []
  | `no_utop -> []
  | `Synced_with_omakefile_with_digest _ -> []

let xlibnames j = match Jbuild.rep j with
  | `preprocessor x -> [Preprocessor_conf.name x]
  | `library x -> [Library_conf.name x]
  | `libraryX x -> [LibraryX_conf.name x]
  | `executables { Executables_conf.names; _ } -> [fake_libname_of_exes names]
  | `embed _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_mycaml -> []
  | `no_utop -> []
  | `Synced_with_omakefile_with_digest _ -> []

let extra_disabled_warnings j = match Jbuild.rep j with
  | `preprocessor _ -> []
  | `library x -> x.Library_conf.extra_disabled_warnings
  | `libraryX _ -> []
  | `executables x -> x.Executables_conf.extra_disabled_warnings
  | `embed _ -> []
  | `compile_c _ -> []
  | `rule _ -> []
  | `alias _ -> []
  | `no_mycaml -> []
  | `no_utop -> []
  | `Synced_with_omakefile_with_digest _ -> []

let generate_dep_rules dc ~dir jbuilds =
  List.concat_map jbuilds ~f:(fun jbuild ->
    List.concat_map (xlibnames jbuild) ~f:(fun libname ->
      let libs = ocaml_libraries jbuild in
      gen_libdeps dc ~dir ~libs libname
    )
  )

let check_jbuild_and_omakefile_are_in_sync ~dir jbuilds =
  (* [expected_digest_opt] can be None when there is no jbuild, or when there is a jbuild
     but it doesn't contain a digest. The omakefile may not exist either. *)
  let expected_digest_opt =
    let digests =
      List.filter_map jbuilds ~f:(fun jbuild ->
        match Jbuild.rep jbuild with
        | `Synced_with_omakefile_with_digest digest -> Some digest
        | _ -> None)
    in
    match digests with
    | [] -> None
    | [v] -> Some v
    | _ :: _ :: _ ->
      failwith "Cannot specify Synced_with_omakefile_with_digest several times"
  in
  let omakefile = relative ~dir "OMakefile" in
  Dep.file_exists omakefile *>>= function
  | false ->
    begin match expected_digest_opt with
    | None -> return ()
    | Some _digest ->
      failwithf "%s/jbuild specifies a digest but there is no corresponding OMakefile"
        (Path.to_string dir) ()
    end
  | true ->
    match expected_digest_opt with
    | None ->
      begin Jbuild.load (relative ~dir "jbuild.gen") *>>= function
      | [] -> return ()
      | t :: rest ->
        match Jbuild.rep t, rest with
        | `Synced_with_omakefile_with_digest _, [] -> return ()
        | _ ->
          failwithf "%s/jbuild does not exist, \
                     or does not specify a digest for its corresponding OMakefile"
            (Path.to_string dir) ()
      end
    | Some expected_digest ->
      Dep.contents omakefile *>>= fun contents ->
      let actual_digest = Caml.Digest.to_hex (Caml.Digest.string contents) in
      if actual_digest <> expected_digest then
        failwithf
          "%s/OMakefile was changed without updating the jbuild (actual digest: %s)"
          (Path.to_string dir) actual_digest ()
      else return ()
;;

(*----------------------------------------------------------------------
 gen_build_rules
----------------------------------------------------------------------*)

let gen_build_rules dc ~dir jbuilds =
  (* Use cflags/ocamlflags from any library_conf for use in $-var expansion for rule
     conf. *)
  let cflags =
    match (
      List.concat_map jbuilds ~f:(fun j ->
        match Jbuild.rep j with `library x -> [x] | _ -> []
      )
    ) with
    | [] -> default_cflags
    | conf::_ -> Library_conf.cflags conf
  in
  let ocamlflags =
    match (
      List.concat_map jbuilds ~f:(fun j ->
        match Jbuild.rep j with `library x -> [x] | _ -> []
      )
    ) with
    | [] -> dc.DC.ocamlflags
    | conf::_ -> Library_conf.ocamlflags conf
  in
  List.concat_map jbuilds ~f:(fun j ->
    match Jbuild.rep j with
    | `preprocessor conf -> preprocessor_rules dc ~dir conf
    | `library conf -> library_rules dc ~dir ~cflags conf
    | `libraryX conf -> libraryX_rules dc ~dir ~cflags conf
    | `executables conf -> executables_rules dc ~dir conf
    | `embed conf -> embed_rules ~dir ~cflags conf
    | `compile_c conf -> user_configured_compile_c_rules ~dir conf
    | `rule conf ->
      let dc_for_rule_conf = {dc with DC. ocamlflags} in
      [rule_conf_to_rule dc_for_rule_conf ~dir ~cflags conf]
    | `alias conf ->
      let dc_for_rule_conf = {dc with DC. ocamlflags} in
      [alias_conf_to_rule dc_for_rule_conf ~dir ~cflags conf]
    | `no_mycaml -> []
    | `no_utop -> []
    | `Synced_with_omakefile_with_digest _ -> []
  )

(*----------------------------------------------------------------------
 directory_rules
----------------------------------------------------------------------*)

let directory_rules dc ~dir jbuilds =
  List.concat [
    [
      Rule.alias (Alias.lib_artifacts ~dir) [];
      Rule.default ~dir [Dep.alias (Alias.lib_artifacts ~dir)];
    ];
    gen_build_rules dc ~dir jbuilds;
    generate_dep_rules dc ~dir jbuilds;
    [hg_version_out_rule ~dir];
    inline_tests_rules dc ~dir;
    inline_bench_rules dc ~dir;
    merlin_rules dc ~dir;
  ]

(*----------------------------------------------------------------------
 libmap.sexp
----------------------------------------------------------------------*)

module Libmap_sexp : sig

  val setup : Scheme.t
  val get : Libmap.t Dep.t

end = struct

  type t = (string * Path.t) list
  with sexp

  let libmap_sexp_rule ~dir =
    let target = relative ~dir "libmap.sexp" in
    Rule.create ~targets:[target] (
      deep_unignored_subdirs ~dir *>>= fun dirs ->
      Dep.all (
        List.map dirs ~f:(fun dir ->
          User_or_gen_config.libnames ~dir *>>| fun libnames ->
          List.map libnames ~f:(fun name -> (name,dir))
        )
      ) *>>| fun xs ->
      let libmap = List.concat xs in
      let sexp = sexp_of_t libmap in
      write_string_action (Sexp.to_string_hum sexp) ~target
    )

  let setup =
    Scheme.create ~tag:"libmap.sexp" (fun ~dir ->
      return [libmap_sexp_rule ~dir]
    )

  let load path =
    read_then_convert_string_via_reader
      ~path
      ~contents:Dep.contents_cutoff
      ~do_read:(fun reader ->
        Reader.read_sexp reader >>| function
        | `Ok sexp -> t_of_sexp sexp
        | `Eof -> failwith "Eof"
      )

  let root_libmap = root_relative "libmap.sexp"

  let get =
    load root_libmap *>>| Libmap.create_exn

end

(*----------------------------------------------------------------------
 setup_liblinks
----------------------------------------------------------------------*)

let setup_liblinks =
  Scheme.create ~tag:"liblinks" (fun ~dir ->
    Libmap_sexp.get *>>= fun libmap ->
    LL.rules ~dir libmap
  )

(*----------------------------------------------------------------------
  autogen: determine from rule targets (& ocamllex/yacc)
----------------------------------------------------------------------*)

let filter_drop ~suffix xs =
  List.filter_map xs ~f:(fun x ->
    String.chop_suffix x ~suffix
  )

(* duplicates knowledge here of what .ml/.mli are autogen via ocaml lex/yacc*)

let infer_autogen jbuilds =
  List.concat_map jbuilds ~f:(fun j ->
    match Jbuild.rep j with
    | `rule {Rule_conf. targets; _} ->
      targets

    | `library x ->
      "inline_tests_runner.ml" :: "inline_benchmarks_runner.ml" ::
      List.concat_cartesian_product (Library_conf.ocamllex x) [".ml"]
      @ List.concat_cartesian_product (Library_conf.ocamlyacc x) [".ml"; ".mli"]

    | `executables x ->
      List.concat_cartesian_product x.Executables_conf.ocamllex [".ml"]
      @ List.concat_cartesian_product x.Executables_conf.ocamlyacc [".ml"; ".mli"]

    | `libraryX _ (* I suppose it could produce ml files, but well *)
    | `preprocessor _
    | `embed _
    | `compile_c _
    | `alias _
    | `no_mycaml
    | `no_utop
    | `Synced_with_omakefile_with_digest _
      -> []
  )

(*----------------------------------------------------------------------
 create_directory_context, setup_main
----------------------------------------------------------------------*)

let create_directory_context ~dir jbuilds =
  (* These dependencies could/should be run in parallel *)
  Centos.ocamllibflags *>>= fun ocamllibflags ->
  Dep.glob_listing (glob_ml ~dir) *>>= fun ml_paths ->
  Dep.glob_listing (glob_mli ~dir) *>>= fun mli_paths ->
  Libmap_sexp.get *>>= fun libmap ->
  let merlinflags =
    let extra_disabled_warnings = List.concat_map jbuilds ~f:extra_disabled_warnings in
    let disabled_warnings = Ocaml_version.disabled_warnings @ extra_disabled_warnings in
    Top.default_merlinflags ~disabled_warnings
  in
  let ocamlflags =
    Top.default_ocamlflags ~disabled_warnings:Ocaml_version.disabled_warnings
  in
  let configured_libs =
    List.concat_map jbuilds ~f:(fun j -> match Jbuild.rep j with
    | `library conf ->
      [(`Name (Library_conf.name conf),
        `Inline_tests_deps (Library_conf.inline_tests_deps conf))]
    | _ -> [])
  in
  let xlibnames =
    List.concat_map jbuilds ~f:xlibnames
  in
  let ocaml_plugin_libraries name =
    List.find_map jbuilds ~f:(fun j -> match Jbuild.rep j with
    | `embed { Embed_conf.names; libraries; _ } ->
      if List.mem names name then Some libraries else None
    | _ -> None
    )
  in
  let no_mycaml_alias =
    List.exists jbuilds ~f:fun j -> match Jbuild.rep j with | `no_mycaml -> true | _ -> false
  in
  let no_utop_alias =
    List.exists jbuilds ~f:fun j -> match Jbuild.rep j with | `no_utop -> true | _ -> false
  in
  let autogen_raw = infer_autogen jbuilds in
  let generated_modules =
    List.filter autogen_raw ~f:(fun base ->
      String.is_suffix base ~suffix:".ml" || String.is_suffix base ~suffix:".mli")
  in
  let autogen_for_c_ish_compilation =
    List.filter autogen_raw ~f:(fun base ->
      String.is_suffix base ~suffix:".c" || String.is_suffix base ~suffix:".h")
  in

  let impls =
    (* Sort, so list order is stable when autogen files appear,
       and so action is unchanged *)
    remove_dups_and_sort
      (filter_drop ~suffix:".ml" (List.map ml_paths ~f:basename @ generated_modules))
  in
  let intfs =
    remove_dups_and_sort
      (filter_drop ~suffix:".mli" (List.map mli_paths ~f:basename @ generated_modules))
  in
  let impl_is_buildable = mem_of_list impls in
  let intf_is_buildable = mem_of_list intfs in

  let dc = {DC.
    dir;
    ocamllibflags;
    merlinflags;
    ocamlflags;
    ocamlcflags = Top.ocamlcflags;
    ocamloptflags = Top.ocamloptflags;
    configured_libs;
    xlibnames;
    ocaml_plugin_libraries;
    no_mycaml_alias;
    no_utop_alias;
    libmap;
    autogen = autogen_for_c_ish_compilation;
    impls;
    intfs;
    impl_is_buildable;
    intf_is_buildable;
  }
  in
  return dc

let setup_main =
  (* Tag renamed: main -> main2 to workaround incorrect stale build artifact removal of
     libmap.sexp which used to be in this scheme but has now moved to a separate scheme *)
  Scheme.create ~tag:"main2" (fun ~dir ->
    User_or_gen_config.load ~dir *>>= fun jbuilds ->
    (* If the check breaks, it will break as soon as the jbuild is loaded. It prevents
       building any files in a directory that is required by the current targets without
       first updating its jbuild, but implies that errors are given as soon as possible. *)
    check_jbuild_and_omakefile_are_in_sync ~dir jbuilds *>>= fun () ->
    create_directory_context ~dir jbuilds *>>= fun dc ->
    gen_recursive_aliases ~dir *>>| fun recursive_aliases ->
    recursive_aliases
    @ directory_rules dc ~dir jbuilds
  )

(*----------------------------------------------------------------------
 env
----------------------------------------------------------------------*)

let putenv = (* setup external actions *)
  [
    ("CAML_LD_LIBRARY_PATH",
     Path.to_absolute_string (relative ~dir:the_root_lib_dir "typehash"));
    (* /tmp partitions are small, which causes issues (for instance many simultaneous
       ocaml-plugin runs can go over the 1GB limit, especially if /tmp already contains
       stuff). So we stick the tmp directory on the local disk, which is much harder to
       fill, and it makes admins happy. *)
    ("TMPDIR", Path.to_absolute_string (relative ~dir:Path.the_root tmpdir));
  ]

let call_hg_showconfig_to_trigger_dirstate_change () =
  (* We rely on this action not touching the dirstate file in the case it is unchanged.
     If it did, we could not unconditionally call this function from build_end without
     causing a continuously looping build *)
  run_action_now (bash ~dir:Path.the_root "hg showconfig > /dev/null")

let build_begin () =
  don't_wait_for (call_hg_showconfig_to_trigger_dirstate_change ());
  (* try to recreate the directory every time in case someone wipes it away from under a
     polling jenga *)
  Unix.mkdir ~p:() tmpdir

let build_end () =
  don't_wait_for (call_hg_showconfig_to_trigger_dirstate_change ());
  Deferred.unit

let deterministic_ranlib =
  (* Intercept calls made to "ranlib" from the ocaml compiler, and fix them to be
     deterministic by adding a 'D' modifier.
     - i.e. calling "ar -Ds" instead of "ar -s"

     We also now intercept calls to ar (from the ocaml compiler).
     And convert: "ar rc" -> "ar Drc"
  *)
  Path.to_absolute_string (root_relative "bin/deterministic-ranlib")

let env =
  Env.create
    ~putenv
    ~command_lookup_path:(
      `Replace [
        ".";
        deterministic_ranlib;
        Path.to_absolute_string (root_relative "bin/cpp_quietly");
        ocaml_bin;
      ]
    )
    ~build_begin
    ~build_end
    [
      ".omake-ocaml-bin"                    , Some setup_dot_ocaml_bin;
      "**jbuild-ignore"                     , None;
      "**jbuild"                            , None;
      "**OMakefile"                         , None;
      "bin/*"                               , None;
      "**/.hg/**"                           , None;
      "**/jbuild.gen"                       , Some setup_jbuild_generated;
      "libmap.sexp"                         , Some Libmap_sexp.setup;
      "lib/*/*"                             , Some setup_liblinks;
      "**"                                  , Some setup_main;
    ]

let setup () =
  Deferred.return env
