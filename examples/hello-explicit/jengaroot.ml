
open Async.Std
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map

let simple_rule ~targets ~deps ~action =
  (* declare simple rule with static dependencies & fixed action *)
  Rule.create ~targets (
    Dep.all_unit deps *>>| fun () ->
    action
  )

let bash ~dir command =
  (* wrap actions with bash for convenience *)
  Action.shell ~dir ~prog:"bash" ~args:["-c"; command]

let scheme =
  Scheme.create ~tag:"the-scheme" (fun ~dir ->
    let file s = Path.relative ~dir s in
    let need s = Dep.path (file s) in
    let bash = bash ~dir in
    return [

      simple_rule
        ~targets:[file "world.cmi"]
        ~deps:[need "world.mli"]
        ~action:(bash "ocamlopt.opt -c world.mli");

      simple_rule
        ~targets:[file "world.o"; file "world.cmx"]
        ~deps:[need "world.ml"; need "world.cmi";]
        ~action:(bash "ocamlopt.opt -c world.ml");

      simple_rule
        ~targets:[file "hello.o"; file "hello.cmx"; file "hello.cmi"]
        ~deps:[need "hello.ml"; need "world.cmi"; need "world.cmx"]
        ~action:(bash "ocamlopt.opt -c hello.ml");

      simple_rule
        ~targets:[file "hello.exe"]
        ~deps:[need "hello.o"; need "hello.cmx"; need "world.o"; need "world.cmx"]
        ~action:(bash "ocamlopt.opt world.cmx hello.cmx -o hello.exe");

      simple_rule
        ~targets:[file "test.out"]
        ~deps:[need "hello.exe"]
        ~action:(bash "./hello.exe > test.out");

      Rule.default ~dir [need "test.out"];
    ]
  )

let env = Env.create ["**", Some scheme]
let setup () = Deferred.return env
