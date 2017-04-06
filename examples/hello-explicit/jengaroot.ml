
open Async
open Jenga_lib.Api

let bash ~dir command =
  Action.process ~dir ~prog:"bash" ~args:["-c"; command] ()

let scheme ~dir =
  Scheme.rules (
    let file s = Path.relative ~dir s in
    let need s = Dep.path (file s) in
    let bash = bash ~dir in
    [

      Rule.simple
        ~targets:[file "world.cmi"]
        ~deps:[need "world.mli"]
        ~action:(bash "ocamlopt.opt -c world.mli");

      Rule.simple
        ~targets:[file "world.o"; file "world.cmx"]
        ~deps:[need "world.ml"; need "world.cmi";]
        ~action:(bash "ocamlopt.opt -c world.ml");

      Rule.simple
        ~targets:[file "hello.o"; file "hello.cmx"; file "hello.cmi"]
        ~deps:[need "hello.ml"; need "world.cmi"; need "world.cmx"]
        ~action:(bash "ocamlopt.opt -c hello.ml");

      Rule.simple
        ~targets:[file "hello.exe"]
        ~deps:[need "hello.o"; need "hello.cmx"; need "world.o"; need "world.cmx"]
        ~action:(bash "ocamlopt.opt world.cmx hello.cmx -o hello.exe");

      Rule.simple
        ~targets:[file "test.out"]
        ~deps:[need "hello.exe"]
        ~action:(bash "./hello.exe > test.out");

      Rule.default ~dir [need "test.out"];
    ]
  )

let env = Env.create (fun ~dir -> { scheme = scheme ~dir; directories_generated_from = None })
let setup () = Deferred.return env
