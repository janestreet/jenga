
open Async.Std
open Jenga_lib.Api

(* Example of using jenga to setup a fixed set of rules for a very small example.
   Shows the API functions which construct values of the various types:

       Path.t
       Dep.t
       Action.t
       Rule.t
       Rule_generator.t
       Rule_scheme.t
       Env.t

   This is rather a-typical for how jenga is expected to be used. Normally JengaRoot would
   setup rules in a more automated/programmatic fashion - rather than having each rule be
   painfully constructed in the bare API. See other examples...


   One thing this example does show is that jenga expects a very accurate description of
   the targets & dependencies of each rule...

   In this example, we are careful to list .o files as well as .cmx

   Also note the difference between the rules which compile hello.ml and world.ml - the
   difference due to the existence of world.mli - so world.cmi is a dependency of
   world.cmx/o - as opposed to being a further sibling target, as it is in the case of
   hello.cmi/cmx/o

*)

let hello_scheme =
  Rule_scheme.create ~tag:"hello-scheme" (fun ~dir ->
    Rule_generator.create ~deps:[] ~gen:(fun () ->
      let file s = Path.relative ~dir s in
      let dep s = Dep.path (file s) in
      return [

        Rule.create
          ~targets:[file "world.cmi"]
          ~deps:[dep "world.mli"]
          ~action:(
            Action.shell ~dir
              ~prog:"ocamlopt.opt" ~args:["-c"; "world.mli"]
          );

        Rule.create
          ~targets:[file "world.o"; file "world.cmx"]
          ~deps:[dep "world.ml"; dep "world.cmi";]
          ~action:(
            Action.shell ~dir
              ~prog:"ocamlopt.opt" ~args:["-c"; "world.ml"]
          );

        Rule.create
          ~targets:[file "hello.o"; file "hello.cmx"; file "hello.cmi"]
          ~deps:[dep "hello.ml"; dep "world.cmi"]
          ~action:(
            Action.shell ~dir
              ~prog:"ocamlopt.opt" ~args:["-c"; "hello.ml"]
          );

        Rule.create
          ~targets:[file "hello.exe"]
          ~deps:[dep "hello.o"; dep "hello.cmx"; dep "world.o"; dep "world.cmx"]
          ~action:(
            Action.shell ~dir
              ~prog:"ocamlopt.opt" ~args:["world.cmx"; "hello.cmx"; "-o"; "hello.exe"]
          );

        Rule.default ~dir [Dep.path (file "hello.exe")];

      ]
    )
  )


let env = Env.create [
  "*", Some hello_scheme
]

let setup () = return env
