open Core
open! Int.Replace_polymorphic_compare

module Q : sig

  val shell_escape : string -> string
  val shell_escape_list : string list -> string

end = struct

  let shell_escape = Sys.quote
  let shell_escape_list l =
    String.concat ~sep:" " (List.map l ~f:shell_escape)

end

let pretty_span span =
  let { Time.Span.Parts.sign = _; hr; min; sec; ms; us = _; ns = _ } =
    Time.Span.to_parts span
  in
  let mins = 60 * hr + min in
  if mins > 0     then sprintf "%dm %02ds" mins sec
  else if sec > 0 then sprintf "%d.%03ds"  sec ms
  else                 sprintf "%dms"      ms
;;

let parse_pretty_span span =
  match String.lsplit2 ~on:' ' span with
  | None -> Time.Span.of_string span
  | Some (minutes, seconds) -> Time.Span.(of_string minutes + of_string seconds)
;;

let%test_unit _ =
  List.iter ~f:(fun str -> [%test_result: string] ~expect:str
                             (pretty_span (parse_pretty_span str)))
    [ "1m 44s"
    ; "23.123s"
    ; "55ms"
    ]
;;

module Stable = struct
  open! Core.Core_stable

  module V2 = struct

    module Start = struct

      type t = {
        uid : int; (* to line up commands in .jenga/debug *)
        need : string;
        where : string;
        prog : string;
        args : string list;
        sandboxed : bool;
      } [@@deriving bin_io, fields, sexp_of]

    end

    module Finish = struct
      type t = {
        outcome : [`success | `error of string];
        duration : Time.Span.V2.t;
      } [@@deriving bin_io, fields, sexp_of]
    end

    module Output = struct
      type t = {
        stdout : string list;
        stderr : string list;
      } [@@deriving bin_io, fields, sexp_of]
    end

    type t = Start.t * Finish.t * Output.t
    [@@deriving bin_io, sexp_of]

  end

  module V1 = struct

    module Start = struct

      type t = {
        uid : int; (* to line up commands in .jenga/debug *)
        need : string;
        where : string;
        prog : string;
        args : string list;
      } [@@deriving bin_io, fields, sexp_of]

      let upgrade { uid; need; where; prog; args } =
        let sandboxed = false in
        { V2.Start.uid; need; where; prog; args; sandboxed }
    end

    module Finish = V2.Finish

    module Output = V2.Output

    type t = Start.t * Finish.t * Output.t
    [@@deriving bin_io, sexp_of]

    let upgrade (start, finish, output) =
      let start = Start.upgrade start in
      (start, finish, output)

  end

  let%expect_test _ =
    print_endline [%bin_digest: V1.t];
    [%expect {| 7b41a82c166551d32016599fffdd3994 |} ]

  let%expect_test _ =
    print_endline [%bin_digest: V2.t];
    [%expect {| 33cc6bf41461f1ef020cb4680125893c |} ]

  type model = V2.t

end

module Start = struct
  include Stable.V2.Start

  let create =
    let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
    fun ~need ~where ~prog ~args ~sandboxed ->
      let uid = genU() in
      { uid; need; where; prog; args; sandboxed }
end

module Finish = Stable.V2.Finish

module Output = Stable.V2.Output

type t = Stable.V2.t
[@@deriving sexp_of]

let split_string_into_lines s =
  match s with
  | "" -> []
  | "\n" -> [""]
  | _ ->
    let s =
      match String.chop_suffix s ~suffix:"\n" with
      | None -> s
      | Some s -> s
    in
    String.split s ~on:'\n'

let create start ~outcome ~duration ~stdout ~stderr =
  let finish = { Finish. outcome; duration } in
  let stdout = split_string_into_lines stdout in
  let stderr = split_string_into_lines stderr in
  let output = { Output. stdout; stderr } in
  (start,finish,output)

let outcome (_,f,_) = f.Finish.outcome

let mk_build_message ~where ~need ~sandboxed =
  let sandboxed = if sandboxed then " (sandboxed)" else "" in
  sprintf "- build %s %s%s" where need sandboxed

let mk_command_message ~prog ~args =
  (* print out the command in a format suitable for cut&pasting into bash
     (except for the leading "+") *)
  let args = List.map args ~f:(fun arg -> Q.shell_escape arg) in
  sprintf "+ %s %s" prog (String.concat ~sep:" " args)

let mk_status_message ~outcome =
  match outcome with
  | `success -> "code 0"
  | `error status_string -> status_string

let build_message ({Start.where;need; sandboxed; _},_,_) =
  mk_build_message ~where ~need ~sandboxed
let command_message ({Start.prog; args; _},_,_) = mk_command_message ~prog ~args
let status_message (_,{Finish.outcome; _},_) = mk_status_message ~outcome

let to_stdout_lines (_,_,o) = o.Output.stdout
let to_stderr_lines (_,_,o) = o.Output.stderr

let iter_lines (
  {Start. where; need; prog; args; sandboxed; uid=_},
  {Finish. outcome; duration},
  {Output. stdout; stderr}
) ~f:put =
  put (mk_build_message ~where ~need ~sandboxed);
  put (mk_command_message ~prog ~args);
  List.iter stdout ~f:put;
  List.iter stderr ~f:put;
  let duration_string = pretty_span duration in
  let status_string = mk_status_message ~outcome in
  put (sprintf "- exit %s %s, %s, %s" where need duration_string status_string)

let to_lines t =
  let collected = ref [] in
  iter_lines t ~f:(fun s -> collected := s :: !collected);
  List.rev !collected
