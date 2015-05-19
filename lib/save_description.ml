
open Core.Std
open Async.Std

type t = {
  contents : string;
  target : Path.t;
  chmod_x : bool;
} with sexp_of

let create ~contents ~target ~chmod_x = { contents; target; chmod_x; }

let escape_backslashes_and_newlines s =
  String.concat_map s ~f:(function
  | '\n' -> "\\n"
  | '\\' -> "\\\\"
  | c -> String.make 1 c)

let bashf ~dir fmt =
  ksprintf (fun s -> Job.create ~dir ~prog:"bash" ~args:["-c"; s]) fmt

let job {contents;target;chmod_x} =
    (* By escaping newlines & using "echo -e", we avoid embedded newlines which are tricky
       to escape if we later extract a Makefile *)
  let base = Message.Q.shell_escape (Path.basename target) in
  bashf ~dir:(Path.dirname target)
    "echo -n -e %s > %s%s"
    (Message.Q.shell_escape (escape_backslashes_and_newlines contents))
    base
    (if chmod_x then sprintf "; chmod +x %s" base else "")

let run {contents;target;chmod_x} =
  let perm = if chmod_x then Some 0o777 else None in
  Effort.track Progress.saves_run (fun () ->
    File_access.enqueue (fun () ->
      Writer.save ?perm (Path.to_string target) ~contents
    ))
