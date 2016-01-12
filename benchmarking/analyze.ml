#!/j/office/app/jane-script/prod/113.11/jane-script run
open Core.Std
open Async.Std

(* copy&pasted from lib/message.ml *)
let parse_pretty_span span =
  match String.lsplit2 ~on:' ' span with
  | None -> Time.Span.of_string span
  | Some (minutes, seconds) -> Time.Span.(of_string minutes + of_string seconds)
;;

(* copy&pasted from lib/message.ml *)
let parse_build_measures_assoc_list =
  let parse =
    let open Re2.Std.Parser in
    Staged.unstage (compile (
      or_ [string "done"; string "failed"]
      *> string " ("
      *> capture (
        string "#"
        *> ignore Decimal.int
        *> string ", "
        *> repeat (ignore Char.any)
      )
      <* string ")"
      <* ignore (optional (string " -- HURRAH"))))
  in
  (fun str ->
     Option.map (parse str)
       ~f:(fun csv ->
         List.map (String.split ~on:',' csv) ~f:(fun value ->
           let value = String.strip value in
           match String.lsplit2 ~on:'=' value with
           | Some (key, value) -> (key, value)
           | None -> ("", value)
         )))

let to_r_expression : float list -> string =
  fun l -> "c (" ^ String.concat ~sep:"," (List.map l ~f:Float.to_string) ^ ")"

let t_test_r_expression x y =
  "{x = " ^ to_r_expression x ^ ";" ^ "y = " ^ to_r_expression y ^ "; t.test(x,y)}"

let t_test_bash_command x y =
  sprintf !"Rscript <(cat <<< %{Filename.quote})" (t_test_r_expression x y)

let parse_line =
  (fun s ->
     match parse_build_measures_assoc_list s with
     | Some (_ :: ("", time) :: _) ->
       Time.Span.to_sec (parse_pretty_span time)
     | _ -> failwith "bad input"
  )

let read x =
  Reader.open_file x
  >>= fun x ->
  Pipe.to_list (Reader.lines x)
  >>| List.map ~f:(parse_line)

let analyze x y =
  read x
  >>= fun x ->
  read y
  >>= fun y ->
  let s = t_test_bash_command x y in
  Nothing.unreachable_code (
    Core.Std.Unix.exec ~prog:"/bin/bash" ~args:["bash"; "-c"; s] ())

let () = Command.run (
  Command.async ~summary:"analyze jenga benchmark results"
    Command.Spec.(empty
     +> anon ((%:) "x" string)
     +> anon ((%:) "y" string)
    )
    (fun x y () -> analyze x y))
