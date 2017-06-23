#!/j/office/app/jane-script/prod/114.00/jane-script run
open Core.Std
open Async.Std

(* copy&pasted from lib/message.ml *)
let parse_pretty_span span =
  match String.lsplit2 ~on:' ' span with
  | None -> Time.Span.of_string span
  | Some (minutes, seconds) -> Time.Span.(of_string minutes + of_string seconds)
;;

let parse_heap h = match String.chop_suffix h ~suffix:"g" with
  | Some s -> Float.of_string s *. 1024.
  | None -> match String.chop_suffix h ~suffix:"m" with
    | Some s -> Float.of_string s
    | None -> failwith "heap needs to be in gigabytes or megabytes"

(* copy&pasted from lib/message.ml *)
let parse_build_measures_assoc_list =
  let module Re = Ocaml_re.Re in
  let regexp =
    Re.(compile
          (seq [ alt [str "done"; str "failed"]
               ; str " ("
               ; group (seq [ str "#"
                            ; opt (set "-+"); rep1 digit
                            ; str ", "
                            ; rep any
                            ])
               ; str ")"
               ; opt (str " -- HURRAH")
               ]))
  in
  (fun str ->
     Option.map (Re.exec_opt regexp str)
       ~f:(fun groups ->
         let csv = Re.Group.get groups 1 in
         List.map (String.split ~on:',' csv) ~f:(fun value ->
           let value = String.strip value in
           match String.lsplit2 ~on:'=' value with
           | Some (key, value) -> (key, value)
           | None -> ("", value)
         ))
  )

let to_r_expression : float list -> string =
  fun l -> "c (" ^ String.concat ~sep:"," (List.map l ~f:Float.to_string) ^ ")"

let t_test_r_expression x y =
  "{x = " ^ to_r_expression x ^ ";" ^ "y = " ^ to_r_expression y ^ "; t.test(x,y)}"

let t_test_bash_command x y =
  sprintf !"Rscript <(cat <<< %{Filename.quote})" (t_test_r_expression x y)

let parse_line =
  (fun s ->
     match parse_build_measures_assoc_list s with
     | Some (_ :: ("", time) :: rest) ->
       (Time.Span.to_sec (parse_pretty_span time), parse_heap (
          Option.value_exn (List.Assoc.find rest ~equal:String.equal "heap")))
     | _ -> failwith "bad input"
  )

let read ~what x =
  Reader.open_file x
  >>= fun x ->
  Pipe.to_list (Reader.lines x)
  >>| List.map ~f:(fun s -> match what with
    | `time -> fst (parse_line s)
    | `heap -> snd (parse_line s)
   )

let analyze ~what x y =
  printf "%s stats:\n" (match what with | `time -> "Time (s)" | `heap -> "Heap (MiB)");
  read ~what x
  >>= fun x ->
  read ~what y
  >>= fun y ->
  let s = t_test_bash_command x y in
  Process.run
    ~prog:"/bin/bash" ~args:["-c"; s] ()
  >>| Or_error.ok_exn
  >>| fun stdout ->
  printf "%s\n" stdout

let () = Command.run (
  Command.async ~summary:"analyze jenga benchmark results"
    Command.Spec.(empty
     +> anon ((%:) "x" string)
     +> anon ((%:) "y" string)
    )
    (fun x y () ->
      analyze ~what:`time x y
      >>= fun () ->
      analyze ~what:`heap x y
    ))

