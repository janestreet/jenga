
open Core
open! Int.Replace_polymorphic_compare

(* copied from some old code -- dev/repo_utils/lib/glob.ml *)

let quote char = Str.quote (sprintf "%c" char)

let convert_unanchored s =
  let group = ref 0 in (*Parenthesis level*)
  let len = String.length s in
  let res = Buffer.create len in
  let (^=) = Buffer.add_string in
  let rec aux pos =
    if pos < len then
      match s.[pos] with
      | '\\' ->
          if (pos+1) < len then
            res ^= (quote (s.[pos+1]));
            aux (pos+2)
      | '*' ->
          if (pos+1) < len && Char.(=) s.[pos+1] '*' then begin
            res ^= ".*";
            aux (pos+2)
          end else begin
            (if pos = 0
             (* special case for leading * to not match hidden . filename
                also wont match the empty string.
             *)
             then res ^= "[^/.][^/]*"
             else res ^= "[^/]*"
            );
            aux (pos+1)
          end
      | '?' ->
        res ^= "[^/]";
        aux (pos+1)
      | ',' when !group > 0 ->
        res ^= "\\|";
        aux (pos+1);
      | '{' ->
        res ^= "\\(";
        incr group;
          aux (pos+1)
      | '}' ->
          if !group = 0 then
            failwith "closing unopened { in glob";
          res ^= "\\)";
          decr group;
          aux (pos+1)
      | '[' ->
          res ^= "[";
          let rec parse i =
            if i = len then
              failwith "unclosed [ in glob";
            match s.[i] with
            | '\\' ->
                res ^= "\\\\";
                parse (i+1)
            | '!' when i = (pos+1) ->
                res ^= "^";
                parse (i+1)
            | '^' when i = (pos+1) ->
                res ^= "\\^";
                parse (i+1)
            | ']' -> i+1
            | c ->
                Buffer.add_char res c;
                parse (i+1)
          in
          let pos = parse (pos+1) in
          res ^= "]";
          aux pos
      | c ->
          let s = quote c in
          res ^= s;
          aux (pos+1)
  in
  aux 0;
  if !group <> 0 then
    failwith "unclosed { in glob";
  Buffer.contents res

let%test_unit _ =
  [%test_result: string] (convert_unanchored "{a,b}") ~expect:"\\(a\\|b\\)"

let%test_unit _ =
  [%test_result: string] (convert_unanchored "{*.[ch],*.cpp,*.hh}")
    ~expect:"\\([^/]*\\.[ch]\\|[^/]*\\.cpp\\|[^/]*\\.hh\\)"
