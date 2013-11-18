
open Core.Std

(* copied from some old code -- dev/repo_utils/lib/glob.ml
   todo - clean this up.
*)

let quote char = sprintf "%c" char

let globre ~path s =
  let group = ref 0 in (*Parenthesis level*)
  let len = String.length s in
  let res = Buffer.create len in
  let (^=) = Buffer.add_string in
  let rec aux pos =
    if pos < len then
      match s.[pos] with
      | '.' ->
          res ^= "\\.";
          aux (pos+1)
      | '\\' ->
          if (pos+1) < len then
            res ^= (quote (s.[pos+1]));
            aux (pos+2)
      | '*' ->
          if (pos+1) < len &&s.[pos+1] = '*' && path then begin
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
          if (pos+1) < len &&s.[pos+1] = '?' && path then begin
            res ^= ".";
            aux (pos+2)
          end else begin
            res ^= "[^/]";
            aux (pos+1)
          end
      | ',' when !group > 0 ->
          res ^= "|";
          aux (pos+1);
      | '{' ->
          res ^= "(?:";
          incr group;
          aux (pos+1)
      | '}' ->
          if !group = 0 then
            failwith "closing unopened { in glob";
          res ^= ")";
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

let convert_unachored glob = globre ~path:true glob
