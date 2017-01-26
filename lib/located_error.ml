open Core

module Loc = struct
  module Source = struct
    type t =
      | File of Path.t
      | Other of string
    [@@deriving sexp_of]

    let to_string t ~dir =
      match t with
      | Other where -> sprintf "In %s" where
      | File path ->
        (* Build manager wants paths to be relative to the directory
           in the [Reportable.t]. *)
        let file = Path.reach_from path ~dir:(Path.of_relative dir) in
        let file = Option.value (String.chop_prefix file ~prefix:"./") ~default:file in
        sprintf "File %S" file
  end

  type t =
    { source    : Source.t
    ; line      : int
    ; start_col : int
    ; end_col   : int
    } [@@deriving sexp_of]
end

type t = { loc : Loc.t; message : string } [@@deriving sexp_of]

let create ~loc message = { loc; message }
let create' ?(line = 1) ?(start_col = 0) ?(end_col = start_col) ~source message =
  create ~loc:{ source; line; start_col; end_col } message

exception E of t [@@deriving sexp_of]

let raise ~loc message = raise (E { loc; message })
let raisef ~loc fmt = Printf.ksprintf (fun s () -> raise ~loc s) fmt

let to_lines { loc; message } ~dir =
  let s =
    sprintf "%s, line %d, characters %d-%d:\nError: %s\n"
      (Loc.Source.to_string loc.source ~dir) loc.line loc.start_col loc.end_col
      message
  in
  String.split_lines s
