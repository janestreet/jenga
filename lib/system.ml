open Core
open! Int.Replace_polymorphic_compare

type t = Linux | Darwin | Dumb | Unexpected of string

let to_string = function
  | Linux -> "Linux"
  | Darwin -> "Darwin"
  | Dumb -> "Dumb"
  | Unexpected s -> sprintf "Unexpected(%s)" s

let of_string = function
  | "Linux" -> Linux
  | "Darwin" -> Darwin
  | "Dumb" -> Dumb
  | s -> Unexpected s

let auto_determined =
  of_string (Unix.Utsname.sysname (Unix.uname()))

let system =
  match Core.Sys.getenv "JENGA_SYSTEM" with
  | Some x -> of_string x
  | None -> auto_determined

let description = to_string system

let num_cpus_if_known =
  match system with
  | Unexpected _ -> None
  | Darwin -> None
  | Dumb -> None
  | Linux ->
    let num_cpus =
      let err fmt = ksprintf (fun s -> Printf.eprintf "%s\n%!" s) fmt in
      let path = "/proc/cpuinfo" in
      let lines =
        try
          let module IC = Core.In_channel in
          IC.with_file path ~f:IC.input_lines
        with
        | _ -> err "cant read: %s" path; []
      in
      match List.filter lines ~f:(String.is_prefix ~prefix:"processor")  with
      | [] -> err "no processor lines seen in: %s" path; 1
      | lines -> List.length lines
    in
    Some num_cpus

let has_inotify =
  match system with
  | Unexpected _ -> false
  | Dumb -> false
  | Darwin -> false
  | Linux -> true
