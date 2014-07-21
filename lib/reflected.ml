
open Core.Std

module Action = struct

  type t = Job.t with sexp_of

  let dir t = Job.dir t

  let string_for_sh t = Job.string_for_sh t

  let escape_newlines_for_make s =
    if not (String.contains s '\n') then s else
      let s =
        String.concat_map s ~f:(function
        | '\n' -> "\\n"
        | '\\' -> "\\\\"
        | c -> String.make 1 c)
      in
      sprintf "bash -c \"$(echo -e %s)\"" (Message.Q.shell_escape ~arg:s)

  let escape_dollars_for_make s =
    if not (String.contains s '$') then s else
      String.concat_map s ~f:(function
      | '$' -> "$$"
      | c -> String.make 1 c)

  let string_for_one_line_make_recipe action =
    let s = string_for_sh action in
    let s = escape_newlines_for_make s in
    let s = escape_dollars_for_make s in
    s

end

module Trip = struct
  type t = {
    targets: Path.t list;
    deps : Path.t list;
    action : Action.t;
  } with sexp_of
  let to_string t = Sexp.to_string (sexp_of_t t)
end

