open Core
open! Int.Replace_polymorphic_compare

module Action = struct

  type t = Action.t [@@deriving sexp_of]

  let dir t = Action.dir t

  let to_sh_ignoring_dir t = Action.to_sh_ignoring_dir t

  let escape_newlines_for_make s =
    (* Newlines are interpreted by [make] as indicating the end of a command. Multiple
       tab-indented lines are executed as a sequence of commands. Although [make] supports
       escaping of newlines using a backslash, it does not allow a literal newline to be
       quoted for interpretation by the shell.

       The trick is to expand the literal newline into a sequence backslash-n, and then to
       wrap the expanded command string with [bash -c], [echo -e] to recover the literal
       newline. *)
    if not (String.contains s '\n') then s else
      let s =
        String.concat_map s ~f:(function
        | '\n' -> "\\n"
        | '\\' -> "\\\\"
        | c -> String.make 1 c)
      in
      sprintf "bash -c \"$(echo -e %s)\"" (Job_summary.Q.shell_escape s)

  let escape_dollars_for_make s =
    (* "$" is interpreted by [make] as a make-variable-reference, but can be escaped by
       writing "$$" *)
    if not (String.contains s '$') then s else
      String.concat_map s ~f:(function
      | '$' -> "$$"
      | c -> String.make 1 c)

  let string_for_one_line_make_recipe_ignoring_dir action =
    let s = to_sh_ignoring_dir action in
    let s = escape_newlines_for_make s in
    let s = escape_dollars_for_make s in
    s

end

module Trip = struct
  type t = {
    targets: Path.t list;
    deps : Path.t list;
    action : Action.t;
  } [@@deriving sexp_of]
  let to_string t = Sexp.to_string (sexp_of_t t)
end

