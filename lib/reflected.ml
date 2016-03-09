
open Core.Std

module Action = struct

  type t = Job.t [@@deriving sexp_of]

  let dir t = Job.dir t

  let to_sh_ignoring_dir t = Job.to_sh_ignoring_dir t

  let escape_newlines_for_make s =
    if not (String.contains s '\n') then s else
      let s =
        String.concat_map s ~f:(function
        | '\n' -> "\\n"
        | '\\' -> "\\\\"
        | c -> String.make 1 c)
      in
      sprintf "bash -c \"$(echo -e %s)\"" (Job_summary.Q.shell_escape s)

  let escape_dollars_for_make s =
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

