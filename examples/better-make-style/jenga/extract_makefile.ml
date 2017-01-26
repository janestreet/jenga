open Core
open Jenga_lib.Api

let ( *>>= ) t f = Dep.bind t ~f
let ( *>>| ) = Dep.map

module F(X : sig
  val dir : Path.t
end) = struct
  open X

  let keep _path = true

  let wrap_relative_cd_from path s =
    if dir = path then s else
      let relative_cd = Path.reach_from ~dir path in
      sprintf "(cd %s; %s)" relative_cd s

  let format_path = Path.reach_from ~dir
  let format_paths xs = String.concat ~sep:" " (List.map xs ~f:format_path)

  let format_trip {Reflected.Trip.deps;targets;action} =
    let deps = List.filter deps ~f:keep in
    let targets = List.filter targets ~f:keep in
    let action_string =
      wrap_relative_cd_from (Reflected.Action.dir action)
        (Reflected.Action.string_for_one_line_make_recipe_ignoring_dir action)
    in
    match targets with
    | [] -> sprintf "#rule with no targets!"
    | t1::rest ->
      let touch_string =
        match rest with | [] -> "" | _ ->
          sprintf "\n\ttouch %s"
            (String.concat ~sep:" " (List.map rest ~f:format_path))
      in
      String.concat (List.map rest ~f:(fun target ->
        sprintf "%s : %s\n" (format_path target) (format_path t1)
      )) ^ sprintf "%s : %s\n\t%s%s"
        (format_path t1) (format_paths deps) action_string touch_string

  let format_makefile ~roots trips =
    sprintf "\nall : %s\n\n%s\n"
      (format_paths roots)
      (String.concat ~sep:"\n\n" (List.map trips ~f:format_trip))

  let extract ~makefile ~from:alias =
    Rule.create ~targets:[makefile] (
      Reflect.alias alias *>>= fun roots ->
      Reflect.reachable ~keep roots *>>| fun trips ->
      Action.save (format_makefile ~roots trips) ~target:makefile
    );

end

let extract ~makefile ~from =
  let dir = Path.dirname makefile in
  let module M = F (struct let dir = dir end) in
  M.extract ~makefile ~from
