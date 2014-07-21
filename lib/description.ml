
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_

module Generator = struct
  type t = Rule.t list Dep.t
  let create x = x
  let run t = t
end

let ( *>>| ) = Dep.map

module Scheme = struct

  type t = {
    tag : string;
    body : (dir:Path.Rel.t -> Generator.t) ref; (* ref for identity *)
  }
  with fields

  let create ~tag f = {tag; body = ref f;}

  let create ~tag f =
    create ~tag (fun ~dir ->
      Generator.create (
        let generator = f ~dir:(Path.of_relative dir) in
        Generator.run generator *>>| fun rules ->
        match
          List.filter (List.concat_map rules ~f:Rule.targets) ~f:(fun path ->
            not (Path.Rel.(dirname path = dir))
          )
        with
        | _::_ as non_local_targets ->
          failwithf "non-local rule-targets created by scheme: %s(%s) - %s"
            (Path.Rel.to_string dir) tag
            (String.concat ~sep:" " (List.map non_local_targets ~f:Path.Rel.to_string))
            ()
        | [] ->
        rules
      )
    )
end

module For_user = struct

  (* access to config from user rules *)

  let the_installed_config = ref None
  let install_config_for_user_rules config = the_installed_config := Some config

  let config() =
    match !the_installed_config with
    | Some config -> config
    | None -> assert false

end
