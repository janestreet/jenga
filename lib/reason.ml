open Core
open! Int.Replace_polymorphic_compare

include Reason_type

let filesystem_related = function
  (* filesystem related errors which might resolve, and so we choose not to count as
     errors when considering [-stop-on-first-error] *)
  | File_read_error _
  | Digest_error _
  | Undigestable _
  | Glob_error _
  | Unexpected_directory _
  | No_directory_for_target _
  | Mtimes_changed _
    -> true

  | Misc _
  | Shutdown
  | Error_in_deps
  | No_definition_for_alias _
  | No_source_at_abs_path _
  | No_rule_or_source _
  | Command_failed _
  | Inconsistent_proxies
  | Running_job_raised _
  | Multiple_rules_for_path _
  | Rule_failed_to_generate_targets _
  | Usercode_raised _
  | Usercode_error _
  | Rules_load_failed _
  | Sandbox_error _
  | Unexpected_targets _
    -> false

let to_string_one_line = function
  | Misc s                            -> s
  | Shutdown                          -> "Shutdown"
  | Error_in_deps                     -> "Unable to build dependencies"
  | File_read_error _                 -> "file read error"
  | Digest_error _                    -> "unable to digest file"
  | Glob_error (g,s)                  -> sprintf "%s %s" (Db.Glob.to_string g) s
  | No_definition_for_alias a         -> (sprintf "No definition found for alias: %s"
                                            (Alias.to_string a))
  | No_source_at_abs_path a           ->
    sprintf "No source at absolute path: %s" (Path.Abs.to_string a)
  | No_rule_or_source p               -> sprintf "No rule or source for: %s" (Path.to_string p)
  | Unexpected_directory p            -> sprintf "Unexpected directory: %s" (Path.to_string p)
  | Command_failed _                  -> "External command failed"
  | No_directory_for_target s         -> sprintf "No directory for target: %s" s
  | Running_job_raised _              -> "Running external job raised exception"
  | Sandbox_error(`at_creation, _)    -> "Error creating sandbox"
  | Sandbox_error(`at_close, _)       -> "Error closing sandbox"
  | Unexpected_targets l ->
    sprintf "Unexpected targets in sandbox: %s" (String.concat l ~sep:", ")
  | Rule_failed_to_generate_targets _ -> "Rule failed to generate targets"

  | Multiple_rules_for_path rel ->
    sprintf "Multiple rules generated for: %s" (Path.Rel.to_string rel)

  | Usercode_raised _ ->
    "User-code raised exception"
  | Usercode_error _ ->
    "User-code produced error"
  | Rules_load_failed (`Before_loading_rules _) ->
    "Error before loading rules"
  | Rules_load_failed (`Located_error_while_loading _ | `Error_while_loading _)
    -> "Error while loading rules"
  | Undigestable k                    ->
    sprintf "undigestable file kind: %s" (Db.Kind.to_string k)
  | Inconsistent_proxies ->
    "Inconsistency proxies"
  | Mtimes_changed paths ->
    sprintf "mtimes changed for dependencies whilst action was running: %s"
      (String.concat ~sep:" " (List.map paths ~f:Path.to_string))


let to_extra_lines t ~dir =
  match t with
  | Misc _
  | Shutdown
  | Error_in_deps
  | Undigestable _
  | Glob_error _
  | No_definition_for_alias _
  | No_rule_or_source _
  | No_source_at_abs_path _
  | Unexpected_directory _
  | Command_failed _
  | No_directory_for_target _
  | Inconsistent_proxies
  | Multiple_rules_for_path _
  | Mtimes_changed _
  | Unexpected_targets _
    -> []

  | Rules_load_failed (`Error_while_loading error) ->
    let lines = String.split_lines (Error.to_string_hum error) in
     (* If the error looks like a normal compilation error, remove the noise from
        ocaml_plugin *)
    (match
       List.drop_while lines ~f:(fun line ->
         not (String.is_prefix ~prefix:"File \"" line))
     with
     | [] -> lines
     | _ :: _ as rest -> rest)

  | Running_job_raised sexp
  | Usercode_raised sexp
  | File_read_error sexp
  | Digest_error sexp
  | Sandbox_error (_, sexp)
    -> [Sexp.to_string sexp]

  | Usercode_error located_error
  | Rules_load_failed ( `Before_loading_rules located_error
                      | `Located_error_while_loading located_error)
    -> Located_error.to_lines ~dir located_error

  | Rule_failed_to_generate_targets paths
    -> List.map paths ~f:(fun path -> "- " ^ Path.Rel.reach_from ~dir path)

let messages ~need t =
  Message.error !"%{Goal}: %s" need (to_string_one_line t);
  let dir = Goal.directory need in
  List.iter (to_extra_lines t ~dir) ~f:(fun s -> Message.message "%s" s)

let message_summary config ~need t =
  Message.error !"(summary) %{Goal}: %s" need (to_string_one_line t);
  let dir = Goal.directory need in
  List.iter (to_extra_lines t ~dir) ~f:(fun s -> Message.message "%s" s);
  if not (Config.brief_error_summary config) then (
    match t with
    | Command_failed summary -> Message.repeat_job_summary summary
    | _ -> ()
  )
