
open Core.Std
open Description

include Error_reason_type

let to_string_one_line = function
  | Shutdown                          -> "Shutdown"
  | Error_in_sibling                  -> "Unable to build sibling target"
  | Error_in_deps                     -> "Unable to build dependencies"
  | Digest_error _                    -> "unable to digest file"
  | Glob_error (g,s)                  -> sprintf "%s %s" (Fs.Glob.to_string g) s
  | Jenga_root_problem s              -> sprintf "Problem with %s: %s" (Misc.jenga_root_basename) s
  | No_definition_for_alias           -> "No definition found for alias"
  | No_source_at_abs_path             -> "No source at absolute path"
  | No_rule_or_source                 -> "No rule or source found for target"
  | Unexpected_directory              -> "Unexpected directory found for target"
  | Non_zero_status _                 -> "External command has non-zero exit code"
  | No_directory_for_target s         -> sprintf "No directory for target: %s" s
  | Scheme_raised _                   -> "Generator scheme raised exception"
  | Running_job_raised _              -> "Running external job raised exception"
  | Rule_failed_to_generate_targets _ -> "Rule failed to generate targets"

  | Multiple_rules_for_paths (gen_key,_) ->
    sprintf "Multiple rules generated for some paths (by: %s)" (Gen_key.to_string gen_key)

  | Usercode_raised _ ->
    "User-code raised exception"
  | Undigestable k                    ->
    sprintf "undigestable file kind: %s" (Fs.Kind.to_string k)
  | Duplicate_scheme_ids xs           ->
    sprintf "Duplicate schemes with ids: %s"
      (String.concat ~sep:" " (List.map xs ~f:(sprintf "%S")))
  | Inconsistent_proxies ->
    "Inconsistency proxies"

let to_extra_lines = function
  | Shutdown
  | Error_in_sibling
  | Error_in_deps
  | Undigestable _
  | Glob_error _
  | Jenga_root_problem _
  | No_definition_for_alias
  | No_rule_or_source
  | No_source_at_abs_path
  | Unexpected_directory
  | Non_zero_status _
  | No_directory_for_target _
  | Duplicate_scheme_ids _
  | Inconsistent_proxies
    -> []

  | Scheme_raised exn
  | Usercode_raised exn
  | Running_job_raised exn
    -> [Exn.to_string exn]

  | Digest_error error
    -> [Error.to_string_hum error]

  | Multiple_rules_for_paths (_,paths)
  | Rule_failed_to_generate_targets paths
    -> List.map paths ~f:(fun path -> "- " ^ Path.to_string path)

let messages ~tag t =
  Message.error "%s: %s" tag (to_string_one_line t);
  List.iter (to_extra_lines t) ~f:(fun s -> Message.message "%s" s)

let message_summary config goal t =
  Message.error "(summary) %s: %s" (Goal.to_string goal) (to_string_one_line t);
  if not (Config.brief_error_summary config) then (
    match t with
    | Non_zero_status summary -> Message.repeat_job_summary summary
    | _ -> ()
  )
