
open Core.Std

include Error_reason_type

let filesystem_related = function
  (* filesystem related errors which might resolve, and so we choose not to count as
     errors when considering [-stop-on-first-error] *)
  | File_read_error _
  | Digest_error _
  | Undigestable _
  | Glob_error _
  | Unexpected_directory _
  | No_directory_for_target _
    -> true

  | Misc _
  | Shutdown
  | Error_in_deps
  | No_definition_for_alias
  | No_source_at_abs_path _
  | No_rule_or_source _
  | Non_zero_status _
  | Inconsistent_proxies
  | Duplicate_scheme_ids _
  | Scheme_raised _
  | Running_job_raised _
  | Multiple_rules_for_paths _
  | Rule_failed_to_generate_targets _
  | Usercode_raised _
    -> false

let to_string_one_line = function
  | Misc s                            -> s
  | Shutdown                          -> "Shutdown"
  | Error_in_deps                     -> "Unable to build dependencies"
  | File_read_error _                 -> "file read error"
  | Digest_error _                    -> "unable to digest file"
  | Glob_error (g,s)                  -> sprintf "%s %s" (Fs.Glob.to_string g) s
  | No_definition_for_alias           -> "No definition found for alias"
  | No_source_at_abs_path a           -> sprintf "No source at absolute path: %s" (Path.Abs.to_string a)
  | No_rule_or_source p               -> sprintf "No rule or source for: %s" (Path.to_string p)
  | Unexpected_directory p            -> sprintf "Unexpected directory: %s" (Path.to_string p)
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
  | Misc _
  | Shutdown
  | Error_in_deps
  | Undigestable _
  | Glob_error _
  | No_definition_for_alias
  | No_rule_or_source _
  | No_source_at_abs_path _
  | Unexpected_directory _
  | Non_zero_status _
  | No_directory_for_target _
  | Duplicate_scheme_ids _
  | Inconsistent_proxies
    -> []

  | Scheme_raised exn
  | Running_job_raised exn
  | Usercode_raised exn
    -> [Exn.to_string exn]

  | File_read_error  error
  | Digest_error error
    -> [Error.to_string_hum error]

  | Multiple_rules_for_paths (_,paths)
  | Rule_failed_to_generate_targets paths
    -> List.map paths ~f:(fun path -> "- " ^ Path.Rel.to_string path)

let messages ~tag t =
  Message.error "%s: %s" tag (to_string_one_line t);
  List.iter (to_extra_lines t) ~f:(fun s -> Message.message "%s" s)

let message_summary config ~need t =
  Message.error "(summary) %s: %s" need (to_string_one_line t);
  List.iter (to_extra_lines t) ~f:(fun s -> Message.message "%s" s);
  if not (Config.brief_error_summary config) then (
    match t with
    | Non_zero_status summary -> Message.repeat_job_summary summary
    | _ -> ()
  )
