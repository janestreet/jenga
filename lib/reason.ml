
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
  | Mtimes_changed _
    -> true

  | Misc _
  | Shutdown
  | Error_in_deps
  | No_definition_for_alias _
  | No_source_at_abs_path _
  | No_rule_or_source _
  | Non_zero_status _
  | Inconsistent_proxies
  | Running_job_raised _
  | Multiple_rules_for_path _
  | Rule_failed_to_generate_targets _
  | Usercode_raised _
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
  | Non_zero_status _                 -> "External command has non-zero exit code"
  | No_directory_for_target s         -> sprintf "No directory for target: %s" s
  | Running_job_raised _              -> "Running external job raised exception"
  | Rule_failed_to_generate_targets _ -> "Rule failed to generate targets"

  | Multiple_rules_for_path rel ->
    sprintf "Multiple rules generated for: %s" (Path.Rel.to_string rel)

  | Usercode_raised _ ->
    "User-code raised exception"
  | Undigestable k                    ->
    sprintf "undigestable file kind: %s" (Db.Kind.to_string k)
  | Inconsistent_proxies ->
    "Inconsistency proxies"
  | Mtimes_changed paths ->
    sprintf "mtimes changed for dependencies whilst action was running: %s"
      (String.concat ~sep:" " (List.map paths ~f:Path.to_string))


let to_extra_lines = function
  | Misc _
  | Shutdown
  | Error_in_deps
  | Undigestable _
  | Glob_error _
  | No_definition_for_alias _
  | No_rule_or_source _
  | No_source_at_abs_path _
  | Unexpected_directory _
  | Non_zero_status _
  | No_directory_for_target _
  | Inconsistent_proxies
  | Multiple_rules_for_path _
  | Mtimes_changed _
    -> []

  | Running_job_raised sexp
  | Usercode_raised sexp
  | File_read_error sexp
  | Digest_error sexp
    -> [Sexp.to_string sexp]

  | Rule_failed_to_generate_targets paths
    -> List.map paths ~f:(fun path -> "- " ^ Path.Rel.to_string path)

let messages ~need t =
  Message.error "%s: %s" need (to_string_one_line t);
  List.iter (to_extra_lines t) ~f:(fun s -> Message.message "%s" s)

let message_summary config ~need t =
  Message.error "(summary) %s: %s" need (to_string_one_line t);
  List.iter (to_extra_lines t) ~f:(fun s -> Message.message "%s" s);
  if not (Config.brief_error_summary config) then (
    match t with
    | Non_zero_status summary -> Message.repeat_job_summary summary
    | _ -> ()
  )
