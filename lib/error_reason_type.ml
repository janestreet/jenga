
open Core.Std
open Description

type t =
| Shutdown
| Error_in_sibling
| Error_in_deps
| Digest_error                      of Error.t
| Undigestable                      of Fs.Kind.t
| Glob_error                        of Fs.Glob.t * string
| Jenga_root_problem                of string
| No_definition_for_alias
| No_source_at_abs_path
| No_rule_or_source
| Unexpected_directory
| Non_zero_status                   of Message.Job_summary.t
| No_directory_for_target           of string
| Inconsistent_proxies
| Duplicate_scheme_ids              of string list
| Scheme_raised                     of exn
| Running_job_raised    of exn
| Multiple_rules_for_paths          of Gen_key.t * Path.t list
| Rule_failed_to_generate_targets   of Path.t list
| Usercode_raised                   of exn
(* | Cycle... *)
