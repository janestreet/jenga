
open Core.Std

type t =
| Misc                              of string
| Shutdown
| Error_in_deps
| File_read_error                   of Error.t
| Digest_error                      of Error.t
| Undigestable                      of Fs.Kind.t
| Glob_error                        of Fs.Glob.t * string
| No_definition_for_alias
| No_source_at_abs_path             of Path.Abs.t
| No_rule_or_source                 of Path.t
| Unexpected_directory              of Path.t
| Non_zero_status                   of Message.Job_summary.t
| No_directory_for_target           of string
| Inconsistent_proxies
| Duplicate_scheme_ids              of string list
| Scheme_raised                     of exn
| Running_job_raised    of exn
| Multiple_rules_for_paths          of Gen_key.t * Path.Rel.t list
| Rule_failed_to_generate_targets   of Path.Rel.t list
| Usercode_raised                   of exn
