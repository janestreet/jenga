
open Core.Std

type t =
| Misc                              of string
| Shutdown
| Error_in_deps
| File_read_error                   of Error.t
| Digest_error                      of Error.t
| Undigestable                      of Fs.Kind.t
| Glob_error                        of Fs.Glob.t * string
| No_definition_for_alias           of Alias.t
| No_source_at_abs_path             of Path.Abs.t
| No_rule_or_source                 of Path.t
| Unexpected_directory              of Path.t
| Non_zero_status                   of Message.Job_summary.t
| No_directory_for_target           of string
| Inconsistent_proxies
| Running_job_raised    of exn
| Multiple_rules_for_path           of Path.Rel.t
| Rule_failed_to_generate_targets   of Path.Rel.t list
| Usercode_raised                   of exn
| Mtimes_changed                    of Path.t list
