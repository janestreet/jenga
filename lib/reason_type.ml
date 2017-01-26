
open Core
open! Int.Replace_polymorphic_compare

type t =
  | Misc                              of string
  | Shutdown
  | Error_in_deps
  | File_read_error                   of Sexp.t
  | Digest_error                      of Sexp.t
  | Undigestable                      of Db.Kind.t
  | Glob_error                        of Db.Glob.t * string
  | No_definition_for_alias           of Alias.t
  | No_source_at_abs_path             of Path.Abs.t
  | No_rule_or_source                 of Path.t
  | Unexpected_directory              of Path.t
  | Command_failed                    of Job_summary.t
  | No_directory_for_target           of string
  | Inconsistent_proxies
  | Running_job_raised                of Sexp.t
  | Multiple_rules_for_path           of Path.Rel.t
  | Rule_failed_to_generate_targets   of Path.Rel.t list
  | Usercode_raised                   of Sexp.t
  | Usercode_error                    of Located_error.t
  | Rules_load_failed                 of [ `Before_loading_rules of Located_error.t
                                         | `Located_error_while_loading of Located_error.t
                                         | `Error_while_loading of Error.t
                                         ]
  | Mtimes_changed                    of Path.t list
  | Sandbox_error                     of [ `at_creation | `at_close ] * Sexp.t
  | Unexpected_targets                of string list
