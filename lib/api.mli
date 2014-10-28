
(* Jenga API - Monadic Style.
   This signature provides the interface between the `user-code' which
   describes the build rules etc for a specific instatnce of jenga,
   and the core jenga build system. *)

open Core.Std
open Async.Std

module Path : sig
  type t with sexp
  val absolute : string -> t (* string must start with / *)
  val relative : dir:t -> string -> t (* string must NOT start with / *)

  (* either absolute or relative taken w.r.t. [dir] - determined by leading char *)
  val relative_or_absolute : dir:t -> string -> t

  val to_string : t -> string (* if relative, displayed as repo-root-relative string *)
  val to_absolute_string : t -> string
  val dirname : t -> t
  val basename : t -> string
  val the_root : t
  val root_relative : string -> t

  val is_descendant : dir:t -> t -> bool
  val reach_from : dir:t -> t -> string (* like [dotdot] but better! *)

  (* [dotdot ~dir path]
     compute relative ".."-based-path-string to reach [path] from [dir] *)
  val dotdot : dir:t -> t -> string

end

module Kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
end

module Glob : sig
  type t with sexp
  val create : dir:Path.t -> ?kinds: Kind.t list -> string -> t
end

module Alias : sig
  type t with sexp
  val create : dir: Path.t -> string -> t
end

module Action : sig
  type t
  (* [process ~dir ~prog ~args] - constructs an action, that when run causes a new process
     to be created to run [prog], passing [args], in [dir].
     No shell (sh, bash.. ) is invoked! *)
  val process : dir:Path.t -> prog:string -> args:string list -> t
  val save : ?chmod_x:unit -> string -> target:Path.t -> t
end

module Shell : sig

  (* [Shell.escape arg] quotes a string (if necessary) to avoid interpretation of
     characters which are special to the shell.

     [Shell.escape] is used internally by jenga when displaying the "+" output lines,
     which show what commands are run; seen by the user when running [jenga -verbose] or
     if the command results in a non-zero exit code. It is exposed in the API since it is
     useful when constructing actions such as: [Action.progress ~dir ~prog:"bash" ~args],
     which should ensure [args] are suitably quoted.

     Examples:
     (1) escape "hello" -> "hello"
     (2) escape "foo bar" -> "'foo bar'"
     (3) escape "foo'bar" -> "'foo'\\''bar'"

     Note the [arg] and result strings in the above examples are expressed using ocaml
     string literal syntax; in particular, there is only single backslash in the result of
     example 3.

     Example (1): No quoting is necessary.  Example (2): simple single quoting is used,
     since [arg] contains a space, which is special to the shell.  Example (3): the result
     is single quoted; the embedded single quote is handled by: un-quoting, quoting using
     a bashslash, then re-quoting.
  *)
  val escape : string -> string

end

module Dep : sig (* The jenga monad *)

  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t
  val deferred : (unit -> 'a Deferred.t) -> 'a t

  val action : Action.t t -> unit t
  val action_stdout : Action.t t -> string t
  val alias : Alias.t -> unit t
  val path : Path.t -> unit t

  (* [source_if_it_exists] Dont treat path as a goal (i.e. don't force it to be built)
     Just depend on its contents, if it exists. It's ok if it doesn't exist. *)
  val source_if_it_exists : Path.t -> unit t

  val contents : Path.t -> string t
  val contents_cutoff : Path.t -> string t

  (* The semantics of [glob_listing] and [glob_change] includes files which exist on the
     file-system AND files which are buildable by some jenga rule *)
  val glob_listing : Glob.t -> Path.t list t
  val glob_change : Glob.t -> unit t

  (* Versions with old semantics: only includes files on the file-system. *)
  val fs_glob_listing : Glob.t -> Path.t list t
  val fs_glob_change : Glob.t -> unit t

  val subdirs : dir:Path.t -> Path.t list t
  val file_exists : Path.t -> bool t

  module List : sig
    val concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t
  end

  val buildable_targets : dir:Path.t -> Path.t list t

  (* [source_files ~dir]
     files_on_filesystem ~dir \ buildable_targets ~dir
  *)
  val source_files : dir:Path.t -> Path.t list t

end

module Reflected : sig
  module Action : sig
    type t
    val dir : t -> Path.t
    val string_for_sh : t -> string
    val string_for_one_line_make_recipe : t -> string
  end
  (* simple make-style rule triple, named [Trip.t] to distinguish from
     Jenga's more powerful rules [Rule.t] below. *)
  module Trip : sig
    type t = {
      targets: Path.t list;
      deps : Path.t list;
      action : Action.t;
    }
  end
end

module Reflect : sig

  val alias : Alias.t -> Path.t list Dep.t
  val path : Path.t -> Reflected.Trip.t option Dep.t

  val reachable :
    keep:(Path.t -> bool) ->
    ?stop:(Path.t -> bool) -> (* defaults to: !keep *)
    Path.t list ->
    Reflected.Trip.t list Dep.t

  val putenv : (string * string) list Dep.t

end

module Rule : sig
  type t
  val create : targets:Path.t list -> Action.t Dep.t -> t
  val alias : Alias.t -> unit Dep.t list -> t
  val default : dir:Path.t -> unit Dep.t list -> t
  val simple : targets:Path.t list -> deps:unit Dep.t list -> action:Action.t -> t
end

module Scheme : sig
  type t
  val rules : Rule.t list -> t
  val dep : t Dep.t -> t
  val all : t list -> t
  val exclude : (Path.t -> bool) -> t -> t
  val rules_dep : Rule.t list Dep.t -> t
  val contents : Path.t -> (string -> t)-> t
  val no_rules : t
  val switch_glob : ?def:t -> (string * t) list -> t
end

module Env : sig
  type t = Env.t
  val create :
    ?run_when_persist_format_has_changed:Action.t ->
    ?putenv:(string * string) list ->
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?build_begin : (unit -> unit Deferred.t) ->
    ?build_end : (unit -> unit Deferred.t) ->

    (* [create ~artifacts ...]

       Optional [artifacts] allows specification, on a per-directory basis, which paths
       are to be regarded as artifacts, and hence become candidates for deletion as
       stale-artifacts, if there is no build rule.

       If [artifacts] is not provided, jenga will determine artifacts using it's own
       knowledge of what was previously built, as recorded in .jenga.db. *)
    ?artifacts: (dir:Path.t -> Path.t list Dep.t) ->

    (* [create f] - Mandatory argument [f] specifies, per-directory, the rule-scheme. *)
    (dir:Path.t -> Scheme.t) ->
    t

end

val verbose : unit -> bool

val run_action_now : Action.t -> unit Deferred.t
val run_action_now_stdout : Action.t -> string Deferred.t
