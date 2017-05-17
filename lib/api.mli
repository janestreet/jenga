(** Jenga API - Monadic Style.
    This signature provides the interface between the `user-code' which describes the
    build rules etc for a specific instance of jenga, and the core jenga build system.
    What is ultimately the main entry point of this module is [Env], at the bottom. *)

open! Core
open! Async

module Path : sig

  (** Path.t can be thought of as [`abs of string list | `rel of string list]
      with absolute paths [`abs l] referring to the unix path reachable by
      following path compoments in [l] starting from the root ("/")
      and [`rel l] referring to the path relative to the jenga root.

      Character '/' is disallowed in path components.
      Path components "" and "."  and ".." are disallowed and if used are simplified out.
  *)
  type t [@@deriving sexp]
  include Comparable.S with type t := t
  include Hashable.S with type t := t

  (** an absolute path made from a /-separated path string.
      the string must start with a '/' *)
  val absolute : string -> t

  (** a relative path made from a /-separated path string.
      the string must NOT start with a '/' *)
  val relative : dir:t -> string -> t

  (** either absolute or relative taken w.r.t. [dir] - determined by leading char *)
  val relative_or_absolute : dir:t -> string -> t

  (** if relative, displayed as repo-root-relative string.
      [relative_or_absolute ~dir:the_root (to_string t) = t] *)
  val to_string : t -> string

  (** refers to the jenga repo root *)
  val the_root : t

  (** refers to the root of the unix filesystem *)
  val unix_root : t

  (** path with the last path component dropped.
      for the roots ([the_root] or [unix_root])
      [dirname x = x] *)
  val dirname : t -> t

  (** last component of the path.
      for the roots we have [basename x = "."] *)
  val basename : t -> string

  (** shortcut for [relative ~dir:the_root] *)
  val root_relative : string -> t

  (** [is_descendant ~dir t = true] iff there exists a ".."-free [x] such that
      [relative ~dir x = t] *)
  val is_descendant : dir:t -> t -> bool

  (** [x = reach_from ~dir t] is such that [relative_or_absolute ~dir x = t],
     x starts with a "." or a '/', and x is otherwise as short as possible *)
  val reach_from : dir:t -> t -> string

  (** returns absolute path string, even if the path is relative.
      depends on jenga repo location. *)
  val to_absolute_string : t -> string

end

module Located_error : sig
  module Loc : sig
    module Source : sig
      type t =
        | File of Path.t
        | Other of string (** When the location is not a file on disk, such as [Var.t] *)
    end

    type t =
      { source    : Source.t
      ; line      : int
      ; start_col : int
      ; end_col   : int
      }
  end

  (** Raise an error. Jenga will transfer the location and error message to the build
      manager so that it is properly displayed in emacs/vim/... *)
  val raise : loc:Loc.t -> string -> _
  val raisef : loc:Loc.t -> ('a, unit, string, unit -> _) format4 -> 'a
end

module Kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  [@@deriving sexp_of]
end

module Glob : sig
  type t [@@deriving sexp]
  (** [create ~dir pattern] refers to anything in [dir] whose basename matches
      the [pattern]. Note that patterns with slashes or path globs (**) in them
      do not work.
      Special syntax allowed in [pattern]:
      * - stands for any string unless it's the leading character, in which case it
      does not match empty string or hidden files (prefixed with a dot).
      ? - stands for any character
      [a-z] - a character in range ['a' .. 'z']
      [!a-z] - a character out of ['a' .. 'z']
      \ - escapes the character following it
      {alt1,alt2} - matches both alt1 and alt2. can be nested.
  *)
  val create : dir:Path.t -> ?kinds: Kind.t list -> string -> t
  (** matches exactly the filename given *)
  val create_from_path : kinds:Fs.Kind.t list option -> Path.t -> t
end

(** An [Alias] is the name of a symbolic target.
    [Alias.create ~dir:"a" "foo"] is what you get on the command line by writing [jenga
    a/.foo].
    When no arguments are given at the command line, jenga interprets it as building
    the alias "DEFAULT" in the current directory. *)
module Alias : sig
  type t [@@deriving sexp]
  val create : dir: Path.t -> string -> t
end

(** Sandboxing in the act of running a compilation into a separate directory, for the
    purpose of enforcing that dependencies and targets be properly specified (instead
    of trusting the user to get them right). *)
module Sandbox : sig

  type t [@@deriving sexp]

  (** Do not sandbox an action. *)
  val none : t

  (** Sandbox an action by hard-linking dependencies. *)
  val hardlink : t

  (** Sandbox an action by copying dependencies. *)
  val copy : t

  (** Sandbox an action by hard-linking dependencies. Ignore misspecified targets. *)
  val hardlink_ignore_targets : t

  (** Sandbox an action by copying dependencies. Ignore misspecified targets. *)
  val copy_ignore_targets : t

  (** Equivalent to [none] unless the --sandbox-actions command-line flag is active, in
      which case it is equivalent to [hardlink]. *)
  val default : t

end

module Action : sig
  type t [@@deriving sexp_of]
  (** [process ?ignore_stderr ~dir ~prog ~args] constructs an action, that when run
      causes a new process to be created to run [prog], passing [args], in [dir].
      The process fails if its exit code is not zero, or if print things on stderr
      (unless ~ignore_stderr:true is passed). The process is not sandboxed by default. *)
  val process
    : ?ignore_stderr:bool
    -> ?sandbox:Sandbox.t
    -> dir:Path.t
    -> prog:string
    -> args:string list
    -> unit
    -> t
  val save : ?chmod_x:unit -> string -> target:Path.t -> t
end

module Shell : sig

  (** [Shell.escape arg] quotes a string (if necessary) to avoid interpretation of
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

module Var : sig

  (** [Var.t] is a registered environment variable. It's value value may be queried and
      modified via the jenga RPC or command line. *)
  type 'a t

  (** [register name ?choices] registers [name].  If [choices] is provided, this is
      attached as meta information, available when querying.

      [register_with_default ?default name ?choices] is like [register] except [default]
      is used when the variable is unset, and is also attached as meta information.

      Except for the registration of [default] as meta-info, [register_with_default]
      behaves as: [register ?choices name |> map ~f:(Option.value ~default)]

      An exception is raised if the same name is registered more than once in a reload of
      the jengaroot.
  *)
  val register              : ?choices:string list -> string                   -> string option t
  val register_with_default : ?choices:string list -> string -> default:string -> string t

  (** [peek t] causes modification to [t] (via RPC or command-line) to trigger a reload of
      the jengaroot. To avoid this use [Dep.getenv] instead. *)
   val peek : ?dont_trigger:unit -> 'a t -> 'a

  val map  : 'a t -> f:('a -> 'b) -> 'b t

end

(** The jenga monad. It has two pieces to it:
    - an implicit accumulator of dependencies, that [Dep.path] and all the functions that
      return a [unit t] add to. The final value of this accumulator is the dependencies of
      the action in [Rule.create] and [Dep.action], and what gets built in the case of
      [Rule.alias]
    - a monad and other usual combinators, which allows to write computation that ask
      jenga to build something and wait for the result (and to rebuild if that input
      changes). Because of this, functions should not be side-effecting.

   To make clear the distinction between the two, take:

   val path : Path.t -> unit t
   val contents : Path.t -> string t

   [path] simply says that the surrounding [Rule.create] (for instance)
   will read the contents of that file.
   [contents] allows to create different action depending on the contents
   of the file.

   It's highly recommended to use parallel combinators like [both] and [all]
   in preference to [bind]. Using [bind] is inherently sequential, whereas
   usually work in a build system can be done in parallel.
*)
module Dep : sig

  type 'a t [@@deriving sexp_of]
  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t
  val deferred : (unit -> 'a Deferred.t) -> 'a t

  (** [let v = memoize ~name t] behaves like [t], except with caching: using [v] in several
      computations will not result in duplicate evaluations of [t].
      [name] is used in displayed in errors, or in the case of dependency cycles. *)
  val memoize : name:string -> 'a t -> 'a t

  val action : Action.t t -> unit t
  val action_stdout : Action.t t -> string t
  val alias : Alias.t -> unit t
  val path : Path.t -> unit t

  (** [getenv v] provides access to a registered environment variable, responding to
      changes notified to jenga via the [setenv] RPC/command-line. *)
  val getenv : 'a Var.t -> 'a t

  (** [group_dependencies t] is equivalent to [t], however jenga will be careful to avoid
      duplicating the set of dependencies that have been declared. This is best used under
      an alias, as the alias will allow to share the computation as well. *)
  val group_dependencies : 'a t -> 'a t

  (** [source_if_it_exists] Dont treat path as a goal (i.e. don't force it to be built)
      Just depend on its contents, if it exists. It's ok if it doesn't exist. *)
  val source_if_it_exists : Path.t -> unit t

  val contents : Path.t -> string t
  val contents_cutoff : Path.t -> string t

  (** The semantics of [glob_listing] and [glob_change] includes files which exist on the
     file-system AND files which are buildable by some jenga rule. Therefore it is an
     error (dependency cycle) to glob a directory while generating the scheme for that
     same directory. Use [Scheme.glob] instead. *)
  val glob_listing : Glob.t -> Path.t list t
  val glob_change : Glob.t -> unit t

  (** Versions with old semantics: only includes files on the file-system. *)
  val fs_glob_listing : Glob.t -> Path.t list t
  val fs_glob_change : Glob.t -> unit t

  val subdirs : dir:Path.t -> Path.t list t
  (** [file_exists] makes the information about file existence available to the rules, but
      does not declare it as an action dependency.
  *)
  val file_exists : Path.t -> bool t
  (** [file_existence] declares an action dependency on file existence *)
  val file_existence : Path.t -> unit t

  module List : sig
    val concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t
    val concat : 'a list t list -> 'a list t
  end

  val buildable_targets : dir:Path.t -> Path.t list t

  (** [source_files ~dir] is files_on_filesystem ~dir \ buildable_targets ~dir *)
  val source_files : dir:Path.t -> Path.t list t

end

(** [Reflected] and [Reflect] are the two parts of the reflection api of jenga, ie the api
    that allows the jenga rules to ask about the structure of the dependency graph.
    There have been two uses of this api so far:
    - generating the input to an other build system, for bootstrapping purpose.
      Given a target, you can ask what action builds it and what dependencies it has,
      and write a makefile rule that does that.
    - checking the sanity of dependencies. If you have systems that can only use
      vetted libraries, this can be used to get all the dependencies of such a system,
      and check that they are only vetted libraries. *)

module Reflected : sig
  module Action : sig
    type t [@@deriving sexp_of]
    val dir : t -> Path.t
    val to_sh_ignoring_dir : t -> string
    val string_for_one_line_make_recipe_ignoring_dir : t -> string
  end
  (** simple make-style rule triple, named [Trip.t] to distinguish from
      Jenga's more powerful rules [Rule.t] below. *)
  module Trip : sig
    type t = {
      targets: Path.t list;
      deps : Path.t list;
      action : Action.t;
    }
    [@@deriving sexp_of]
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

  val putenv : (string * string option) list Dep.t

end

(** A rule specifies how to build a [Goal.t]. There is no nothing of scoping or
    permissions, so all rules can be referred to at the command line, and by any [Dep.t].
*)
module Rule : sig
  type t [@@deriving sexp_of]

  (** [create ~targets action_dep] specifies that the given static list of targets
      can be built by the action in the dep, and that action depends on the implicit
      dependencies (see [Dep]).
      A given file can only be the target of a single rule. A rule can only declare
      targets in the directories of the scheme that declares it (required so schemes
      can be evaluated lazily).
      The action must not depend on the filesystem other than how the given dependencies,
      and must create all the specified targets. These properties are assumed, or checked
      when using [Sandbox].
      The dep itself can do arbitrary computation like reading generated files, but these
      do not become dependencies of running the action, only dependencies of figuring out
      what the action is.
      If you need a dynamic set of targets, you have two choices:
      - use [Scheme.dep] before building this rule (which is not a panacea, as it means
        building any alias in that directory will run that dep, so that's only reasonable
        for cheap computations)
      - create a tar ball instead of many targets
      If some of these targets are symlink, then the rule must depend on the target
      of the symlink. *)
  val create : targets:Path.t list -> Action.t Dep.t -> t

  (** [alias a] means that requesting the build of the alias [a] must executing all the
      corresponding. This is not like phony targets in make, as these computations are
      cached and only run as needed.
      Same as for rules, the directory of the alias must be the directory on the scheme
      that contains the alias.
      Unlike rules, multiple definitions can be given for an alias. Building the alias
      then builds all the definitions. *)
  val alias : Alias.t -> unit Dep.t list -> t
  val default : dir:Path.t -> unit Dep.t list -> t
  val simple : targets:Path.t list -> deps:unit Dep.t list -> action:Action.t -> t
end

(** A scheme describes, for a given directory, what rules are available in that
    directory. A scheme can include computation (in the form of [Scheme.dep]) but
    there are limits to what can be done, because jenga will reject anything that
    looks like a dependency cycle. *)
module Scheme : sig
  type t [@@deriving sexp_of]
  val empty : t

  val rules : ?sources:Path.t list -> Rule.t list -> t
  val sources : Path.t list -> t

  val dep : t Dep.t -> t
  (** Evaluates the schemes sequentially, which means that latter schemes can
      depend (using [glob]) on the targets of the former schemes. *)
  val all : t list -> t
  val rules_dep : Rule.t list Dep.t -> t
  val contents : Path.t -> (string -> t)-> t

  (** Used to glob the directory of the current scheme, without causing dependency
      cycles. It is an error if any rule created after this glob is matched by the
      glob. *)
  val glob : Glob.t -> (Path.t list -> t) -> t
end

module Env : sig

  (** Specifies for a given directory [dir]
      - its rule-scheme, ie a specification of what rules are available in that directory
      - whether jenga can create the directory if it doesn't already exist.
      If [directories_generated_from = None], jenga will assume [dir] exists.
      If [directories_generated_from = Some path], then [path] must be a prefix of [dir],
      and jenga will create all the ancestors of [dir] and descendants of [path]
      (both inclusive) as needed.
      These values are assumed to be consistent, ie if the specification for [dir] states
      [directories_generated_from = Some path], then the specification for any directory
      below [path] (inclusive) must state the same. *)
  module Per_directory_information : sig
    type t =
      { scheme : Scheme.t
      ; directories_generated_from : Path.t option
      }
  end

  type t = Env.t

  val create :
    (** Env variable for the execution of actions, not for jenga itself. *)
    ?putenv:(string * string option) list ->
    (** Same as the previous option, for the PATH. *)
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?build_begin : (unit -> unit Deferred.t) ->
    ?build_end : (unit -> unit Deferred.t) ->

    (** Optional specification of which paths are to be regarded as artifacts, and hence
        become candidates for deletion as stale-artifacts, if there is no build rule for
        them.

        [delete_if_depended_upon] is what is expected to be used, whereas [delete_eagerly]
        is more of a workaround for certain specific scenarios.

        If [delete_if_depended_upon] is not provided, nothing is deleted (which is not
        already deleted by the other rule). The predicate will be called when specifying a
        dependency on a file that is not a target (using Dep.path or Dep.glob_listing).
        It is useful to guarantee that the build doesn't depend on ignored files that
        can't be built (otherwise, how could you build a fresh clone, where that ignored
        file isn't there?).

        If [delete_eagerly] is not provided, jenga will delete artifacts that it knows it
        built previously, as recorded in .jenga/db.
        The downside of [delete_eagerly] is that it is (in general) too aggressive, as it
        can delete files that are ignored but not related to the build system (like .orig
        files from the version control system). Is is mostly meant to delete certain files
        that you know a command can read even though you haven't put dependencies on them
        (like ocamlopt reading cmis for which no ml exist, if you create dependencies
        based on which ml files exist).

        The [Dep.t] used in these functions should only be the ones that depend on what's
        on the filesystem right now, rather than the ones that also anticipate what can be
        built. So [fs_glob_listing] should be used instead of [glob_listing] for instance.
    *)
    ?delete_eagerly:((non_target:Path.t -> bool) Dep.t) ->
    ?delete_if_depended_upon:((non_target:Path.t -> bool) Dep.t) ->

    (dir:Path.t -> Per_directory_information.t) ->
    t

end

(** The jenga API intentionally shadows printf, so that if opened in jenga/root.ml the
    default behaviour is for [printf] to print via jenga's message system, and not
    directly to stdout. Sending to stdout makes no sense for a dameonized jenga server.

    [printf] sends messages via jenga's own message system, (i.e. not directly to stdout).
    Messages are logged, transmitted to jenga [trace] clients, and displayed to stdout if
    the jenga server is not running as a daemon.

    [printf_verbose] is like [printf], except the message are tagged as `verbose', so
    allowing non-verbose clients to mask the message

    There is no need to append a \n to the string passed to [printf] or [printf_verbose].
*)

val printf : ('a, unit, string, unit) format4 -> 'a
val printf_verbose : ('a, unit, string, unit) format4 -> 'a

val run_action_now : Action.t -> unit Deferred.t
val run_action_now_stdout : Action.t -> string Deferred.t
