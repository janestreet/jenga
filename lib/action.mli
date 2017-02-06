open! Core
open! Async

(** [Action.t] - things which a build-rule can do:
    - [shell] run a process (a [Job.t])
    - [save] save a file

    Although some actions can be run directly by the jenga process (i.e. file-save), it is
    always possible to obtain the equivalent [Job.t], which is important when we want to
    externalize the action, for example, when extracting a Makefile. *)
type t [@@deriving sexp_of]

val proxy : default_sandbox:Db.Sandbox_kind.t -> t -> Db.Action_proxy.t

val dir : t -> Path.t

module Output : sig

  (** Policy for treating the command output *)
  type 'a t

  (** no stdout expected; command being run for effect *)
  val ignore : unit t

  (** stdout expected & wanted *)
  val stdout : string t

  (** given an ouput policy of a specific type, cause no output *)
  val none : 'a t -> 'a

end

exception Shutdown

val run : t ->
  message:(unit->unit) ->
  output: 'a Output.t ->
  deps: Db.Proxy_map.t ->
  targets: Path.Rel.t list ->
  putenv: (string * string option) list ->
  progress: Progress.t ->
  need: string ->
  default_sandbox: Db.Sandbox_kind.t ->
  ('a,
   [
   | `command_failed of Job_summary.t
   | `other_error of exn
   | `sandbox_error of Sandbox.error
   ]
  ) Result.t Deferred.t

val run_now : t -> output:'a Output.t -> 'a Deferred.t

(** returns a bash script that expects to be run from [dir t] *)
val to_sh_ignoring_dir : t -> string

val process : dir:Path.t -> prog:string -> args:string list
  -> sandbox:Sandbox.kind option -> ignore_stderr:bool -> t
val save : ?chmod_x:unit -> string -> target:Path.t -> t
