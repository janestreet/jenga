(** [Db] contains types which will be stored persistently, so jenga doesn't need to rerun
    all the actions on restart.
    This is different from [make] which does not need persistent state, because instead
    of storing the knowledge that dependencies+action produced targets, it uses timestamps
    dependencies > timestamps of targets to claim that targets need to be recreated, and
    it doesn't even make record the dependency on the action.
    Most types do not expose bin_ios because there is some sharing in the serialized
    format across values (see With_index), and so they cannot be serialized
    independently. *)

open! Core

module Kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  [@@deriving sexp, hash, compare]
  val to_string : t -> string
end

module Mtime : sig
  type t
  [@@deriving sexp_of, hash, compare]
  val of_float : float -> t
  val equal : t -> t -> bool
end

module Stats : sig  (** reduced stat info *)
  type t
  [@@deriving sexp, hash, compare]
  val of_unix_stats : Core.Unix.stats -> t
  val equal : t -> t -> bool
  val kind : t -> Kind.t
  val mtime : t -> Mtime.t
  val dev : t -> int
  val ino : t -> int
end

module Digest : sig  (** proxy for the contents of a file in the file-system *)

  type t
  [@@deriving sexp_of, hash, compare]
  val intern : Md5.t -> t

  module With_store : sig
    type 'a t [@@deriving bin_io]
    val snapshot : 'a -> 'a t
    val value : 'a t -> 'a
  end

end

module Listing : sig (** result of globbing *)
  type t [@@deriving sexp, compare]
  module Elem : sig
    type t
    val create : base:string -> kind:Kind.t -> t
  end
  val create : dir:Path.t -> elems:Elem.t list -> t
  val of_file_paths_exn : dir:Path.t -> Path.t list -> t
  val equal : t -> t -> bool
  val paths : t -> Path.Set.t
  module Restriction : sig
    type t [@@deriving sexp_of]
    val create : kinds:Kind.t list option -> Pattern.t -> t
    val to_string : t -> string
    val pattern : t -> Pattern.t
    val kind_allows_file : t -> bool
    val compare : t -> t -> int
  end
  val restrict : t -> Restriction.t -> t
end

module Glob : sig
  type t [@@deriving sexp, hash, compare]
  include Hashable with type t := t
  val create : dir:Path.t -> restriction:Listing.Restriction.t -> t
  val dir : t -> Path.t
  val restriction : t -> Listing.Restriction.t
  val to_string : t -> string
end

module Pm_key : sig  (** Pm_key - path or glob *)
  type t [@@deriving sexp_of, hash, compare]
  include Comparable.S_plain with type t := t
  val equal : t -> t -> bool
  val of_abs_path : Path.Abs.t -> t
  val of_rel_path : Path.Rel.t -> t
  val of_path : Path.t -> t
  val of_glob : Glob.t -> t
  val to_string : t -> string
  val to_path_exn : t -> Path.Rel.t (* for targets_proxy_map *)
  val to_path_opt : t -> Path.t option (* for mtimes check *)
end

module Proxy : sig
  type t [@@deriving sexp_of, hash, compare]
  val of_digest : Digest.t -> t
  val of_listing : dir:Path.t -> Path.Set.t -> t
  val equal : t -> t -> bool
end

module Proxy_map : sig
  type t [@@deriving hash, compare]
  val empty  : t
  val single : Pm_key.t -> Proxy.t -> t
  val group : t -> t

  type inconsistency = (Pm_key.t * Proxy.t list) list
  [@@deriving sexp_of]

  val create_by_path : (Path.t * Proxy.t) list -> (t, inconsistency) Result.t

  val equal : t -> t -> bool

  (** Return [None] if [before] and [after] are equal and [Some l] where [l] is a
      witness of the differences otherwise *)
  val diff : before:t -> after:t -> Pm_key.t list option

  (** [Error] means the proxy map is inconsistent. However, some inconsistent proxy maps
      are [Ok]. *)
  val merge : t list -> (t, (Pm_key.t * Proxy.t list) list) Result.t

  (** The size of the dependencies, excluding the shared parts. Used to build profiling
      information. *)
  val shallow_length : t -> int

  (**
     [filesystem_assumptions t] returns the set of things accessed via a relative path on
     the filesystem that have been used to construct [t].
     `Dirs is the list of directories that must exist to reproduce [t].
     `Files is the list of files that need to be read.
     `Arbitrary_files is the list of files that must exist to reproduce directory
     listings, but that don't need to be read.
  *)
  val filesystem_assumptions
    :  t
    -> [> `Dirs of Path.Rel.Hash_set.t ]
       * [> `Files of Path.Rel.Hash_set.t ]
       * [> `Arbitrary_files of Path.Rel.Hash_set.t ]

  module Group : sig
    type t
    (** [find_or_add t ~unique_id_across_jenga:typ ~unique_f_across_jenga:f] computes
        [f (paths t)], making sure [f] is only called once per [t].
        Values for [typ] and [f] must be the same for every call of
        [find_or_add] for a given [t].
        In particular in the current implementation, passing different ids will raise
        but passing different functions will probably give you incorrect results.

        Note that [paths t] is shallow, that is it ignores subgroups of [t].
    *)
    val find_or_add :
      t
      -> unique_id_across_jenga:'a Type_equal.Id.t
      -> unique_f_across_jenga:(Path.t list -> 'a)
      -> 'a
  end
  val to_paths_for_mtimes_check : t -> Path.t list * Group.t list
end

module Sandbox_kind : sig
 type t =
   | No_sandbox
   | Hardlink
   | Copy
   | Hardlink_ignore_targets
   | Copy_ignore_targets
 [@@deriving sexp_of, compare]
end

module Save_proxy : sig
  type t
  [@@deriving sexp_of, compare]
  val create :
    contents:string -> target:Path.t -> chmod_x:bool -> t
  val contents : t -> string
  val target : t -> Path.t
  val chmod_x : t -> bool
end

module Process_proxy : sig
  type t
  [@@deriving sexp_of, compare]
  val create :
    dir:Path.t -> prog:string -> args:string list ->
    ignore_stderr:bool -> sandbox:Sandbox_kind.t -> t
  val dir : t -> Path.t
  val prog : t -> string
  val args : t -> string list
  val ignore_stderr : t -> bool
  val sandbox : t -> Sandbox_kind.t
end

module Action_proxy : sig
  type t =
    | Process of Process_proxy.t
    | Save of Save_proxy.t
  [@@deriving sexp_of, compare]
  include Hashable_binable with type t := t
end

module Rule_proxy : sig
  type t = {
    targets : Proxy_map.t;
    deps : Proxy_map.t;
    action : Action_proxy.t
  } [@@deriving hash, compare, fields]
end

module Output_proxy : sig
  type t = {
    deps : Proxy_map.t;
    stdout : string;
  } [@@deriving hash, compare, fields]
end

type t

val create : unit -> t

val digest_cache : t -> (Stats.t * Digest.t) Path.Table.t
val generated : t -> Path.Set.t Path.Rel.Table.t
val ruled : t -> Rule_proxy.t Path.Rel.Table.t (* actions run for target-rules *)
val actioned : t -> Output_proxy.t Action_proxy.Table.t (* actions run for their stdout *)

module With_index : sig
  type outer_t
  type t [@@deriving bin_io, sexp_of]
  module Index : sig
    type t
    val iter : t -> f:(Proxy_map.t -> unit) -> unit
  end
  val snapshot : outer_t -> t
  val value : t -> outer_t
  val index : t -> Index.t
end with type outer_t := t
