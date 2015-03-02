
open Core.Std

(* [Db] contains types which will be stored persistently. *)

module Job : sig
  type t
  with sexp_of, bin_io, compare
  include Hashable_binable with type t := t
  val create : dir:Path.t -> prog:string -> args:string list -> t
  val dir : t -> Path.t
  val prog : t -> string
  val args : t -> string list
end

module Kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  with sexp, bin_io, compare
  val to_string : t -> string
end

module Mtime : sig
  type t
  with sexp_of, bin_io, compare
  val of_float : float -> t
  val equal : t -> t -> bool
end

module Stats : sig  (* reduced stat info *)
  type t
  with sexp_of, bin_io
  val of_unix_stats : Async.Std.Unix.Stats.t -> t
  val equal : t -> t -> bool
  val kind : t -> Kind.t
  val mtime : t -> Mtime.t
end

module Digest : sig  (* proxy for the contents of a file in the file-system *)

  type t
  with sexp_of, bin_io, compare
  val intern : string -> t

  module With_store : sig
    type 'a t with bin_io
    val snapshot : 'a -> 'a t
    val value : 'a t -> 'a
  end

end

module Listing : sig (* result of globbing *)
  type t with sexp_of, bin_io, compare
  module Elem : sig
    type t
    val create : base:string -> kind:Kind.t -> t
  end
  val create : dir:Path.t -> elems:Elem.t list -> t
  val of_file_paths_exn : dir:Path.t -> Path.t list -> t
  val equal : t -> t -> bool
  val paths : t -> Path.Set.t
  module Restriction : sig
    type t with sexp_of, bin_io
    val create : kinds:Kind.t list option -> Pattern.t -> t
    val to_string : t -> string
    val pattern : t -> Pattern.t
    val kind_allows_file : t -> bool
    val compare : t -> t -> int
  end
  val restrict : t -> Restriction.t -> t
end

module Glob : sig
  type t with sexp, bin_io
  include Hashable with type t := t
  val create : dir:Path.t -> restriction:Listing.Restriction.t -> t
  val dir : t -> Path.t
  val restriction : t -> Listing.Restriction.t
  val to_string : t -> string
end

module Pm_key : sig  (* Pm_key - path or glob *)
  type t with sexp_of, bin_io, compare
  include Comparable_binable with type t := t
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
  type t with sexp_of, bin_io, compare
  val of_digest : Digest.t -> t
  val of_listing : dir:Path.t -> Path.Set.t -> t
  val equal : t -> t -> bool
end

module Proxy_map : sig
  type t = Proxy.t Pm_key.Map.t with sexp_of, bin_io, compare
  val empty  : t
  val single : Pm_key.t -> Proxy.t -> t
end

module Rule_proxy : sig
  type t = {
    targets : Proxy_map.t;
    deps : Proxy_map.t;
    action : Job.t
  } with sexp_of, bin_io, compare, fields
end

module Output_proxy : sig
  type t = {
    deps : Proxy_map.t;
    stdout : string;
  } with sexp_of, bin_io, compare, fields
end

type t with sexp_of, bin_io

val create : unit -> t

val digest_cache : t -> (Stats.t * Digest.t) Path.Table.t
val generated : t -> Path.Set.t Gen_key.Table.t
val ruled : t -> Rule_proxy.t Path.Rel.Table.t (* actions run for target-rules *)
val actioned : t -> Output_proxy.t Job.Table.t (* actions run for their stdout *)
