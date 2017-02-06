open Core
open Async
open! Int.Replace_polymorphic_compare

type 'a cached_exec = ('a * Db.Proxy_map.t) Builder.t
type 'a cached_reflect = ('a * Path.Set.t) Builder.t

(** [Dep_type.t] is the GADT implementation behind [Dep.t]. Values of this type are
    interpreted by jenga's build algorithm.*)

type 'a t =
| Return : 'a -> 'a t
| Map : 'a t * ('a -> 'b) -> 'b t
| Bind : 'a t * ('a -> 'b t) -> 'b t
| All : 'a sexp_opaque t list -> 'a list t
| Cutoff : ('a -> 'a -> bool) * 'a t -> 'a t
| Deferred : (unit -> 'a Deferred.t) -> 'a t
| Action_stdout : Action.t t -> string t
| Alias : Alias.t -> unit t
| Path : Path.t -> unit t
| Group_dependencies : 'a t -> 'a t
| Source_if_it_exists : Path.t -> unit t
| Contents : Path.t -> string t
| Reflect_path : Path.t -> Reflected.Trip.t option t
| Reflect_alias : Alias.t -> Path.Set.t t
| Reflect_putenv : (string * string option) list t
| Buildable_targets : Path.t -> Path.Set.t t
| Source_files : Path.t -> Path.Set.t t
| Glob_listing_OLD : Fs.Glob.t -> Path.Set.t t
| Glob_listing : Fs.Glob.t -> Path.Set.t t (* FS or buildable *)
| Glob_change_OLD : Fs.Glob.t -> unit t
| Glob_change : Fs.Glob.t -> unit t (* FS or buildable *)
| Var : 'a Var.t -> 'a t
| Memoize : { name : string
            ; t : 'a t
            ; mutable cached_exec : 'a cached_exec option sexp_opaque
            ; mutable cached_reflect : 'a cached_reflect option sexp_opaque
            } -> 'a t
[@@deriving sexp_of] (* sexp_of_t is only usable for debugging *)
