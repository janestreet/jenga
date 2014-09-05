
open Async.Std

(* [Dep_type.t] is the GADT implementation behind [Dep.t]. Values of this type are
   interpreted by jenga's build algorithm.*)

type _ t =
| Return : 'a -> 'a t
| Bind : 'a t * ('a -> 'b t) -> 'b t
| All : 'a t list -> 'a list t
| Cutoff : ('a -> 'a -> bool) * 'a t -> 'a t
| Deferred : (unit -> 'a Deferred.t) -> 'a t
| Action_stdout : Action.t t -> string t
| Alias : Alias.t -> unit t
| Path : Path.t -> unit t
| Source_if_it_exists : Path.t -> unit t
| Contents : Path.t -> string t
| Reflect_path : Path.t -> Reflected.Trip.t option t
| Reflect_alias : Alias.t -> Path.Set.t t
| Reflect_putenv : (string * string) list t
| Buildable_targets : Path.t -> Path.Set.t t
| Glob_listing_OLD : Fs.Glob.t -> Path.Set.t t
| Glob_listing : Fs.Glob.t -> Path.Set.t t (* FS or buildable *)
| Glob_change_OLD : Fs.Glob.t -> unit t
| Glob_change : Fs.Glob.t -> unit t (* FS or buildable *)
