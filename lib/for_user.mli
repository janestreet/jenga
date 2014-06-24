
open Core.Std
open Async.Std

val install_config_for_user_rules : Config.t -> unit
val install_fs_for_user_rules : Fs.t -> unit

val config : unit -> Config.t
val fs : unit -> Fs.t

val load_sexp_for_jenga : (Sexp.t -> 'a) -> Path.t -> 'a Deferred.t
val load_sexps_for_jenga : (Sexp.t -> 'a) -> Path.t -> 'a list Deferred.t
