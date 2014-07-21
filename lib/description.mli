
open Core.Std
open Async.Std

module Generator : sig
  type t = Rule.t list Dep.t
  val create : Rule.t list Dep.t -> t
  (*val switch : cond:(Path.t -> bool) -> t -> t -> t*)
  val run : t -> Rule.t list Dep.t
end

module Scheme : sig
  type t
  val create : tag:string -> (dir:Path.t -> Generator.t) -> t
  val tag : t -> string
  val body : t -> (dir:Path.Rel.t -> Generator.t) ref
end

module For_user : sig
  val install_config_for_user_rules : Config.t -> unit
  val config : unit -> Config.t
end

