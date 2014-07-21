
open Async.Std

type t

type rep = {
  putenv : (string * string) list;
  command_lookup_path : [`Replace of string list | `Extend of string list] option;
  schemes : (Pattern.t * Description.Scheme.t option) list;
  build_begin : (unit -> unit Deferred.t);
  build_end : (unit -> unit Deferred.t);
}

val rep : t -> rep

val create :
  ?putenv : (string * string) list ->
  ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
  ?build_begin : (unit -> unit Deferred.t) ->
  ?build_end : (unit -> unit Deferred.t) ->
  (string * Description.Scheme.t option) list ->
  t
