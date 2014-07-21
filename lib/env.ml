
open Core.Std
open Async.Std

module Scheme = Description.Scheme

type rep = {
  putenv:(string * string) list;
  command_lookup_path : [`Replace of string list | `Extend of string list] option;
  schemes : (Pattern.t * Scheme.t option) list;
  build_begin : (unit -> unit Deferred.t);
  build_end : (unit -> unit Deferred.t);
}

type t = rep

let rep t = t

let create
    ?(putenv=[])
    ?command_lookup_path
    ?(build_begin=(fun () -> Deferred.return ()))
    ?(build_end=(fun () -> Deferred.return ()))
    schemes =
  {
    putenv;
    command_lookup_path;
    schemes =
      List.map schemes ~f:(fun (string,scheme) ->
        Pattern.create_from_glob_string string, scheme
      );
    build_begin;
    build_end;
  }
