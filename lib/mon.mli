
open Core.Std

module Progress : sig

  type t = {

    (* todo *)
    checking  : int;
    blocked   : int;
    jwait     : int;
    running   : int; (* external scanner/action *)
    usercode  : int; (* internal generator/scanner/action *)

    (* done; ok *)
    source    : int;
    target    : int;
    alias     : int;
    scanner   : int;
    glob      : int;

    (* done; bad *)
    error     : int;
    failure   : int; (* error in dep *)

  } with bin_io


  val total : t -> int
  val todo : t -> int
  val fraction : t -> (int*int) (* good/total *)
  val completed : t -> bool (* good = total *)

  val to_string : [`full|`brief] -> t -> string

  val readme : unit -> string

end
