
open Core.Std
open Async.Std

(* Query for grabbing the latest progress,
   expressed as the fraction: built targets / all targets
*)

module Fraction : sig
  type t = int * int with bin_io
end

val progress_stream : (unit,Fraction.t,unit) Rpc.Pipe_rpc.t
