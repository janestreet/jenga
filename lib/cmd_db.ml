
open Core.Std
open! No_polymorphic_compare
open Async.Std

module Spec = Command.Spec

let to_sexp =
  Command.async_or_error Spec.empty
    ~summary:"cat .jenga.db in sexp format (big!)"
    (fun () ->
       Writer.behave_nicely_in_pipeline ();
       match Special_paths.discover_root () with
       | Error _ as e -> return e
       | Ok () ->
         Persist.load_db ()
         >>|? fun db ->
         Print.printf !"%{sexp#hum:Db.t}\n%!" db
    )

let memory_use_of_result_of =
  let live_words_now () =
    Gc.full_major ();
    (Gc.stat ()).live_words in
  fun f ->
    let before = live_words_now () in
    f () >>|? fun res ->
    let after = live_words_now () in
    res, Byte_units.create `Words (Float.of_int (after - before))

let stats =
  Command.async_or_error Spec.empty
    ~summary:"print various stats to stdout"
    (fun () ->
       Writer.behave_nicely_in_pipeline ();
       match Special_paths.discover_root () with
       | Error _ as e -> return e
       | Ok () ->
         memory_use_of_result_of Persist.load_db
         >>=? fun (db, size_in_mem) ->
         Unix.stat (Persist.get_db_filename ())
         >>= fun stat ->
         let size_on_disk = Byte_units.create `Bytes (Int64.to_float stat.size) in
         let avg table f =
           let count = Hashtbl.length table in
           let total =
             Hashtbl.fold table ~init:0 ~f:(fun ~key:_ ~data acc -> acc + f data)
           in
           float total /. float count
         in
         let sexp = <:structural_sexp<
           { size_on_disk = (Byte_units.to_string_hum size_on_disk : string)
           ; size_in_mem = (Byte_units.to_string_hum size_in_mem : string)
           ; num_digest_cache_entries = (Hashtbl.length (Db.digest_cache db) : int)
           ; num_generated_entries = (Hashtbl.length (Db.generated db) : int)
           ; num_ruled_entries = (Hashtbl.length (Db.ruled db) : int)
           ; avg_ruled_num_deps = (avg (Db.ruled db) (fun t -> Map.length t.deps) : float)
           ; num_actioned_entries = (Hashtbl.length (Db.actioned db) : int)
           ; avg_actioned_num_deps = (avg (Db.actioned db) (fun t -> Map.length t.deps) : float)
           } >>
         in
         Print.printf !"%{Sexp#hum}\n%!" sexp;
         return (Ok ())
    )


let command =
  Command.group
    ~summary:"query/dump info from jenga's persistent database"
    [
      "stats" , stats;
      "to-sexp" , to_sexp;
    ]
