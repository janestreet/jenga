open Core
open! Int.Replace_polymorphic_compare
open Async

open Command.Let_syntax
let return = Async.return

let to_sexp =
  Command.async_or_error
    ~summary:"cat .jenga/db in sexp format (big!)"
    [%map_open
      let path =
        (* This flag is meant so in the test, we can print some subset of the data without
           relying on the sexp tool, whose output could change over time. *)
        flag "-path" ~doc:" which subsexp to print" (optional string)
      in fun () ->
       Writer.behave_nicely_in_pipeline ();
       match Special_paths.discover_and_set_root () with
       | Error _ as e -> return e
       | Ok () ->
         Persist.load_db ()
         >>|? fun db ->
         let sexp = Db.With_index.sexp_of_t (Db.With_index.snapshot db) in
         let sexp = Sexplib.Path.get ?str:path sexp in
         Print.printf !"%{Sexp#hum}\n%!" sexp
    ]

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
  Command.async_or_error
    ~summary:"print various stats to stdout"
    [%map_open
      let () = return ()
      in fun () ->
       Writer.behave_nicely_in_pipeline ();
       match Special_paths.discover_and_set_root () with
       | Error _ as e -> return e
       | Ok () ->
         memory_use_of_result_of Persist.load_db
         >>=? fun (db, size_in_mem) ->
         Unix.stat (Persist.get_db_filename ())
         >>= fun stat ->
         let size_on_disk = Byte_units.create `Bytes (Int64.to_float stat.size) in
         let avg iter v f =
           let count = ref 0 in
           let total = ref 0 in
           iter v ~f:(fun data -> incr count; total := !total + f data);
           [%sexp
             { num_entries = (!count : int)
             ; avg_num_deps = (if !count = 0
                               then 0.
                               else float !total /. float !count : float)
             }]
         in
         let db_with_index = Db.With_index.snapshot db in
         let sexp = [%sexp
           { size_on_disk = (Byte_units.to_string_hum size_on_disk : string)
           ; size_in_mem = (Byte_units.to_string_hum size_in_mem : string)
           ; num_digest_cache_entries = (Hashtbl.length (Db.digest_cache db) : int)
           ; num_generated_entries = (Hashtbl.length (Db.generated db) : int)
           ; index = (avg Db.With_index.Index.iter (Db.With_index.index db_with_index)
                       (fun t -> Db.Proxy_map.shallow_length t) : Sexp.t)
           ; ruled = (avg Hashtbl.iter (Db.ruled db)
                       (fun t -> Db.Proxy_map.shallow_length t.deps) : Sexp.t)
           ; actioned = (avg Hashtbl.iter (Db.actioned db)
                          (fun t -> Db.Proxy_map.shallow_length t.deps) : Sexp.t)
           }]
         in
         Print.printf !"%{Sexp#hum}\n%!" sexp;
         return (Ok ())
    ]


let command =
  Command.group
    ~summary:"query/dump info from jenga's persistent database"
    [
      "stats" , stats;
      "to-sexp" , to_sexp;
    ]
