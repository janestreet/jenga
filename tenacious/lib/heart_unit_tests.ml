open Core
open Async
open Int.Replace_polymorphic_compare

module Heart = Heart
module Glass = Heart.Glass

let%test_unit _ = (
  (* Check [when_broken] has strong reference to the heart being watched *)
  let g1 = Glass.create () in
  let g2 = Glass.create () in
  let heart = Heart.combine2 (Heart.watch g1) (Heart.watch g2) in
  let def = Heart.when_broken heart in
  let def = def >>| Fn.id in
  Gc.full_major();
  Gc.full_major();
  Glass.break g1;
  Async_kernel_scheduler.run_cycles_until_no_jobs_remain();
  assert (Deferred.is_determined def)
)

module Memory = struct

  let gc () =
    Gc.full_major ();
    Gc.full_major ()

  let used_memory () =
    let stat = Gc.stat () in
    stat.Gc.Stat.live_words

  let iterate n ~f =
    let rec go i =
      if n <= i
      then ()
      else (f i; go (i + 1))
    in
    go 0
  ;;
  (** do [n] iterations of [job] using up to [words_limit] words per iteration *)
  let ensure_leaks_at_most ~words_limit n job =
    gc ();
    let start_mem = used_memory () in
    (* allow 100 words for benchmark overhead *)
    let total_limit = words_limit * n + 100 in
    iterate n ~f:(fun i ->
      job ();
      gc ();
      let additional_memory = used_memory () - start_mem in
      if total_limit < additional_memory
      then
        failwithf
          "memory limit exceeded on %dth iteration! additional memory used: %d"
          i additional_memory ())

end

let%test_unit _ = (
  let h1 = Heart.watch (Heart.Glass.create ()) in
  let h2 = Heart.watch (Heart.Glass.create ()) in
  let job () =
    Memory.iterate 100 ~f:(fun _ ->
      let _ = Heart.combine2 h1 h2 in ()
    )
  in
  Memory.ensure_leaks_at_most
    ~words_limit:0
    30
    job
)
