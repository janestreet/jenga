
open Core.Std
open Async.Std

module Request = struct
  type t = {
    putenv : (string * string) list;
    dir : Path.t;
    prog : string;
    args : string list;
  }
  let create ~putenv ~dir ~prog ~args = {putenv; dir; prog; args}
end

module Reply = struct
  type t = {
    stdout : string;
    stderr : string;
    outcome : [`success | `error of string];
  }
end

module type Fork_process_sig = sig
  val run : Request.t -> Reply.t Deferred.t
end

module Fork_process : Fork_process_sig = struct

  let run {Request. putenv; dir; prog; args} =
    List.iter putenv ~f:(fun (key,data) -> Core.Std.Unix.putenv ~key ~data);
    let working_dir = Path.to_absolute_string dir in
    Process.create ~working_dir ~prog ~args () >>= function
    | Error error ->
      let exn = Error.to_exn error in
      let outcome = `error (Exn.to_string exn) in
      let stdout = "" in
      let stderr = "" in
      return { Reply. stdout; stderr; outcome }
    | Ok process ->
      let module Output = Process.Output in
      Process.wait process >>= fun output ->
      let stdout = output.Output.stdout in
      let stderr = output.Output.stderr in
      let exit_status = output.Output.exit_status in
      let outcome =
        match exit_status with
        | Error _ -> `error (Unix.Exit_or_signal.to_string_hum exit_status)
        | Ok () -> `success
      in
      return { Reply. stdout; stderr; outcome }

end

let spawned_forker_function hub =
  let fork_throttle =
    (* We need 3 fds per forked process, so conservatively we
       throttle the number of forked processed to be < 1024/3 *)
    Throttle.create ~continue_on_error:true ~max_concurrent_jobs:300
  in
  let pipe_reader = Async_parallel.Std.Hub.listen_simple hub in
  don't_wait_for (
    let rec loop () =
      Pipe.read pipe_reader >>= function
      | `Eof -> failwith "Forker.spawned_forker_function:Eof"
      | `Ok (client,(uid,request)) ->
        don't_wait_for (
          Throttle.enqueue fork_throttle (fun () ->
            Fork_process.run request
          ) >>= fun reply ->
          Async_parallel.Std.Hub.send hub client (uid,reply);
          return ()
        );
        loop ()
    in
    loop ()
  );
  Deferred.never()

module Forker_proc : sig

  type  t
  val create : unit -> t
  val run : t -> Request.t -> Reply.t Deferred.t

end = struct

  type t = {
    genU : (unit -> int);  (* to match reply with request *)
    channel_def : (int * Request.t, int * Reply.t) Async_parallel.Std.Channel.t Deferred.t;
    reply_table : Reply.t Ivar.t Int.Table.t;
  }

  let create () =
    let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
    let channel_def = Async_parallel.Std.Parallel.spawn spawned_forker_function >>| fst in
    let reply_table = Int.Table.create () in
    don't_wait_for (
      channel_def >>= fun channel ->
      let rec loop () =
        Async_parallel.Std.Channel.read channel >>= fun (uid,reply) ->
        match (Hashtbl.find_and_remove reply_table uid) with
        | None -> failwith "Forker_proc, unknown reply uid";
        | Some ivar ->
          Ivar.fill ivar reply;
          loop()
      in
      loop ()
    );
    { genU; channel_def; reply_table; }

  let run t request =
    let uid = t.genU() in
    let ivar = Ivar.create () in
    Hashtbl.add_exn t.reply_table ~key:uid ~data:ivar;
    t.channel_def >>= fun channel ->
    Async_parallel.Std.Channel.write channel (uid,request);
    Ivar.read ivar

end

type t = {
  (* empty list - no parallel forkers, do the fork in the local process *)
  mutable forkers : Forker_proc.t list;
}

let t_opt_ref = ref None

let the_t () = match !t_opt_ref with | None -> assert false | Some t -> t

let init config =
  match !t_opt_ref with
  | Some _ -> failwith "Forker.init() called more than once"
  | None ->
    let n = Config.f_number config in
    Message.trace "Forker.init() - N = %d" n;
    let forkers = List.init n ~f:(fun _ -> Forker_proc.create ()) in
    assert (List.length forkers = n);
    let t = { forkers; } in
    t_opt_ref := Some t

let run request =
  let t = the_t () in
  match t.forkers with
  | [] -> Fork_process.run request
  | forker1::forkers ->
    t.forkers <- forkers @ [forker1]; (* round-robin the forkers *)
    Forker_proc.run forker1 request
