
open Core.Std
open Async.Std

module Request = struct
  type t = {
    putenv : (string * string) list;
    dir : Path.X.t;
    prog : string;
    args : string list;
  }
end

module Reply = struct
  type t = {
    stdout : string;
    stderr : string;
    outcome : [`success | `error of string];
  }
end

let do_the_fork {Request. putenv; dir; prog; args} =
  try_with (fun () ->
    List.iter putenv ~f:(fun (key,data) -> Core.Std.Unix.putenv ~key ~data);
    let working_dir = Path.X.to_absolute_string dir in
    Async_shell.run_full_and_error ~working_dir prog args
  ) >>= fun result ->
  match result with
  | Ok (stdout,stderr) ->
    return { Reply. stdout; stderr; outcome = `success }
  | Error exn ->
    let exn = Monitor.extract_exn exn in
    let module SP = Async_shell.Process in
    match exn with
    | Async_shell.Process.Failed res ->
      let stdout = res.SP.stdout in
      let stderr = res.SP.stderr in
      let outcome = `error (SP.status_to_string res.SP.status) in
      return { Reply. stdout; stderr; outcome }
    | _ ->
      let stdout = "" in
      let stderr = "" in
      let outcome = `error (Exn.to_string exn) in
      return { Reply. stdout; stderr; outcome }

let spawned_forker_function hub =
  let pipe_reader = Parallel.Std.Hub.listen_simple hub in
  don't_wait_for (
    let rec loop () =
      Pipe.read pipe_reader >>= function
      | `Eof -> failwith "Forker.spawned_forker_function:Eof"
      | `Ok (client,(uid,request)) ->
        don't_wait_for (
          do_the_fork request >>= fun reply ->
          Parallel.Std.Hub.send hub client (uid,reply);
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
    channel_def : (int * Request.t, int * Reply.t) Parallel.Std.Channel.t Deferred.t;
    reply_table : Reply.t Ivar.t Int.Table.t;
  }

  let create () =
    let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
    let channel_def = Parallel.Std.Parallel.spawn spawned_forker_function >>| fst in
    let reply_table = Int.Table.create () in
    don't_wait_for (
      channel_def >>= fun channel ->
      let rec loop () =
        Parallel.Std.Channel.read channel >>= fun (uid,reply) ->
        match (Hashtbl.find reply_table uid) with
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
    Parallel.Std.Channel.write channel (uid,request);
    Ivar.read ivar

end

type t = {
  (* empty list - no parallel forkers, so call do_the_fork in local process *)
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
    let t = { forkers } in
    t_opt_ref := Some t

let run ~putenv ~dir ~prog ~args =
  let t = the_t () in
  let request = {Request. putenv; dir; prog; args} in
  match t.forkers with
  | [] -> do_the_fork request
  | forker1::forkers ->
    t.forkers <- forkers @ [forker1]; (* round-robin the forkers *)
    Forker_proc.run forker1 request
