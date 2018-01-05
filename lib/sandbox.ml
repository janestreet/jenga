open! Core
open! Async
open! Int.Replace_polymorphic_compare

type kind = Db.Sandbox_kind.t =
  | No_sandbox
  | Hardlink
  | Copy
  | Hardlink_ignore_targets
  | Copy_ignore_targets
[@@deriving sexp]

type error =
  | Creation_failed of exn
  | Closing_failed of exn
  | Unexpected_targets of string list
  | Missing_targets of Path.Rel.t list
[@@deriving sexp_of]

let rec remove_existing_file_recursively name =
  let%bind () =
    let%bind stats = Unix.lstat name in
    match stats.kind with
    | `Directory ->
      let%bind children = Sys.ls_dir name in
      Deferred.List.iter children
        ~f:(fun child -> remove_existing_file_recursively (name ^ "/" ^ child));
    | _ -> return ()
  in
  Unix.remove name

(* With hardlinks around, [Unix.rename] can succeed without actually unlinking the source
   file. This function ensures that the source file is unlinked. *)
let safe_rename ~src ~dst =
  let%bind () = Unix.rename ~src ~dst in
  match%bind Sys.file_exists src with
  | `Yes | `Unknown -> Unix.remove src
  | `No -> return ()

let rec move_existing_file_recursively src_name dest_name =
  let%bind stats = Unix.lstat src_name in
  match stats.kind with
  | `Directory -> begin
      let%bind () =
        match%map Monitor.try_with (fun () -> Unix.mkdir dest_name) with
        | Ok () -> ()
        | Error e ->
          match Monitor.extract_exn e with
          | Unix.Unix_error (EEXIST, _, _) -> ()
          | _ -> raise e
      in
      let%bind children = Sys.ls_dir src_name in
      let%bind () =
        Deferred.List.iter children
          ~f:(fun child ->
                let src_name = src_name ^ "/" ^ child in
                let dest_name = dest_name ^ "/" ^ child in
                move_existing_file_recursively src_name dest_name)
      in
      Unix.remove src_name
    end
  | _ -> safe_rename ~src:src_name ~dst:dest_name

let move_existing_file_recursively root =
  move_existing_file_recursively (Path.to_string root) (Path.to_string Path.the_root)
;;

let create_dirs ~root ~paths =
  let root_name = Path.to_string root in
  let%bind () =
    match%bind Sys.is_directory root_name with
    | `Yes -> remove_existing_file_recursively root_name
    | `No | `Unknown -> return ()
  in
  let%bind () = Unix.mkdir root_name in
  let rec inner_loop path =
    if Path.Rel.equal path Path.Rel.the_root then return []
    else begin
      let parent = Path.Rel.dirname path in
      let%bind acc = inner_loop parent in
      let from_root = Path.Rel.to_string path in
      let dest = Path.relative ~dir:root from_root in
      let name = Path.to_string dest in
      match%bind Sys.is_directory name with
      | `Yes -> return acc
      | `No | `Unknown ->
        let%bind () = Unix.mkdir name in
        return (from_root :: acc)
    end
  in
  Deferred.List.map paths ~f:inner_loop
  >>| List.cons ["."] (* the root directory *)
  >>| List.rev (* inner directories first, to simplify remove_dirs *)
  >>| List.concat

let remove_dirs ~root ~dirs =
  Deferred.List.fold dirs ~init:(Ok ())
    ~f:(fun acc dir ->
      match acc with
      | Error _ as err -> return err
      | Ok () ->
        let dest = Path.relative ~dir:root dir in
        let name = Path.to_string dest in
        match%bind Sys.ls_dir name with
        | [] -> Unix.rmdir name >>| fun () -> Ok ()
        | _ :: _ as l ->
          let targets = List.map (List.take l 5) ~f:(fun f -> Filename.concat dir f) in
          return (Error (Unexpected_targets targets)))

let copy_file ~source ~dest =
  let%bind stats = Unix.stat source in
  let perm = stats.perm in
  Reader.with_file source ~f:(fun source ->
    Writer.with_file dest ~perm ~f:(fun dest ->
      match%map
        Reader.read_one_chunk_at_a_time source
          ~handle_chunk:(fun buf ~pos ~len ->
            Writer.write_bigstring dest buf ~pos ~len;
            let%map () = Writer.flushed dest in
            `Continue
          )
      with
      | `Eof -> ()
      | `Stopped nothing -> Nothing.unreachable_code nothing
      | `Eof_with_unconsumed_data _ ->
        failwith "can't happen because we don't return `Consumed"
    ))

let link_files ~root ~paths ~how =
  let rec follow_symlinks name =
    let%bind stats = Unix.lstat name in
    match stats.kind with
    | `Link ->
      let%bind link_name = Unix.readlink name in
      let name = (Filename.dirname name) ^ "/" ^ link_name in
      follow_symlinks name
    | _ -> return name
  in
  Deferred.List.map paths
    ~f:(fun path ->
      let from_root = Path.Rel.reach_from ~dir:Path.Rel.the_root path in
      let src = Path.relative ~dir:Path.the_root from_root in
      let dest = Path.relative ~dir:root from_root in
      let src_name = Path.to_string src in
      let dest_name = Path.to_string dest in
      let%bind src_name = follow_symlinks src_name in
      let%map () =
        match how with
        | `Copy -> copy_file ~source:src_name ~dest:dest_name
        | `Hardlink -> Unix.link ~target:src_name ~link_name:dest_name ()
      in
      dest_name)

let touch_files ~root ~paths =
  Deferred.List.map paths
    ~f:(fun path ->
      let dest = Path.relative ~dir:root (Path.Rel.to_string path) in
      let name = Path.to_string dest in
      let%bind fd = Unix.openfile ~mode:[`Creat; `Wronly; `Trunc] name in
      let%map () = Unix.close fd in
      name)

let unlink_files ~paths =
  Deferred.List.iter ~f:Unix.unlink paths

let restore_targets ~root ~paths =
  let%map res =
    Deferred.List.map paths
      ~f:(fun path ->
          let from_root = Path.Rel.to_string path in
          let src = Path.relative ~dir:root from_root in
          let dest = Path.relative ~dir:Path.the_root from_root in
          let src_name = Path.to_string src in
          let dest_name = Path.to_string dest in
          Monitor.try_with (fun () -> safe_rename ~src:src_name ~dst:dest_name)
          >>| function
          | Ok () -> Ok ()
          | Error exn ->
            match Monitor.extract_exn exn with
            | Unix.Unix_error (ENOENT, _, _) -> Error path
            | _ -> raise exn)
  in
  let res =
    List.fold_left ~init:(Ok ()) res
      ~f:(fun acc res ->
        match acc, res with
        | Ok (), Ok () -> acc
        | Error _, Ok () -> acc
        | Ok (), Error path -> Error [path]
        | Error paths, Error path -> Error (path :: paths))
  in
  match res with
  | Ok () as res -> res
  | Error paths -> Error (Missing_targets paths)

type t = {
  root : Path.t;
  dirs : string list;
  files : string list;
  empty_files : string list;
  ignore_targets : bool;
}

let root t = t.root

module M = Unique_id.Int()

let initialize_exn =
  let v =
    Lazy_deferred.create (fun () ->
      Process.run_expect_no_output_exn ~prog:"rm" ~args:[ "-rf"; ".jenga/sandbox" ] ()
      >>= fun () ->
      (* We place the hg directory outside each sandbox, so paths relative to hg
         will be wrong (hg root is not necessarily the jenga root, so it's not that
         straightforward to make hg root work). We write a [requires] file to give
         a somewhat decent error message. *)
      Unix.mkdir ~p:() ".jenga/sandbox/.hg"
      >>= fun () ->
      Writer.save ".jenga/sandbox/.hg/requires"
        (* this must be in one line *)
        ~contents:"ATTN: [hg root] may produce unexpected results when called from jenga \
                   builds; sandboxed builds forbid it. Consider using paths relative to \
                   [jenga root] (or [Jenga_rules_integration.blocking_root] in OCaml) \
                   instead."
    )
  in
  fun () ->
    Lazy_deferred.force_exn v

let with_sandbox ~dir ~deps ~kind ~targets ~f =
  Monitor.try_with (fun () ->
    initialize_exn ()
    >>= fun () ->
    let root = Path.root_relative (sprintf ".jenga/sandbox/%d" (M.create () :> int)) in
    let `Dirs dirs, `Files files, `Arbitrary_files empty_files =
      Db.Proxy_map.filesystem_assumptions deps
    in
    List.iter targets
      ~f:(fun target -> Hash_set.add dirs (Path.Rel.dirname target));
    begin match Path.case dir with
    | `absolute _ -> ()
    | `relative dir -> Hash_set.add dirs dir
    end;
    (* Add jenga.conf so commands can find the root whether the command is running
       sandboxed or not (or even not within jenga). *)
    Hash_set.add empty_files Special_paths.jenga_conf;
    let dirs = Hash_set.to_list dirs in
    let empty_files = Hash_set.to_list (Hash_set.diff empty_files files) in
    let files = Hash_set.to_list files in
    let how_to_link, ignore_targets =
      match kind with
      | No_sandbox -> failwith "Sandbox.start"
      | Hardlink -> `Hardlink, false
      | Hardlink_ignore_targets -> `Hardlink, true
      | Copy -> `Copy, false
      | Copy_ignore_targets -> `Copy, true
    in
    let%bind dirs = create_dirs ~root ~paths:dirs in
    let%bind files = link_files ~root ~paths:files ~how:how_to_link in
    let%map empty_files = touch_files ~root ~paths:empty_files in
    { root; dirs; files; empty_files; ignore_targets }
  ) >>= function
  | Error exn ->
    let exn = Monitor.extract_exn exn in
    return (Error (Creation_failed exn))
  | Ok ({ root; dirs; files; empty_files; ignore_targets } as box) ->
    match%bind f box with
    | Error _ as err ->
      Monitor.try_with (fun () ->
        let%bind () = unlink_files ~paths:empty_files in
        let%bind () = unlink_files ~paths:files in
        move_existing_file_recursively root
      ) >>| fun _ -> Ok err
    | Ok _ as res ->
      Monitor.try_with ~extract_exn:true (fun () ->
        let%bind () = unlink_files ~paths:empty_files in
        let%bind () = unlink_files ~paths:files in
        match%bind restore_targets ~root ~paths:targets with
        | Error _ as err -> return err
        | Ok () ->
          if ignore_targets then
            let%map () = move_existing_file_recursively root in
            Ok ()
          else begin
            remove_dirs ~root ~dirs
          end
      ) >>= function
      | Error exn ->
        Monitor.try_with (fun () ->
          move_existing_file_recursively root
        ) >>| fun _ ->
        Error (Closing_failed exn)
      | Ok (Error _ as err) ->
        Monitor.try_with (fun () ->
          move_existing_file_recursively root
        ) >>| fun _ -> err
      | Ok (Ok ()) -> return (Ok res)
