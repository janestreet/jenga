open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

module Heart = Tenacious.Heart

let equal_using_compare compare = fun x1 x2 -> 0 = compare x1 x2

let exit_code_upon_control_c = ref Exit_code.incomplete

module Target_rule = Rule.Target_rule
module Digest = Fs.Digest
module Glob = Fs.Glob
module Pm_key = Db.Pm_key
module Proxy = Db.Proxy
module Proxy_map = Db.Proxy_map
module Rule_proxy = Db.Rule_proxy
module Output_proxy = Db.Output_proxy

module PPs = struct (* list of path-tagged proxies *)
  type t = (Path.t * Proxy.t) list
end

module RR = Run_reason

(*----------------------------------------------------------------------
 proxy map operations
----------------------------------------------------------------------*)

module Proxy_map_op : sig

  type t = Db.Proxy_map.t [@@deriving hash, compare]

  val empty  : t
  val single : Pm_key.t -> Proxy.t -> t

  type inconsistency = (Pm_key.t * Proxy.t list) list
  [@@deriving sexp_of]

  val create_by_path : PPs.t -> (t, inconsistency) Result.t
  val merge : t list -> (t, inconsistency) Result.t

  val equal : t -> t -> bool
  val diff : before:t -> after:t -> Pm_key.t list option

end = struct

  include Db.Proxy_map

  type inconsistency = (Pm_key.t * Proxy.t list) list [@@deriving sexp_of]

  let create = of_alist

  let create_by_path xs =
    create (List.map xs ~f:(fun (path,v) -> (Pm_key.of_path path,v)))

  let equal = equal_using_compare compare

  let diff ~before ~after =
    match Db.Proxy_map.equal_or_witness before after with
    | Ok () -> None
    | Error l -> Some l

end

module Problem : sig

  type t

  (** The list of individual errors together with dependency traces.
     (a, [b,c,d]) means
     "error 'a' happened at subgoal 'd',
     which was a part of subgoal 'c',
     which was a part of subgoal 'b'".
  *)
  val reasons : t -> (Reason.t * Goal.t list) list

  (** The list of errors that happened at the current subgoal.
    This should be true:
    [reasons_here t = List.filter_map (reasons t) ~f:(function
      | (r, []) -> Some r
      | (r, _) -> None)]
    *)
  val reasons_here : t -> Reason.t list

  (** This is a superset of the needs listed in [reasons]
      because a single Reason.t can be reachable by multiple path,
      all of needs along which are going to be in error.
  *)
  val needs_in_error : t -> Goal.Set.t

  val create : Reason.t -> t
  val all : t list -> t
  val subgoal : Goal.t -> t -> t

end = struct

  module Id = Unique_id.Int()

  (**
    [reasons] contains all errors collected transitively together with
    dependency paths via which they've been reached;

     [reasons_here] contains only the errors at this subgoal.
  *)
  type t = {
    reasons : (Reason.t * Goal.t list) Id.Map.t
  ; reasons_here : Reason.t Id.Map.t
  ; needs_in_error : Goal.Set.t
  }

  let union_left_biased = Map.merge ~f:(fun ~key:_ -> function
    | `Left l -> Some l
    | `Both (l, _) -> Some l
    | `Right r -> Some r
  )

  let reasons t = Map.data (t.reasons)
  let reasons_here t = Map.data (t.reasons_here)
  let needs_in_error t = t.needs_in_error

  let create r =
    let id = Id.create () in
    { reasons = Id.Map.singleton id (r, [])
    ; reasons_here = Id.Map.singleton id r
    ; needs_in_error = Goal.Set.empty
    }

  let merge a b =
    { reasons = union_left_biased a.reasons b.reasons
    ; reasons_here =
        union_left_biased
          a.reasons_here
          b.reasons_here
    ; needs_in_error =
        Set.union a.needs_in_error b.needs_in_error
    }

  let empty =
    { reasons = Id.Map.empty
    ; reasons_here = Id.Map.empty
    ; needs_in_error = Goal.Set.empty
    }

  let all = List.fold_left ~init:empty ~f:merge

  let subgoal need t =
    { reasons = Id.Map.map ~f:(fun (r, l) -> (r, need :: l)) t.reasons
    ; reasons_here = Id.Map.empty
    ; needs_in_error = Set.add t.needs_in_error need
    }

end

(*----------------------------------------------------------------------
  Builder
----------------------------------------------------------------------*)

module Builder : sig (* layer error monad within tenacious monad *)
  type 'a t

  val wrap : ('a, Problem.t) Result.t Tenacious.t -> 'a t
  val expose : 'a t -> ('a, Problem.t) Result.t Tenacious.t

  val of_tenacious : 'a Tenacious.t -> 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val ( *>>| ) : 'a t -> ('a -> 'b) -> 'b t

  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t

  val reify : name:String.t Lazy.t -> 'a t -> 'a t

  val error : Reason.t -> 'a t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  val subgoal : Goal.t -> 'a t -> 'a t

  val both : 'a t -> 'b t -> ('a * 'b) t

  val of_deferred : (unit -> 'a Deferred.t) -> 'a t

  val desensitize : 'a t -> ('a * Heart.t) t
  val sensitize : Heart.t -> unit t

  val bracket
    :  'a t
    -> running:(int -> unit)
    -> finished:(('a, Problem.t) Result.t -> unit)
    -> cancelled:(unit -> unit)
    -> 'a t

  val uncancellable : 'a t -> 'a t

  val return_result : ('a, Reason.t) Result.t -> 'a t
  val bind_result : 'a t -> ('a -> ('b, Reason.t) Result.t) -> 'b t

end = struct

  type 'a t = ('a, Problem.t) Tenacious.Result.t

  let cutoff ~equal x =
    Tenacious.cutoff
      ~equal:(fun res1 res2 ->
        match res1,res2 with
        | Ok x1, Ok x2 -> equal x1 x2
        (* never cutoff errors *)
        | Ok _, Error _
        | Error _, Ok _
        | Error _, Error _
          -> false
      ) x

  let wrap t = t
  let expose t = t

  let reify = Tenacious.reify

  let of_tenacious tenacious =
    Tenacious.map tenacious ~f:(fun x -> Ok x)

  let return = Tenacious.Result.return
  let bind = Tenacious.Result.bind
  let map x ~f = Tenacious.Result.map x ~f

  (* we can't use Tenacious.Result.all because:
     - it's sequential, but we want parallel
     - it only returns one error, but we want all of them *)
  let all xs =
    Tenacious.map (Tenacious.all xs) ~f:(fun ys ->
      let rec collect probs oks = function
        | Ok x :: xs -> collect probs (x::oks) xs
        | Error p :: xs -> collect (p::probs) oks xs
        | [] ->
          match probs with
          | [] -> Ok (List.rev oks)
          | _::_ -> Error (Problem.all (List.rev probs))
      in
      collect [] [] ys
    )

  let all_unit ts = map (all ts) ~f:(fun (_ : unit list) -> ())

  let error reason = Tenacious.Result.fail (Problem.create reason)

  let subgoal need builder =
    Tenacious.Result.map_error builder ~f:(Problem.subgoal need)

  let of_deferred f =
    Tenacious.lift (fun () ->
      f () >>| fun x -> (Ok x, Heart.unbreakable)
    )

  let desensitize t =
    Tenacious.bind (Tenacious.desensitize t) ~f:(function
    | (Ok x,heart) -> Tenacious.return (Ok (x,heart))
    | (Error e, heart) ->
      Tenacious.lift (fun () ->
        Deferred.return (Error e, heart)
      )
    )

  let sensitize heart =
    Tenacious.lift (fun () -> Deferred.return (Ok (), heart))

  let bracket t ~running ~finished ~cancelled =
    Tenacious.bracket t ~running ~finished ~cancelled

  let uncancellable builder = Tenacious.uncancellable builder

  let ( *>>| ) x f = map x ~f
  let both : ('a t -> 'b t -> ('a * 'b) t) =
    fun a b ->
      all [
        (a *>>| fun a -> `a a);
        (b *>>| fun b -> `b b);
      ] *>>| function
      | [`a a; `b b] -> (a,b)
      | _ -> assert false

  let return_result v =
    Tenacious.return
      (match v with
       | Error reason -> Error (Problem.create reason)
       | Ok _ as ok -> ok)

  let bind_result t f = Tenacious.map t ~f:(function
    | Error _ as e -> e
    | Ok v ->
      match f v with
      | Ok _ as ok -> ok
      | Error reason -> Error (Problem.create reason))

end
let _ = Builder.(return_result)
let return   = Builder.return
let ( *>>| ) = Builder.( *>>| )
let ( *>>= ) t f = Builder.bind t ~f
let ( *>>|= ) = Builder.bind_result
let error    = Builder.error

let build_all_in_sequence xs ~f = (* stopping on first error *)
  let rec loop acc = function
    | [] -> return (List.rev acc)
    | x::xs -> f x *>>= fun v -> loop (v::acc) xs
  in
  loop [] xs

let memo_builder :(
  memo: 'a Builder.t option ref -> (unit -> 'a Builder.t) -> 'a Builder.t
) =
  fun ~memo f ->
    match !memo with
    | Some builder -> builder
    | None ->
      let builder = Builder.reify ~name:(lazy "memo-builder") (f ()) in
      memo := Some builder;
      builder

(*----------------------------------------------------------------------
  Memo - dynamic cache - support sharing & cycle detection
----------------------------------------------------------------------*)

module Memo = struct

  (* The value in these hashtable are computations, and so are not sexp convertable.
     But that's ok, because they DONT get saved to file.

     The "ing" suffix is intended to indicate the continuous nature of these
     computatations *)

  type t = {
    reflecting : Reflected.Trip.t option Builder.t Path.Table.t;
    building  : Proxy_map.t Builder.t Goal.Table.t;
  }

  let create () = {
    reflecting = Path.Table.create();
    building = Goal.Table.create();
  }

end

(*----------------------------------------------------------------------
 jr_spec
----------------------------------------------------------------------*)

module Jr_spec = struct

  type t = In_root_dir | Path of Path.t | Env of (unit -> Env.t)

end

(*----------------------------------------------------------------------
  t - The downwards bucket type
----------------------------------------------------------------------*)

type t = {

  config : Config.t; (* messaging choices *)
  fs : Fs.t; (* file system *)
  persist : Persist.t; (* persisant cache *)
  memo : Memo.t; (* dynamic cache *)
  progress : Progress.t; (* track state of each goal being built *)
  jr_spec : Jr_spec.t; (* access to the jengaroot *)

  (* recursive calls to build_goal go via here - avoids having big mutual let rec *)
  build_goal : (t -> Goal.t -> Proxy_map.t Builder.t);
  reflect_path : (t -> Path.t -> Reflected.Trip.t option Builder.t);
  reflect_alias : (t -> Alias.t -> Path.Set.t Builder.t);
  buildable_targets : (t -> dir:Path.t -> Path.Set.t Builder.t);
  delete_eagerly
    : (t -> dir:Path.t -> (non_target:Path.t -> bool) Builder.t);
  delete_if_depended_upon
    : (t -> (non_target:Path.t -> bool) Builder.t);
  delete_union : (t -> non_target:Path.t -> bool Builder.t);
} [@@deriving fields]

let generated t = Db.generated (Persist.db t.persist)
let ruled t = Db.ruled (Persist.db t.persist)
let actioned t = Db.actioned (Persist.db t.persist)

let memo_reflecting t = t.memo.Memo.reflecting
let memo_building t = t.memo.Memo.building

let build_sub_goal : (t ->  Goal.t -> Proxy_map.t Builder.t) =
  fun t goal -> t.build_goal t goal

let apply_user_function' f =
  try Ok (f ()) with
  | exn -> Error (Reason.Usercode_raised (sexp_of_exn exn))
let apply_user_function f =
  try return (f ()) with
  | exn -> error (Reason.Usercode_raised (sexp_of_exn exn))

let run_user_async_code : ((unit -> 'a Deferred.t) -> 'a Builder.t) =
  fun f ->
    Builder.of_deferred (fun () ->
      Monitor.try_with ~extract_exn:true (fun () ->
        f ()
      )
    ) *>>= function
    | Ok x -> return x
    | Error exn ->
      error (Reason.Usercode_raised (sexp_of_exn exn))

(*----------------------------------------------------------------------
  File system interface
----------------------------------------------------------------------*)

let get_contents
    : (t -> Path.t -> string Builder.t) =
  fun t file ->
    Builder.of_tenacious (Fs.contents_file t.fs ~file)
    *>>= function
    | `file_read_error e    -> error (Reason.File_read_error (Error.sexp_of_t e))
    | `is_a_dir             -> error (Reason.Unexpected_directory file)
    | `contents s           -> return s

let digest_path
    : (t -> Path.t -> [ `file of Digest.t | `missing | `is_a_dir ] Builder.t) =
  fun t path ->
    Builder.of_tenacious (Fs.digest_file t.fs ~file:path)
    *>>= function
    | `stat_error e   -> error (Reason.File_read_error (Error.sexp_of_t e))
    | `is_a_dir       -> return `is_a_dir
    | `does_not_exist -> return `missing
    | `undigestable k -> error (Reason.Undigestable k)
    | `digest_error e -> error (Reason.Digest_error (Error.sexp_of_t e))
    | `digest digest  -> return (`file digest)

let look_for_source : (t -> Path.t -> Proxy.t option Builder.t) =
  fun t path ->
    digest_path t path *>>= function
    | `file digest    -> return (Some (Proxy.of_digest digest))
    | `missing        -> return None
    | `is_a_dir       -> return None

let glob_fs_only : (t -> Glob.t -> Path.Set.t Builder.t) =
  fun t glob ->
    let err s = Reason.Glob_error (glob,s) in
    Builder.of_tenacious (Fs.list_glob t.fs glob) *>>= function
    | Error e -> error (err (Error.to_string_hum e))
    | Ok (`not_a_dir) -> error (err "not a directory")
    | Ok `does_not_exist -> return Path.Set.empty
    | Ok (`listing listing) -> return (Db.Listing.paths listing)

let ensure_directory : (t -> dir:Path.Rel.t -> unit Builder.t) =
  fun t ~dir ->
    Builder.of_tenacious (Fs.ensure_directory t.fs ~dir:(Path.of_relative dir))
    *>>= function
    | `ok -> return ()
    | `not_a_dir -> error (Reason.No_directory_for_target "not a directory")
    | `failed _err -> error (Reason.No_directory_for_target "failed to create")

(*----------------------------------------------------------------------
 stale artifacts
----------------------------------------------------------------------*)

let remove_stale_artifact : t -> Path.t -> unit Deferred.t =
  fun t path ->
    let path_string = Path.to_string path in
    try_with (fun () -> Sys.remove path_string) >>| function
    | Error _ -> ()
    | Ok () ->
      if Config.show_actions_run t.config then (
        Message.message "Removed stale build artifact: %s" path_string;
      );
      Fs.clear_watcher_cache t.fs path ~needed_for_correctness:false

let blow_stale_files_away t ~dir stale_files =
  match Set.to_list stale_files with
  | [] -> return ()
  | _ :: _ as stale_files ->
    Builder.of_deferred (fun () ->
      Locking.lock_directory_for_action ~dir (fun () ->
        Deferred.List.iter stale_files ~f:(fun path ->
          assert (Path.(dirname path = dir));
          remove_stale_artifact t path
        )))

let on_filesystem t ~dir =
  (* Treat symbolic-links the same as regular files.
     To allow [source_files] to be used in user-rule for [artifacts].
     Means symbolic-links to directories are not handled correctly.
     But matches [Fs.is_digestable] which also handles [`Link] and [`File] the same. *)
  let glob = Fs.Glob.create ~dir ~kinds:[`File;`Link] "*" in
  glob_fs_only t glob

let determine_and_remove_stale_artifacts : (
  t -> dir:Path.t -> targets:Path.Set.t
  -> unit Builder.t
) =
  fun t ~dir ~targets ->
    match Path.case dir with
    | `absolute _ -> return ()
    | `relative rel ->
      Builder.cutoff ~equal:Path.Set.equal (
        on_filesystem t ~dir
        *>>| fun files_on_disk ->
        Set.diff files_on_disk targets
      ) *>>= fun non_targets_on_disk ->
      begin
        if Set.is_empty non_targets_on_disk then return ()
        else
          t.delete_eagerly ~dir t *>>= fun delete ->
          let stale_files =
            Set.filter non_targets_on_disk ~f:(fun non_target -> delete ~non_target)
          in
          blow_stale_files_away t ~dir stale_files
      end *>>| fun () ->
      let gen_key = Gen_key.create ~dir:rel in
      let prev_targets =
        match (Hashtbl.find (generated t) gen_key) with
        | Some x -> x
        | None -> Path.Set.empty
      in
      if not (Path.Set.equal prev_targets targets) then (
        Hashtbl.set (Persist.modify "generated" (generated t)) ~key:gen_key ~data:targets;
      )
;;

let share_builder : (
  name:string ->
  key_to_string : ('a -> string) ->
  memo : ('a, 'b Builder.t) Hashtbl.t ->
  f : (unit -> 'b Builder.t) ->
  key : 'a ->
  'b Builder.t
) =
  fun ~name ~key_to_string ~memo ~f ~key ->
    match (Hashtbl.find memo key) with
    | Some builder -> builder
    | None ->
      let builder =
        Builder.reify (f ())
          ~name:(lazy (sprintf "share-builder %S: %s" name (key_to_string key)))
      in
      Hashtbl.add_exn memo ~key ~data:builder;
      builder

(*----------------------------------------------------------------------
 glob - new semantics (on_filesystem or buildable)
----------------------------------------------------------------------*)

let buildable_targets : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  fun t ~dir -> t.buildable_targets t ~dir

let merge_fs_and_buildable t ~dir ~filesystem ~buildable =
  let result_before_deletion = Path.Set.union buildable filesystem in
  let files_on_disk_matching_glob_but_not_buildable =
    Set.diff filesystem buildable
  in
  if Set.is_empty files_on_disk_matching_glob_but_not_buildable
  then return result_before_deletion
  else
    t.delete_if_depended_upon t
    *>>= fun delete ->
    let stale_files =
      Set.filter files_on_disk_matching_glob_but_not_buildable
        ~f:(fun non_target -> delete ~non_target)
    in
    blow_stale_files_away t ~dir stale_files
    *>>| fun () ->
    Set.diff result_before_deletion stale_files
;;

let glob_fs_or_buildable =
  let memo = Glob.Table.create() in
  fun t glob ->
    share_builder ~key:glob
      ~name:"glob_fs_or_buildable"
      ~key_to_string:Glob.to_string
      ~memo ~f:(fun () ->
      Builder.cutoff
        ~equal:Path.Set.equal
        (let dir = Glob.dir glob in
         begin
          if Glob.kind_allows_file glob
          then
            let pattern = Glob.pattern glob in
            buildable_targets t ~dir *>>| fun targets ->
            let filtered_targets =
              Path.Set.filter targets ~f:(fun path ->
                Pattern.matches pattern (Path.basename path))
            in
            filtered_targets
          else
            return Path.Set.empty
        end
        *>>= fun filtered_targets ->
        glob_fs_only t glob
        *>>= fun filesystem ->
        merge_fs_and_buildable t ~dir ~filesystem ~buildable:filtered_targets
        ))

let scheme_glob_fs_or_buildable t glob ~buildable =
  Builder.cutoff
    ~equal:(List.equal ~equal:Path.equal)
    (glob_fs_only t glob
     *>>= fun filesystem ->
     merge_fs_and_buildable t ~dir:(Glob.dir glob) ~filesystem ~buildable
     *>>| fun set ->
     Set.to_list set)
;;

(*----------------------------------------------------------------------
  running actions
----------------------------------------------------------------------*)

let run_action_with_message :
  (t -> Env.t -> Action.t -> message:(unit->unit) ->
   deps:Proxy_map.t -> targets:Path.Rel.t list ->
   need:string -> output:'a Job.Output.t -> 'a Builder.t) =
  fun t env action ~message ~deps ~targets ~need ~output ->
    let action =
      Action_sandbox.maybe_sandbox ~sandbox:t.config.sandbox_actions
        action ~deps ~targets
    in
    let progress = t.progress in
    let putenv = Env.putenv env in
    let dir = Job.dir (Action.job action) in
    Builder.of_deferred (fun () ->
      (* We lock actions w.r.t targets to guard against overlapping the execution of
         external processes which write to the same target.

         Currently the call to [Locking.lock_directory_for_action] occurs within calls
         to [Builder.uncancellable]. This is undesirable because we should like to
         respond to the cancel until the last moment before the external process is run.

         However, this is currently a non issue, because the enclosing code structure
         has invariants which ensure we never attempt to even acquire a target lock
         until after the previous acquisition has been released:

         1. Reified tenacious computations are memoized per-goal & per-rule-head-target.

         2. The LHS of a [Tenacious.bind] wont be re-run while the RHS is still running.

         3. Actions are embedded within uncancellable tenacious computations. This would
         be so even without the use of [Builder.uncancellable] since a [Tenacious.lift]
         used to embed the deferred action computation would also be uncancellable.

         4. A tenacious computation sequenced to follow an uncancellable tenacious
         computation [t] may not start until [t] has finished, even if [t] is cancelled.

      *)
      Locking.lock_directory_for_action ~dir (fun () ->
        Fs.lock_targets_and_mask_updates t.fs ~targets (fun () ->
          Action.run action ~message ~output ~putenv ~progress ~need
        ))) *>>= function
    | Ok x                                -> return x
    | Error (`command_failed output)     -> error (Reason.Command_failed output)
    | Error (`other_error Job.Shutdown)   -> error Reason.Shutdown
    | Error (`other_error exn)            -> error (Reason.Running_job_raised (sexp_of_exn exn))

let run_action_for_targets :
    (t -> RR.t -> Env.t -> Action.t ->
     deps:Proxy_map.t ->
     targets:Path.Rel.t list ->
     need:string ->
     unit Builder.t) =
  fun t rr env action ~deps ~targets ~need ->
    let message() =
      if Config.show_actions_run t.config then
        Message.message "Building: %s [%s]"
          (String.concat ~sep:" " (List.map targets ~f:Path.Rel.to_string))
          (RR.to_string t.config rr)
    in
    run_action_with_message t env action ~message ~deps ~targets ~need
      ~output:Job.Output.ignore

let run_action_for_stdout t rr env ~deps ~need action =
  let job = Action.job action in
  let message() =
    if Config.show_actions_run t.config then
      Message.message "Action(%s): %s [%s]"
        (Path.to_string (Job.dir job))
        (Job.to_sh_ignoring_dir job)
        (RR.to_string t.config rr)
  in
  run_action_with_message t env action ~message ~deps ~targets:[] ~need
    ~output:Job.Output.stdout

(*----------------------------------------------------------------------
 report errors / record status
----------------------------------------------------------------------*)

let report_error_for_need t need reason =
  let show_now =
    match reason with
    | Reason.Command_failed _ (* we see the error message from the command *)
    | Reason.Shutdown
        -> false
    | _
      -> true
  in
  if show_now then (
    Reason.messages ~need:(Goal.to_string need) reason);
  if Config.stop_on_first_error t.config && not (Reason.filesystem_related reason) then (
    Quit.quit Exit_code.build_failed;
  )


let report_status t need ore =
  match ore with
  | Ok _ ->
    Progress.set_status_built t.progress need
  | Error problem -> (
      let reasons = Problem.reasons_here problem in
    List.iter reasons ~f:(fun reason ->
      report_error_for_need t need reason
      );
    Progress.set_status_error t.progress need reasons
    )

let build_considering_needed : (t -> Goal.t -> 'a Builder.t -> 'a Builder.t) =
  (* Expose the builder's result/error for reporting *)
  fun t need builder ->
    Builder.subgoal need (
      (* Report considering/re-considering; count as: check/recheck *)
      Builder.bracket builder
        ~running:(fun i ->
          Progress.set_status_todo t.progress need;
          let first_time = i = 0 in
          if Config.show_considering t.config
          || (not first_time && Config.show_reconsidering t.config)
          then (
            Message.message !"%s: %{Goal}"
              (if first_time then "Considering" else "Re-considering") need
          ))
        ~finished:(fun a ->
          Metrics.Counter.incr Progress.considerations_run;
          report_status t need a)
        ~cancelled:(fun () ->
          Progress.clear_status t.progress need;
          if Config.show_considering t.config
          then (
            Message.message !"Not considering anymore: %{Goal}" need
          ))
    )

(*----------------------------------------------------------------------
 expand_dollar_vars
----------------------------------------------------------------------*)

let expand_dollar_vars_between ~left ~right ~lookup orig =
  match String.lsplit2 orig ~on:'$' with
  | None -> orig (* no dollars, do nothing *)
  | Some (before_first_dollar, after_first_dollar) ->
    let translate after_dollar =
      match (
        match String.chop_prefix after_dollar ~prefix:(String.make 1 left) with
        | None -> None
        | Some after_lp ->
          match String.lsplit2 after_lp ~on:right with
          | None -> None
          | Some (var_name, after_rp) ->
            match (lookup var_name) with
            | None -> None
            | Some x -> Some (x, after_rp)
      ) with
      | None -> "$" ^ after_dollar (* cant translate - leave the string as it is*)
      | Some (expansion, after_rp) -> expansion ^ after_rp
    in
    let rec loop acc = function
      | [] -> assert false
      | [last] -> String.concat (List.rev (translate last::acc))
      | after_dollar::xs -> loop (translate after_dollar :: acc) xs
    in
    loop [before_first_dollar] (String.split after_first_dollar ~on:'$')

let expand_dollar_vars ~lookup s =
  (*let s = expand_dollar_vars_between ~left:'(' ~right:')' ~lookup s in*)
  let s = expand_dollar_vars_between ~left:'{' ~right:'}' ~lookup s in
  s

let expand_module_string_dollar_vars =
  (* replace ${jenga} in jenga.conf module strings with the root path of the jenga
     distribution, determined via the dirname of the running jenga.exe ($0) after
     stripping any "bin/" suffix *)
  let bin_directory = Filename.dirname Sys.executable_name in
  let root_distribution =
    match Filename.basename bin_directory with
    | "bin" -> Filename.dirname bin_directory
    | _ -> bin_directory
  in
  expand_dollar_vars ~lookup:(function
  | "jenga" -> Some root_distribution
  | _ -> None)

(*----------------------------------------------------------------------
  jenga.conf
----------------------------------------------------------------------*)

let errors_for_omake_server ~within errs short =
  Message.errors_for_omake_server within errs;
  error (Reason.Misc (sprintf "%s: %s" (Path.to_string within) short))

let one_error_for_omake_server ~within ?extra short =
  errors_for_omake_server ~within [Message.Err.create ?extra short] short

module Jenga_conf_rep : sig
  type t [@@deriving of_sexp]
  val modules : t -> string list
end = struct
  type t = [`modules of string list] [@@deriving sexp]
  let modules (`modules xs) = xs
end

let read_jenga_conf : (t -> conf:Path.t -> Jenga_conf_rep.t Builder.t) =
  fun t ~conf:path ->
    get_contents t path
    *>>= fun contents ->
    match Sexp.of_string_conv_exn (String.rstrip contents)
            [%of_sexp: Jenga_conf_rep.t]
    with
    | exception exn ->
      one_error_for_omake_server ~within:path
        ~extra:(Exn.to_string exn) "failed sexp conversion"
    | x -> return x
;;

let path_exists : (t -> Path.t -> bool Builder.t) =
  fun t path ->
    let glob = Glob.create_from_path ~kinds:None path in
    glob_fs_only t glob *>>| fun paths ->
    not (Path.Set.is_empty paths)

let check_path_is_digestable t path =
  let err prob =
    let short = sprintf "%s: %s" (Path.to_string path) prob in
    Some (Message.Err.create short)
  in
  digest_path t path *>>| function
  | `missing -> err "unreadable/missing file"
  | `is_a_dir -> err "is-a-directory"
  | `file __ignored_digest -> None

let jenga_conf_load_spec : (t -> conf:Path.t -> Load_root.Spec.t Builder.t) =
  fun t ~conf ->
    read_jenga_conf t ~conf *>>= fun jc ->
    let module_strings = Jenga_conf_rep.modules jc in
    let module_strings = List.map module_strings ~f:expand_module_string_dollar_vars in
    let badly_suffixed_modules =
      List.filter_map module_strings ~f:(fun m ->
        match (String.rsplit2 ~on:'.' m) with
        | None -> None (* .ml will be added *)
        | Some (_,"ml") -> None
        | Some (_,suf) ->
          let short = sprintf "%s: module with unexpected suffix: .%s" m suf in
          Some (Message.Err.create short))
    in
    match badly_suffixed_modules with
    | _::_ as errs -> errors_for_omake_server ~within:conf errs "badly suffixed modules"
    | [] ->
      let modules = (* path, but no suffix *)
        List.map module_strings ~f:(fun s ->
          match String.rsplit2 ~on:'.' s with | Some (s,_) -> s | None -> s)
      in
      let basenames =
        List.map modules ~f:(fun s ->
          match String.rsplit2 ~on:'/' s with | Some (_,s) -> s | None -> s)
      in
      (* duplicates checked w.r..t basenames *)
      match List.find_a_dup basenames with | Some dup ->
        one_error_for_omake_server ~within:conf (sprintf "duplicate module name: %s" dup)
      | None ->
        (* .ml/.mli paths re-constructed w.r.t path of config file *)
        let mls,mlis =
          let re_suffix suf =
            List.map modules ~f:(fun m ->
              Path.relative_or_absolute ~dir:(Path.dirname conf) (m ^ suf))
          in re_suffix ".ml", re_suffix ".mli"
        in
        match mls with
        | [] -> one_error_for_omake_server ~within:conf "no modules configured"
        | _::_  ->
          begin
            Builder.all (
              List.map mlis ~f:(fun mli ->
                path_exists t mli *>>| function
                | true -> Some mli
              | false -> None
              )) *>>| List.filter_map ~f:Fn.id
          end
          *>>= fun mlis_which_exist -> (* retriggering when come/go *)
          Builder.all (
            List.map (mls @ mlis_which_exist) ~f:(check_path_is_digestable t)
          ) *>>= fun results ->
          match (List.filter_map results ~f:Fn.id) with
          | _::_ as errs -> errors_for_omake_server ~within:conf errs "unreadable modules"
          | [] ->
            (* Ocaml_plugin is called to load a list of .mls;
               Corresponding .mlis will also be loaded if they exist *)
            return (Load_root.Spec.config_file ~conf ~mls)

let jenga_root_load_spec : (t -> root_ml:Path.t -> Load_root.Spec.t Builder.t) =
  fun t ~root_ml ->
    check_path_is_digestable t root_ml *>>= function
    (* The check also ensures polling jenga retriggers on changes *)
    | Some err -> errors_for_omake_server ~within:root_ml [err] "unreadable"
    | None -> return (Load_root.Spec.ml_file ~ml:root_ml)

let jenga_load_spec : (t -> Jr_spec.t -> Env.t Builder.t) =
  fun t jr_spec ->
    let env_of_load_root_spec spec =
      Builder.of_deferred (fun () -> Load_root.get_env spec) *>>= function
      | Error e ->
        error (Reason.Jengaroot_load_failed e)
      | Ok env ->
        return env
    in
    match jr_spec with
    | Env env ->
      return (env ())
    | Jr_spec.Path path ->
      let is_ml =
        match String.lsplit2 ~on:'.' (Path.basename path) with
        | Some (_,"ml") -> true
        | Some _ | None -> false
      in
      (if is_ml
      then jenga_root_load_spec t ~root_ml:path
      else jenga_conf_load_spec t ~conf:path)
      *>>= env_of_load_root_spec
    | Jr_spec.In_root_dir ->
      (let conf = Path.of_relative (Special_paths.jenga_conf) in
      let root_ml = Path.of_relative (Special_paths.jenga_root) in
      let what_kind_of_jenga_setup =
        Builder.both (path_exists t conf) (path_exists t root_ml)
        *>>| function
        | (true,true)  -> `both_conf_and_root
        | (true,false) -> `just_new_style_conf
        | (false,true) -> `just_old_style_root
        | (false,false)-> `no_conf_or_root
      in
      what_kind_of_jenga_setup *>>= function
      | `no_conf_or_root ->
        one_error_for_omake_server ~within:conf "jenga.conf missing"
      | `just_old_style_root ->
        jenga_root_load_spec t ~root_ml
      | `just_new_style_conf
      | `both_conf_and_root ->
        jenga_conf_load_spec t ~conf)
        *>>= env_of_load_root_spec

(*----------------------------------------------------------------------
  jenga_root
----------------------------------------------------------------------*)

let jenga_root : (t -> Env.t Builder.t) =
  (* wrap up the call to [Load_root.get_env] into a tenacious builder,
     which will reload any time the jengaroot is modified *)
  fun t ->
    Builder.of_tenacious Var.change_to_peeked_var *>>= fun () ->
    jenga_load_spec t t.jr_spec

let jenga_root : (t -> Env.t Builder.t) =
  (* Memoization of jenga_root is simpler that other cases, because:
     - There is only one; we don't need a hashtable, just a ref.
     - The root has no deps, so there is no chance of cycles *)
  let memo = ref None in
  fun t ->
    memo_builder ~memo (fun () ->
      jenga_root t
    )

let ensure_jenga_root t = (* for use in [build_goal] to avoid a cycle. *)
  jenga_root t *>>| fun _ -> ()

let jenga_root : (t -> Env.t Builder.t) =
  let memo = ref None in
  fun t ->
    memo_builder ~memo (fun () ->
      build_sub_goal t Goal.Jengaroot *>>= fun (_ignored : Proxy_map.t) ->
      jenga_root t
    )

(*----------------------------------------------------------------------
 Mtimes - to check mtimes are unchanged for deps while an action is running
----------------------------------------------------------------------*)

module Mtimes : sig

  type t
  val create : (Path.t * Fs.Mtime.t) list -> t
  val keys : t -> Path.t list
  val diff : before:t -> after:t -> Path.t list option

end = struct

  type t = (Path.t * Fs.Mtime.t) list [@@deriving hash, compare]
  let create t = t

  let keys t = List.map t ~f:fst

  let equal = equal_using_compare compare

  let diff ~before ~after =
    (* expect to only be called on [Mtimes.t] values with the same path-keys *)
    if equal before after then None
    else begin
      let r = ref [] in
      List.iter2_exn before after ~f:(fun (path1, mtime1) (path2, mtime2) ->
        assert (Path.equal path1 path2);
        if not (Fs.Mtime.equal mtime1 mtime2)
        then r := path1 :: !r);
      Some !r
    end

end

let mtime_file_cached t path =
  Builder.wrap (
    Tenacious.map (Fs.mtime_file t.fs ~file:path)
      ~f:(function
        | Some mtime -> Ok (path, mtime)
        | None ->
          let msg = sprintf !"file disappeared for mtime: %{Path}" path in
          Error (Problem.create (Reason.Misc msg))))
;;

let mtimes_of_proxy_map =
  let type_id : (Path.t * Fs.Mtime.t) list Builder.t Type_equal.Id.t =
    Type_equal.Id.create ~name:"" [%sexp_of: _]
  in
  let compute_without_reify t paths =
    Builder.all (List.map paths ~f:(mtime_file_cached t))
  in
  let compute_with_reify t paths =
    Builder.reify
      ~name:(lazy "mtimes_of_proxy_map")
      (compute_without_reify t paths)
  in
  fun t proxy_map ->
    if Jenga_options.t.turn_off_mtimes_check
    then return (Mtimes.create [])
    else
      let paths, groups = Db.Proxy_map.to_paths_for_mtimes_check proxy_map in
      let all =
        compute_without_reify t paths ::
        List.map groups ~f:(fun group ->
          Db.Proxy_map.Group.find_or_add group
            ~unique_id_across_jenga:type_id
            (* Here we can have different [t]. However the function only cares about
               t.fs, and there is only one of those. *)
            ~unique_f_across_jenga:(compute_with_reify t))
      in
      Builder.all all
      *>>| fun l ->
      Mtimes.create (List.concat l)
;;

let mtimes_of_paths_right_now paths =
  Builder.of_deferred (fun () -> Fs.mtime_files_right_now paths)
  *>>= function
  | Ok mtimes -> return (Mtimes.create mtimes)
  | Error file -> error (Reason.Misc ("file disappeared for mtime: " ^ file))
;;

let check_mtimes_unchanged : (t -> Mtimes.t -> unit Builder.t) =
  fun t mtimes_before ->
    match Mtimes.keys mtimes_before with
    | [] -> return ()
    | _ :: _ as paths ->
      mtimes_of_paths_right_now paths
      *>>|= fun mtimes_now ->
      match Mtimes.diff ~before:mtimes_before ~after:mtimes_now with
      | None -> Ok ()
      | Some paths ->
        (* clearing the watcher cache allows the build to retrigger if running
           without notifiers, or we lost an inotify event *)
        List.iter paths ~f:(Fs.clear_watcher_cache t.fs ~needed_for_correctness:false);
        Error (Reason.Mtimes_changed paths)

(*----------------------------------------------------------------------
 things we can depend upon
----------------------------------------------------------------------*)

let run_action_for_stdout_if_necessary t ~deps ~need action =
  jenga_root t *>>= fun env ->
  let job = Action.job action in
  let run_and_cache rr =
    mtimes_of_proxy_map t deps *>>= fun mtimes ->
    Builder.uncancellable (
      run_action_for_stdout t rr env action ~deps ~need *>>= fun stdout ->
      check_mtimes_unchanged t mtimes *>>= fun () ->
      let output_proxy = {Output_proxy. deps; stdout;} in
      Hashtbl.set (Persist.modify "actioned" (actioned t)) ~key:job ~data:output_proxy;
      return stdout
    )
  in
  match (Hashtbl.find (actioned t) job) with
  | None -> run_and_cache RR.No_record_of_being_run_before
  | Some prev ->
    match (Proxy_map_op.diff ~before:prev.Output_proxy.deps ~after:deps) with
    | Some keys -> run_and_cache (RR.Deps_have_changed keys)
    | None ->
      (* Nothing has changed, use cached stdout *)
      return prev.Output_proxy.stdout


let need_abs_path : (t -> Path.Abs.t -> Proxy_map.t Builder.t) =
  (* For dependencies on absolute paths (extenal to the repo):
     make no attempt to build them; (there can be no rule!)
     just digest & return the proxy.
  *)
  fun t abs ->
    let path = Path.of_absolute abs in
    digest_path t path *>>= fun res ->
    match res with
    | `missing ->  error (Reason.No_source_at_abs_path abs)
    | `is_a_dir -> error (Reason.Unexpected_directory path)
    | `file digest ->
      let proxy = Proxy.of_digest digest in
      let pm = Proxy_map_op.single (Pm_key.of_abs_path abs) proxy in
      return pm

let need_path : (t -> Path.t -> Proxy_map.t Builder.t) =
  fun t path ->
    match Path.case path with
    | `relative path -> build_sub_goal t (Goal.Path path)
    | `absolute path -> need_abs_path t path

let build_merged_proxy_maps : (Proxy_map.t list -> Proxy_map.t Builder.t) =
  fun pms ->
    match (Proxy_map_op.merge pms) with
    | Error _inconsistency -> error Reason.Inconsistent_proxies
    | Ok pm -> return pm

(*----------------------------------------------------------------------
 source_files
----------------------------------------------------------------------*)

let source_files : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  (* Set of files (per directory) [on_filesystem] \\ [buildable_targets].
     Likely to be useful for the user-provided [artifacts] definition. *)
  let memo = Path.Table.create() in
  fun t ~dir ->
    share_builder ~memo ~key:dir
      ~name:"source files: "
      ~key_to_string:Path.to_string
      ~f:(fun () ->
      (* We use [cutoff] to avoid triggering dependents as buildable files are generated
         for the first time & appear on the filesystem. *)
        Builder.cutoff ~equal:(fun set1 set2 -> Path.Set.equal set1 set2)
          (Builder.both (on_filesystem t ~dir) (t.buildable_targets t ~dir)
           *>>| fun (filesystem,buildable) ->
           Path.Set.diff filesystem buildable))

(*----------------------------------------------------------------------
 build_depends
----------------------------------------------------------------------*)

let build_depends t depends ~need =
  let rec exec : type a. a Dep.t -> (a * Proxy_map.t) Builder.t = fun dep ->
    match dep with

    | Dep.Return x ->
      return (x, Proxy_map_op.empty)

    | Dep.Map (x, f) ->
      exec x *>>|= fun (v1, pm) ->
      apply_user_function' (fun () -> (f v1, pm))

    | Dep.Bind (left,f_right) ->
      exec left *>>= fun (v1,pm1) ->
      apply_user_function (fun () -> f_right v1) *>>= fun right ->
      exec right *>>= fun (v2,pm2) ->
      build_merged_proxy_maps [pm1;pm2] *>>= fun pm ->
      return (v2, pm)

    | Dep.Cutoff (equal,body) ->
      Builder.cutoff
        ~equal:(fun (x1,pm1) (x2,pm2) -> equal x1 x2 && Proxy_map_op.equal pm1 pm2)
        (exec body)

    | Dep.All xs ->
      Builder.all (List.map ~f:exec xs) *>>= fun xs ->
      let vs,pms = List.unzip xs in
      build_merged_proxy_maps pms *>>= fun pm ->
      return (vs,pm)

    | Dep.Deferred f ->
      run_user_async_code f *>>| fun v ->
      (v, Proxy_map.empty)

    | Dep.Alias alias ->
      build_sub_goal t (Goal.Alias alias) *>>| fun pm -> ((), pm)

    | Dep.Path path ->
      need_path t path *>>| fun pm ->
      ((), pm)

    | Dep.Source_if_it_exists path ->
      look_for_source t path *>>| fun proxy_opt ->
      let pm =
        match proxy_opt with
        | None -> Proxy_map.empty
        | Some proxy -> Proxy_map.single (Pm_key.of_path path) proxy
      in
      ((),pm)

    | Dep.Contents path ->
      begin
        match Path.case path with
        | `relative path ->
          build_sub_goal t (Goal.Path path) *>>| fun _ignored_pm -> ()
        | `absolute _ -> return ()
      end *>>= fun () ->
      let pm  = Proxy_map.empty in
      get_contents t path *>>| fun contents ->
      (contents, pm)

    | Dep.Action_stdout action_depends ->
      exec action_depends *>>= fun (action,pm) ->
      run_action_for_stdout_if_necessary t ~deps:pm ~need action *>>| fun stdout ->
      (stdout, Proxy_map.empty)

    | Dep.Reflect_path path ->
      t.reflect_path t path *>>| fun x ->
      (x,Proxy_map.empty)

    | Dep.Reflect_alias alias ->
      t.reflect_alias t alias *>>| fun x ->
      (x,Proxy_map.empty)

    | Dep.Reflect_putenv ->
      jenga_root t *>>| fun env ->
      (Env.putenv env, Proxy_map.empty)

    | Dep.Buildable_targets dir ->
      t.buildable_targets t ~dir *>>| fun targets ->
      (targets, Proxy_map.empty)

    | Dep.Source_files dir ->
      source_files t ~dir *>>| fun paths ->
      (paths, Proxy_map.empty)

    | Dep.Glob_listing_OLD glob ->
      (* CARRY the glob listing; dont put into the proxy-map *)
      glob_fs_only t glob *>>| fun paths ->
      (paths, Proxy_map.empty)

    | Dep.Glob_listing glob ->
      (* CARRY the glob listing; dont put into the proxy-map *)
      glob_fs_or_buildable t glob *>>| fun paths ->
      (paths, Proxy_map.empty)

    | Dep.Glob_change_OLD glob ->
      (* Dont carry the glob listing; do put into the PROXY-MAP *)
      glob_fs_only t glob *>>| fun paths ->
      let dir = Glob.dir glob in
      let pm = Proxy_map.single (Pm_key.of_glob glob) (Proxy.of_listing ~dir paths) in
      ((), pm)

    | Dep.Glob_change glob ->
      (* Dont carry the glob listing; do put into the PROXY-MAP *)
      glob_fs_or_buildable t glob *>>| fun paths ->
      let dir = Glob.dir glob in
      let pm = Proxy_map.single (Pm_key.of_glob glob) (Proxy.of_listing ~dir paths) in
      ((), pm)

    | Dep.Group_dependencies dep ->
      exec dep *>>| fun (v, pm) ->
      v, Db.Proxy_map.group pm

    | Dep.Var var ->
      Builder.of_tenacious (Var.watch var) *>>|= fun result ->
      apply_user_function' (fun () -> ok_exn result, Proxy_map.empty)

  in
  exec depends

(*----------------------------------------------------------------------
 reflect depends
----------------------------------------------------------------------*)

let reflect_depends t depends ~need =
  let rec exec : type a. a Dep.t -> (a * Path.Set.t) Builder.t = function

    | Dep.Return x ->
      return (x, Path.Set.empty)

    | Dep.Map (x, f) ->
      exec x *>>|= fun (v1, set) ->
      apply_user_function' (fun () -> (f v1, set))

    | Dep.Bind (left,f_right) ->
      exec left *>>= fun (v1,set1) ->
      apply_user_function (fun () -> f_right v1) *>>= fun right ->
      exec right *>>= fun (v2,set2) ->
      let set = Path.Set.union set1 set2 in
      return (v2, set)

    | Dep.Cutoff (equal,body) ->
      Builder.cutoff
        ~equal:(fun (x1,set1) (x2,set2) -> equal x1 x2 && Path.Set.equal set1 set2)
        (exec body)

    | Dep.All xs ->
      Builder.all (List.map ~f:exec xs) *>>= fun xs ->
      let vs,sets = List.unzip xs in
      let set = List.fold sets ~init:Path.Set.empty ~f:Path.Set.union in
      return (vs,set)

    | Dep.Deferred f ->
      run_user_async_code f *>>| fun v ->
      (v, Path.Set.empty)

    | Dep.Alias alias ->
      t.reflect_alias t alias *>>| fun set ->
      ((),set)

    | Dep.Path path ->
      return ((), Path.Set.singleton path)

    | Dep.Source_if_it_exists path ->
      look_for_source t path *>>| fun proxy_opt ->
      let set =
        match proxy_opt with
        | None -> Path.Set.empty
        | Some __proxy -> Path.Set.singleton path
      in
      ((),set)

    | Dep.Contents path ->
      (* reflect causes building here by calling [build_sub_goal] *)
      begin
        match Path.case path with
        | `relative path ->
          build_sub_goal t (Goal.Path path) *>>| fun __pm -> ()
        | `absolute _ -> return ()
      end *>>= fun () ->
      get_contents t path *>>| fun contents ->
      (contents, Path.Set.empty)

    | Dep.Action_stdout action_depends ->
      (* reflect causes building here by calling [build_depends] *)
      build_depends t action_depends ~need *>>= fun (action,deps_proxy) ->
      run_action_for_stdout_if_necessary t ~deps:deps_proxy action ~need *>>| fun stdout ->
      (stdout, Path.Set.empty)

    | Dep.Reflect_path path ->
      t.reflect_path t path *>>| fun x ->
      (x,Path.Set.empty)

    | Dep.Reflect_alias alias ->
      t.reflect_alias t alias *>>| fun x ->
      (x,Path.Set.empty)

    | Dep.Reflect_putenv ->
      jenga_root t *>>| fun env ->
      (Env.putenv env, Path.Set.empty)

    | Dep.Buildable_targets dir ->
      t.buildable_targets t ~dir *>>| fun targets ->
      (targets, Path.Set.empty)

    | Dep.Source_files dir ->
      source_files t ~dir *>>| fun paths ->
      (paths, Path.Set.empty)

    | Dep.Glob_listing_OLD glob ->
      glob_fs_only t glob *>>| fun paths ->
      (paths, Path.Set.empty)

    | Dep.Glob_listing glob ->
      glob_fs_or_buildable t glob *>>| fun paths ->
      (paths, Path.Set.empty)

    | Dep.Glob_change_OLD _ ->
      return ((), Path.Set.empty)

    | Dep.Glob_change _ ->
      return ((), Path.Set.empty)

    | Dep.Group_dependencies dep ->
      (* We should probably do something smarter here, but reflection doesn't matter as
         much, since it's not usually done on whole trees. *)
      exec dep

    | Dep.Var var ->
      Builder.of_tenacious (Var.watch var) *>>|= fun result ->
      apply_user_function' (fun () -> ok_exn result, Path.Set.empty)

  in
  exec depends

(*----------------------------------------------------------------------
schemes...
----------------------------------------------------------------------*)

let get_scheme : (t -> dir:Path.t -> Scheme.t Builder.t) =
  let memo = Path.Table.create() in
  fun t ~dir ->
    share_builder ~memo ~key:dir
      ~name:"get_scheme"
      ~key_to_string:Path.to_string
      ~f:(fun () ->
        jenga_root t *>>= fun env ->
        apply_user_function (fun () ->
          Env.get_scheme env ~dir
        )
      )

let validate_scheme_glob ~dir glob =
  let glob_dir = Glob.dir glob in
  if Path.equal glob_dir dir
  then Ok ()
  else
    Error
      (Reason.Misc
         (sprintf !"Scheme.glob can only be given a glob in the same directory \
                    (got %{Path} instead of %{Path})" glob_dir dir))
;;

let scheme_glob_matches ~validated_glob:glob path =
  Pattern.matches (Glob.pattern glob) (Path.Rel.basename path)
;;

let check_for_non_local_targets ~dir ruleset =
  match Path.case dir with
  | `absolute _ -> Ok ()
  | `relative dir ->
    match Ruleset.check_for_non_local_targets ruleset ~dir with
    | [] -> Ok ()
    | _ :: _ as l ->
      let msg =
        sprintf !"non-local targets in scheme of %{Path.Rel}: %s" dir
          (String.concat ~sep:", "
             (List.map l ~f:(function
                | `Sources p | `Rule_target p -> Path.Rel.to_string p
                | `Alias a -> Alias.to_string a)))
      in
      Error (Reason.Misc msg)
;;

let check_for_targets_matching_previous_glob ~validated_globs ruleset =
  match validated_globs with
  | [] -> Ok () (* no need to iterate over the targets *)
  | _ :: _ ->
    match
      List.find_map (Ruleset.targets ruleset) ~f:(fun path ->
        List.find validated_globs ~f:(fun validated_glob ->
          scheme_glob_matches ~validated_glob path))
    with
    | None -> Ok ()
    | Some glob ->
      let msg =
        sprintf !"target must be declared in the scheme before %{Glob}" glob
      in
      Error (Reason.Misc msg)
;;

module Scheme_stream = struct

  (* The ruleset in Final and Finished is the union of all the previous ones in the
     scheme. *)
  type state =
    | Init
    | Unfinished of Ruleset.t * state Builder.t
    | Finished of (Ruleset.t, Reason.t) Result.t

  type item =
    | Initial
    | Rules of Ruleset.t
    | Final of Ruleset.t

  let end_of_stream =
    Builder.expose
      (return
         (Finished (Error (Reason.Misc "jenga bug: not supposed to ask for data past the \
                                        end of a scheme stream"))))

  let stream t ~dir =
    (* [globs] is the list of [Scheme.glob]s that we have seen from the beginning of the
       stream, for checking that we don't declare targets matching them.
       [pr] is a [Ruleset.t] accumulated from the beginning of the scheme to the last
       returned value. It is what the stream query will receive in the Final constructor.
       [acc] is the [Ruleset.t] that we are building at this iteration in the stream,
       which is what the stream query will receive in the Rules constructor. *)
    let rec loop globs pr (acc : Ruleset.t) (scheme : Scheme.t) k : state =
      match scheme with
      | Rules ruleset ->
        begin match ruleset with
        | `Duplicate_target rel -> Finished (Error (Reason.Multiple_rules_for_path rel))
        | `Ok ruleset ->
          match check_for_non_local_targets ~dir ruleset with
          | Error reason -> Finished (Error reason)
          | Ok () ->
            match
              check_for_targets_matching_previous_glob ~validated_globs:globs ruleset
            with
            | Error reason -> Finished (Error reason)
            | Ok () ->
              match Ruleset.union ~into:acc ruleset with
              | `Duplicate_target rel -> Finished (Error (Reason.Multiple_rules_for_path rel))
              | `Ok acc -> k globs pr acc
        end
      | All schemes ->
        let rec inner_loop globs pr acc schemes k =
          match schemes with
          | [] -> k globs pr acc
          | scheme :: schemes ->
            loop globs pr acc scheme (fun globs pr acc -> inner_loop globs pr acc schemes k)
        in
        inner_loop globs pr acc schemes k
      | Glob (glob, f) ->
        begin match Ruleset.union ~into:pr acc with
        | `Duplicate_target rel -> Finished (Error (Reason.Multiple_rules_for_path rel))
        | `Ok pr ->
          match validate_scheme_glob ~dir glob with
          | Error reason -> Finished (Error reason)
          | Ok () ->
            let dep =
              let targets =
                Ruleset.targets pr
                |> List.filter ~f:(fun p -> scheme_glob_matches ~validated_glob:glob p)
                |> List.map ~f:Path.of_relative
                |> Path.Set.of_list
              in
              scheme_glob_fs_or_buildable t glob ~buildable:targets
              *>>|= fun scheme ->
              match apply_user_function' (fun () -> f scheme) with
              | Error _ as e -> e
              | Ok scheme -> Ok (loop (glob :: globs) pr Ruleset.empty scheme k)
            in
            Unfinished (acc, dep)
        end
      | Dep scheme_dep ->
        begin match Ruleset.union ~into:pr acc with
        | `Duplicate_target rel -> Finished (Error (Reason.Multiple_rules_for_path rel))
        | `Ok pr ->
          let dep =
            build_depends t scheme_dep ~need:"scheme" *>>| fun (scheme,__proxy_map) ->
            loop globs pr Ruleset.empty scheme k
          in
          Unfinished (acc, dep)
        end
    in
    let finish _globs pr acc =
      match Ruleset.union ~into:pr acc with
      | `Duplicate_target rel -> Finished (Error (Reason.Multiple_rules_for_path rel))
      | `Ok pr ->
        (* Right before finishing, clean up stale artifacts if any. *)
        Unfinished (Ruleset.empty,
                    (let targets =
                       Path.Set.of_list
                         (List.map (Ruleset.targets pr) ~f:Path.of_relative)
                     in
                     determine_and_remove_stale_artifacts t ~dir ~targets
                     *>>| fun () ->
                     Unfinished (acc, return (Finished (Ok pr)))))
    in
    let next = function
      | Ok Init ->
        let state =
          get_scheme t ~dir *>>| fun scheme ->
          loop [] Ruleset.empty Ruleset.empty scheme finish
        in
        Ok Initial, Builder.expose state
      | Ok (Unfinished (rules, state)) ->
        Ok (Rules rules), Builder.expose state
      | Ok (Finished (Ok all_rules)) ->
        Ok (Final all_rules), end_of_stream
      | Ok (Finished (Error reason)) ->
        Error (Problem.create reason), end_of_stream
      | Error _ as e -> e, end_of_stream
    in
    Tenacious.Stream.unfold (Ok Init) next
      ~name:(lazy ("scheme of " ^ Path.to_string dir))
  ;;

  let find =
    let memo = Path.Table.create () in
    fun t ~dir ->
      match Hashtbl.find memo dir with
      | Some stream -> stream
      | None ->
        let stream = stream t ~dir in
        Hashtbl.add_exn memo ~key:dir ~data:stream;
        stream

  let query t ~dir query =
    Builder.wrap (Tenacious.Stream.query (find t ~dir) query)
  ;;
;;

end

let find_target_rule : (t -> Path.Rel.t -> Target_rule.t option Builder.t) =
  fun t rel ->
    let rec next rules : _ Tenacious.Stream.query =
      match (rules : (Scheme_stream.item, _) Result.t) with
      | Error _ as e -> Return e
      | Ok Initial -> Continue next
      | Ok (Final _all_rules) -> Return (Ok None)
      | Ok (Rules rules) ->
        match Ruleset.find_target rules rel with
        | `Found opt -> Return (Ok opt)
        | `Not_found -> Continue next
    in
    Scheme_stream.query t ~dir:(Path.of_relative (Path.Rel.dirname rel))
      (Continue next)
;;

let scheme_rules : (t -> dir:Path.t -> Ruleset.t Builder.t) =
  fun t ~dir ->
    match Path.case dir with
    | `absolute _ -> return Ruleset.empty
    | `relative _rel ->
      let rec next rules : _ Tenacious.Stream.query =
        match (rules : (Scheme_stream.item, _) Result.t) with
        | Error _ as e -> Return e
        | Ok (Final all_rules) -> Return (Ok all_rules)
        | Ok Initial -> Continue next
        | Ok (Rules _) -> Continue next
      in
      Scheme_stream.query t ~dir (Continue next)
;;

let find_alias_deps : (t -> Alias.t -> unit Dep.t Builder.t) =
  fun t alias ->
    let dir = Path.of_relative (Alias.directory alias) in
    scheme_rules t ~dir *>>|= fun ruleset ->
    match Ruleset.find_alias ruleset alias with
    | None -> Error (Reason.No_definition_for_alias alias)
    | Some deps -> Ok (Dep.all_unit deps)
;;

(*----------------------------------------------------------------------
 targets
----------------------------------------------------------------------*)

let check_targets :
    (t -> Path.Rel.t list -> [ `ok of PPs.t | `missing of Path.Rel.t list] Builder.t) =
  (* Check the targets a rule claims to build.
     - this function may be called before the action is run.
     - and will definitely be called after an action successfully completes. *)
  fun t paths ->
    build_all_in_sequence paths ~f:(fun path ->
      look_for_source t (Path.of_relative path) *>>= fun res -> return (path,res)
    ) *>>= fun tagged ->
    let good,bad =
      List.partition_map tagged ~f:(function
      | (path,Some proxy) -> `Fst (Path.of_relative path, proxy)
      | (path,None) -> `Snd path
      )
    in
    match bad with
    | _::_ -> return (`missing bad)
    | [] -> return (`ok good)

let build_target_rule :
  (* run a rule/action, iff:
     - we have no record of running it before
     - one of its dependencies has changed
     - the action has changed (for an internal action; jengaroot has changed)
     - one of the targets is missing
     - one of the targets is different from expected
     Record a successful run in the persistent state.
  *)
    (t -> Target_rule.t -> PPs.t Builder.t) =
  fun t tr ->
    let targets = Target_rule.targets tr in
    (* The persistent caching is keyed of the [head_target] *)
    let head_target,other_targets = Target_rule.head_target_and_rest tr in
    let need = Path.Rel.basename head_target in
    build_depends t (Target_rule.action_depends tr) ~need:("rule of " ^ need)
    *>>= fun (action,deps_proxy_map) ->
    jenga_root t *>>= fun env ->
    let job = Action.job action in
    let run_and_cache rr =
      mtimes_of_proxy_map t deps_proxy_map *>>= fun mtimes ->
      Builder.uncancellable (
        run_action_for_targets t rr env action ~deps:deps_proxy_map ~targets ~need *>>= fun () ->
        (* The cache is cleared AFTER the action has been run, and BEFORE we check the
           targets created by the action. Clearing the cache forces the files to be
           re-stated, without relying on any events from inotify to make this happen *)
        List.iter targets ~f:(fun rel ->
          Fs.clear_watcher_cache t.fs (Path.of_relative rel) ~needed_for_correctness:true
        );
        check_targets t targets *>>= function
        | `missing paths -> error (Reason.Rule_failed_to_generate_targets paths)
        | `ok path_tagged_proxys ->
          match (Proxy_map_op.create_by_path path_tagged_proxys) with
          | Error _inconsistency -> error Reason.Inconsistent_proxies
          | Ok targets_proxy_map ->
            check_mtimes_unchanged t mtimes *>>= fun () ->
            let rule_proxy = {
              Rule_proxy.
              targets = targets_proxy_map;
              deps = deps_proxy_map;
              action = job;
            }
            in
            Hashtbl.set (Persist.modify "ruled" (ruled t)) ~key:head_target ~data:rule_proxy;
            (* We remove data associated with the [other_targets]. Its not essential for
               correctness, but it avoid cruft from building up in the persistent state *)
            List.iter other_targets ~f:(fun other -> Hashtbl.remove (ruled t) other);
            return path_tagged_proxys
      )
    in
    match (Hashtbl.find (ruled t) head_target) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some prev ->
      match (equal_using_compare Job.compare prev.Rule_proxy.action job) with
      | false -> run_and_cache RR.Action_changed
      | true ->
        match (Proxy_map_op.diff ~before:prev.Rule_proxy.deps ~after:deps_proxy_map) with
        | Some keys -> run_and_cache (RR.Deps_have_changed keys)
        | None ->
          (* de-sensitize to the pre-action state of targets...
             in case we have to run the action
          *)
          Builder.desensitize (check_targets t targets) *>>= fun (opt,heart) ->
          match opt with
          | `missing paths -> run_and_cache (RR.Targets_missing paths)
          | `ok path_tagged_proxys ->
            match (Proxy_map_op.create_by_path path_tagged_proxys) with
            | Error _inconsistency -> error Reason.Inconsistent_proxies
            | Ok targets_proxy_map ->
              match (Proxy_map_op.diff ~before:prev.Rule_proxy.targets ~after:targets_proxy_map) with
              | Some keys ->
                let paths = List.map keys ~f:Pm_key.to_path_exn in
                run_and_cache (RR.Targets_not_as_expected paths)
              | None ->
                (* Everything is as it should be! re-sensitize to the targets. *)
                if Config.show_checked t.config then  (
                  Message.message "NOT RUNNING: %s" (Job.to_sh_ignoring_dir job);
                );
                Builder.sensitize heart *>>= fun () ->
                return path_tagged_proxys

let build_target_rule :
    (t -> Path.Rel.t -> PPs.t Builder.t) =
  let memo = Path.Rel.Table.create() in
  fun t head_target ->
    share_builder
      ~memo ~key:head_target
      ~name:"build_target_rule"
      ~key_to_string:Path.Rel.to_string
      ~f:(fun () ->
      find_target_rule t head_target *>>= function
      | Some tr ->
        build_target_rule t tr
      | None ->
        error (Reason.No_rule_or_source (Path.of_relative head_target)))


let expect_source : (t -> Path.Rel.t -> Proxy_map.t Builder.t) =
  fun t demanded ->
    let path = Path.of_relative demanded in
    digest_path t path
    *>>= function
    | `missing ->  error (Reason.No_rule_or_source path)
    | `is_a_dir -> error (Reason.Unexpected_directory path)
    | `file digest ->
      let success =
        Proxy_map.single (Pm_key.of_rel_path demanded) (Proxy.of_digest digest)
      in
      t.delete_union t ~non_target:path
      *>>= function
      | false -> return success
      | true ->
        (* We self trigger a bit here, because the deletion will make [digest_path]
           above go in the missing case (and return the same error). It's not clear how
           to avoid this. *)
        blow_stale_files_away t ~dir:(Path.dirname path) (Path.Set.singleton path)
        *>>= fun () ->
        error (Reason.No_rule_or_source path)

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  (* build a goal -- alias or path
     In either case, first get the ruleset applicable to the goal.
     For an alias - there must be a rule in this ruleset.
     For a path
     - If there is a rule, use it.
     - Otherwise, that path had better exists as source. *)
  fun t goal ->
    match goal with
    | Goal.Jengaroot ->
       begin
         ensure_jenga_root t *>>| fun () ->
         Proxy_map.empty
       end
    | Goal.Alias alias ->
      begin
        find_alias_deps t alias *>>= fun depends ->
        build_depends t depends ~need:(Alias.basename alias)
        *>>| fun ((),pm) -> pm
      end
    | Goal.Path demanded ->
      begin
        find_target_rule t demanded *>>= function
        | Some tr ->
          let head_target = Target_rule.head_target tr in
          let head_build =
            build_target_rule t head_target *>>| fun tagged ->
            let (path,proxy) =
              List.find_exn tagged ~f:(fun (path,_) ->
                Path.equal path (Path.of_relative demanded)
              )
            in
            Proxy_map.single (Pm_key.of_path path) proxy
          in
          if Path.Rel.(=) demanded head_target
          then head_build
          else
            (* Going via [build_sub_goal] makes sure errors get attributed to the head
               target and also [build_considering_needed (... head_target)] is only
               called once.
               The proxy map we get from that is wrong though: it only contains an entry
               for head target. So we discard that and compute our own. *)
            build_sub_goal t (Goal.Path head_target) *>>= fun (_wrong : Proxy_map.t) ->
            head_build
        | None ->
          expect_source t demanded
      end

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  fun t goal ->
    ensure_directory t ~dir:(Goal.directory goal)
    *>>= fun () ->
    build_goal t goal

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  fun t goal ->
    share_builder
      ~key: goal
      ~name:"build_goal"
      ~key_to_string:Goal.to_string
      ~memo:(memo_building t)
      ~f: (fun () ->
        build_considering_needed t goal (
          build_goal t goal
        )
      )

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  fun t goal ->
    if Quit.is_quitting()
    then error Reason.Shutdown
    else build_goal t goal

let reflect_path : (t -> Path.t -> Reflected.Trip.t option Builder.t) =
  fun t path ->
    match Path.case path with
    | `absolute _ -> return None (* absolute path: can never be build *)
    | `relative rel ->
      let goal = Goal.Path rel in
      begin
        let need = goal in
        if Config.show_reflecting t.config then (
          Message.message "Reflecting: %s" (Goal.to_string need);
        )
      end;
      find_target_rule t rel *>>= function
      | None -> return None (* scheme has no rule, so path must be source *)
      | Some tr ->
        let targets = List.map (Target_rule.targets tr) ~f:Path.of_relative in
        reflect_depends t (Target_rule.action_depends tr)
          ~need:("rule of " ^ Path.Rel.basename rel)
        *>>| fun (action,deps) ->
        let action = Action.job action in
        let deps = Path.Set.to_list deps in
        Some { Reflected.Trip. targets; deps; action; }

let reflect_path : (t -> Path.t -> Reflected.Trip.t option Builder.t) =
  fun t path ->
    share_builder
      ~key:path
      ~name:"reflect_path"
      ~key_to_string:Path.to_string
      ~memo: (memo_reflecting t)
      ~f: (fun () -> reflect_path t path)

let reflect_alias : (t -> Alias.t -> Path.Set.t Builder.t) =
  fun t alias ->
    find_alias_deps t alias *>>= fun depends ->
    reflect_depends t depends ~need:(Alias.basename alias) *>>| fun ((),set) ->
    set

(*----------------------------------------------------------------------
 buildable_targets
----------------------------------------------------------------------*)

let buildable_targets : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  let memo = Path.Table.create() in
  fun t ~dir ->
    share_builder ~key:dir
      ~name:"buildable-targets"
      ~key_to_string:Path.to_string
      ~memo ~f:(fun () ->
        scheme_rules t ~dir *>>| fun ruleset ->
        Path.Set.of_list (List.map ~f:Path.of_relative (Ruleset.targets ruleset)))

(*----------------------------------------------------------------------
 artifacts
----------------------------------------------------------------------*)

let delete_nothing = fun ~non_target:_ -> false

let internal_artifacts t ~dir =
  (* Determine artifacts as what was previously build by jenga. Not completely reliable -
     doesn't work if jenga.db is removed - but avoids any effort from the rule-author. *)
  let gen_key = Gen_key.create ~dir in
  match Hashtbl.find (generated t) gen_key with
  | Some previous_targets -> (fun ~non_target -> Set.mem previous_targets non_target)
  | None -> delete_nothing

let delete_eagerly =
  let memo = Path.Table.create() in
  fun t ~dir ->
    let name = "delete-eagerly" in
    share_builder ~key:dir ~memo ~name
      ~key_to_string:Path.to_string
      ~f:(fun () ->
        match Path.case dir with
        | `absolute _ -> return delete_nothing
        | `relative reldir ->
          jenga_root t
          *>>= fun env ->
          match Env.delete_eagerly env with
          | Some f -> build_depends t ~need:name f *>>| fst
          | None -> return (internal_artifacts t ~dir:reldir))
;;

let delete_if_depended_upon =
  let memo = Unit.Table.create() in
  fun t ->
    let name = "delete-if-depended-upon" in
    share_builder ~key:() ~memo ~name
      ~key_to_string:Unit.to_string
      ~f:(fun () ->
        jenga_root t
        *>>= fun env ->
        build_depends ~need:name t (Env.delete_if_depended_upon env)
        *>>| fst
      )
;;

let delete_union =
  let memo = Path.Table.create() in
  fun t ~non_target ->
    let dir = Path.dirname non_target in
    share_builder ~key:dir ~memo
      ~name:"deletable-union" ~key_to_string:Path.to_string
      ~f:(fun () ->
        match Path.case dir with
        | `absolute _ -> return (delete_nothing, delete_nothing)
        | `relative _ ->
          Builder.both
            (delete_eagerly t ~dir)
            (delete_if_depended_upon t))
    *>>| fun (pred1, pred2) ->
    pred1 ~non_target || pred2 ~non_target
;;

(*----------------------------------------------------------------------
 buildable
----------------------------------------------------------------------*)

let build_one_root_goal :
    (
      jr_spec: Jr_spec.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      Config.t ->
      Progress.t ->
      demanded:Goal.t ->
      unit Builder.t
    ) =
  (* Entry point to build a single root goal
     Here the downwards "t" parameter is constructed from various components.
     And here we break out of the Builder monad, and revert to the plain
     tenacious monad - ignoring the proxy map or any errors.
  *)
  fun ~jr_spec fs persist memo
    config progress ~demanded ->
    let t = {
      config;
      fs;
      persist;
      memo;
      progress;
      jr_spec;
      build_goal;
      reflect_path;
      reflect_alias;
      buildable_targets;
      delete_eagerly;
      delete_if_depended_upon;
      delete_union;
    } in
    build_goal t demanded
    *>>| fun (_ :Proxy_map.t) -> ()

let get_env_option :
    (
      jr_spec: Jr_spec.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      Config.t ->
      Progress.t ->
      Env.t option Tenacious.t
    ) =
  fun ~jr_spec fs persist memo config progress ->
    let t = {
      config;
      fs;
      persist;
      memo;
      progress;
      jr_spec;
      build_goal;
      reflect_path;
      reflect_alias;
      buildable_targets;
      delete_eagerly;
      delete_if_depended_upon;
      delete_union;
    } in
    let builder = jenga_root t
    in
    let tenacious =
      Tenacious.map (Builder.expose builder) ~f:(function
      | Error _ -> None
      | Ok env -> (Some env)
      )
    in
    tenacious


(*----------------------------------------------------------------------
  transient progress reports
----------------------------------------------------------------------*)

let show_progress_reports config ~fin progress =
  match Config.progress config with
  | None -> ()
  | Some style ->

    let transient_message () =
      let snap = Progress.snap progress in
      Message.transient "%s" (Progress.Snap.to_string snap style)
    in

    let progress_report_period =
      match style with
      | `omake_style -> sec 1.0
      | `jem_style -> sec 0.3
    in
    don't_wait_for (
      let rec loop () =
        Clock.after progress_report_period >>= fun () ->
        if (!fin || Quit.is_quitting()) then Deferred.unit else (
          transient_message ();
          loop ()
        )
      in
      loop ()
    )

module Cycle_checking = struct
  let message_cycle_path (`Prefix prefix, `Cycle cycle) =
    Message.message "%s"
      (String.concat (List.map ~f:(fun s -> s ^ "\n") (List.concat [
         ["CYCLIC DEPENDENCIES: "];
         prefix;
         ["--- cycle starts here ---"];
         cycle;
         [List.hd_exn cycle];
       ]
       )))

  let message_to_inform_omake_server_we_are_quitting () =
    let pr fmt = ksprintf (fun s -> Core.Std.Printf.printf "%s\n%!" s) fmt in
    pr "*** OMakeroot error:";
    pr "   dependency cycle; jenga.exe quitting\n";
    pr "*** omake error:"

  let handle_cycle_if_any config () =
    match Tenacious_lib.Graph.look_for_a_cycle () with
    | None -> Deferred.return ()
    | Some cycle ->
      Message.error "Cycle found.";
      message_cycle_path cycle;
      match Config.poll_forever config with
      | true ->
        Message.error "ERROR: dependency cycle";
        Deferred.return ()
      | false ->
        message_to_inform_omake_server_we_are_quitting ();
        Quit.quit Exit_code.cycle_abort;
        Deferred.never ()

  let start_cycle_checking config =
    Option.iter Jenga_options.t.cycle_checking_interval ~f:(fun interval ->
      Clock.every' interval (handle_cycle_if_any config))

end


(*----------------------------------------------------------------------
 gc_full_major_and_show_stats
----------------------------------------------------------------------*)

let live_kb =
  let words_per_kb = 1024 / 8 in
  fun () ->
    let stat = Gc.stat () in
    let live_kb = stat.Gc.Stat.live_words / words_per_kb in
    live_kb

let _live_kb_delta =
  let last = ref 0 in
  fun () ->
    let current = live_kb () in
    let delta = current - !last in
    last := current;
    delta

(*----------------------------------------------------------------------
  build_once
----------------------------------------------------------------------*)

let compact_zero_overhead () =
  let prev_space_overhead = (Gc.get()).Gc.Control.space_overhead in
  Gc.tune ~space_overhead:0 ();
  Gc.compact();
  Gc.tune ~space_overhead:prev_space_overhead ()

let build_once :
    (Config.t -> Progress.t -> unit Builder.t ->
     (Heart.t * int) Deferred.t) =
  (* Make a single build using the top level tenacious builder
     Where a single build means we've done all we can
     (maybe we are complete, or maybe some targets are in error)
     given the current state of the file-system.
     And now we are just polling of file-system changes.
  *)
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun config progress top_builder ->
    let u = genU() in (* count of the times we reach polling *)

    let start_time = Time.now() in

    let top_tenacious = Builder.expose top_builder in

    Tenacious.exec ~name:(lazy "top-tenacious") top_tenacious >>= fun (ore,heart) ->

    (* to avoid reporting of stale errors etc... *)
    (*Progress.mask_unreachable progress dg;*)

    let errors, needs_in_error =
      match ore with
      | Ok () -> ([], Goal.Set.empty)
      | Error problem -> (Problem.reasons problem, Problem.needs_in_error problem)
    in

    let is_reachable_error need = Set.mem needs_in_error need in
    Progress.mask_unreachable progress ~is_reachable_error;

    let duration = Time.diff (Time.now()) start_time in

    let snap = Progress.snap progress in

    let effort_string = Progress.Snap.to_effort_string snap in
    let memory_metrics = Metrics.Memory.create_diff_from_previous_create () in

    let exit_code =
      if Progress.Snap.no_errors snap then (
        let built = Progress.Snap.built snap in
        Message.build_done ~duration ~u ~total:built effort_string memory_metrics;
        Exit_code.build_done
      ) else (
        let fraction = Progress.Snap.fraction snap in
        Message.build_failed ~duration ~u ~fraction effort_string memory_metrics;
        Exit_code.build_failed
      )
    in
    Message.message "%s" (Progress.Snap.to_string snap `jem_style);

    Metrics.Disk_format.append
      (Map.add ~key:"time" ~data:(Time.Span.to_float duration, Second)
         (Metrics.disjoint_union_exn
            (Progress.Snap.to_metrics snap)
            (Metrics.Memory.to_metrics memory_metrics)))
    >>| fun () ->

    List.iter errors ~f:(fun (reason, stack_trace) ->
      Reason.message_summary
        config
        ~need:(match List.last stack_trace with
          | None -> ""
          | Some n -> Goal.to_string n)
        reason;
      (if Config.show_error_dependency_paths config
       then
         List.iter (List.rev stack_trace) ~f:(fun need ->
           Message.message "Wanted by %s" (Goal.to_string need)
         )));
    heart, exit_code

(*----------------------------------------------------------------------
  entry point -- build_forever
----------------------------------------------------------------------*)

let run_user_function_from_env_opt env_opt tag ~f =
  match env_opt with
  | None -> Deferred.unit
  | Some env ->
    Monitor.try_with ~extract_exn:true (fun () ->
      f env ()
    ) >>= function
    | Ok () -> Deferred.unit
    | Error exn ->
      Message.error "%s: threw exception:\n%s" tag (Exn.to_string exn);
      Deferred.unit

let build_forever =
  (* co-ordinate the build-forever process *)

  fun config progress ~jr_spec ~top_level_demands fs persist
    ~save_db_now ~when_rebuilding ->

    let memo = Memo.create () in

    let get_env_opt =
      get_env_option
        ~jr_spec
        fs persist memo
        config progress
    in

    Cycle_checking.start_cycle_checking config;

    let rec build_and_poll ()  = (* never finishes if polling *)

      Progress.reset_metrics();

      (* start up various asyncronous writers/dumpers *)
      let fin = ref false in

      show_progress_reports config ~fin progress;

      let top_builder =
        Builder.all_unit (
          List.map top_level_demands ~f:(fun demanded ->
            build_one_root_goal
              ~jr_spec
              fs persist memo
              config progress ~demanded
          )
        )
      in

      Tenacious.exec ~name:(lazy "get_env_opt") get_env_opt >>= fun (env_opt,__heart) ->

      (* call user build_begin function *)
      run_user_function_from_env_opt env_opt "build_begin" ~f:Env.build_begin >>= fun () ->

      (* do the build once *)
      build_once config progress top_builder >>= fun (heart,exit_code) ->

      (* call user build_end function *)
      run_user_function_from_env_opt env_opt "build_end" ~f:Env.build_end >>= fun () ->

      fin := true;

      match Config.poll_forever config with
      | false ->
        save_db_now ()
        >>= fun () ->
        Message.message "build finished; not in polling mode so quitting";
        Quit.quit exit_code;
        Deferred.never()
      | true ->
        (* -P *)
        Message.polling ();
        (* wait here until something changes on the file-system *)
        let wait = Heart.when_broken heart in
        exit_code_upon_control_c := exit_code;

        begin
          (* Let's wait until it looks like we don't need to build anything, and then we
             can compact if needed. This way we don't get in the way of the user. *)
          Quit.with_prevent_quitting (fun quitting ->
            Clock.with_timeout (Time.Span.of_sec 60.)
              (Deferred.choose
                 [ Deferred.choice wait (fun () -> `Done_waiting)
                 ; Deferred.choice quitting (fun () -> `Quitting)
                 ])
            >>= function
            | `Result `Done_waiting -> Deferred.return ()
            | `Result `Quitting -> save_db_now ()
            | `Timeout ->
              save_db_now ()
              >>| fun () ->
              if not (Quit.is_quitting ()) then begin
                Message.message "GC.collecting...";
                let percentage_live =
                  Gc.full_major (); (* might run finalizers, so.. *)
                  Gc.full_major (); (* GC again *)
                  let stat = Gc.stat () in
                  let live = stat.live_words in
                  let heap = stat.heap_words in
                  Float.to_int (100. *. float live /. float heap)
                in
                if percentage_live < 80 then (
                  Message.message "GC.compacting...";
                  compact_zero_overhead();
                );
                Message.message "GC.done";
              end
          )
        end >>= fun () ->
        wait >>= fun () ->
        exit_code_upon_control_c := Exit_code.incomplete;
        Message.rebuilding ();
        when_rebuilding() >>= fun () ->
        build_and_poll ()

    in
    build_and_poll ()
