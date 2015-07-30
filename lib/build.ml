open Core.Std
open! Int.Replace_polymorphic_compare
open Async.Std

module Heart = Tenacious.Heart

let equal_using_compare compare = fun x1 x2 -> Int.(=) 0 (compare x1 x2)

let exit_code_upon_control_c = ref Exit_code.incomplete

module Target_rule = Rule.Target_rule
module Digest = Fs.Digest
module Glob = Fs.Glob
module DG = Discovered_graph
module Pm_key = Db.Pm_key
module Proxy = Db.Proxy
module Proxy_map = Db.Proxy_map
module Rule_proxy = Db.Rule_proxy
module Output_proxy = Db.Output_proxy

module PPs = struct (* list of path-tagged proxies *)
  type t = (Path.t * Proxy.t) list
end

(*----------------------------------------------------------------------
 proxy map operations
----------------------------------------------------------------------*)

module Proxy_map_op : sig

  type t = Db.Proxy_map.t with sexp_of, bin_io, compare

  val empty  : t
  val single : Pm_key.t -> Proxy.t -> t

  type inconsistency = (Pm_key.t * Proxy.t list) list
  with sexp_of

  val create_by_path : PPs.t -> [`ok of t | `err of inconsistency]
  val merge : t list -> [ `ok of t | `err of inconsistency]

  val equal : t -> t -> bool
  val diff : before:t -> after:t -> Pm_key.t list option

  val path_keys : t -> Path.Set.t

end = struct

  include Db.Proxy_map

  type inconsistency = (Pm_key.t * Proxy.t list) list with sexp_of

  let empty = Pm_key.Map.empty
  let single key proxy = Pm_key.Map.of_alist_exn [(key,proxy)]

  let create xs =
    match (Pm_key.Map.of_alist xs) with
    | `Duplicate_key key ->
      let proxys = List.filter_map xs ~f:(fun (key',proxy) ->
        if (Pm_key.equal key key') then Some proxy else None
      )
      in
      (* just find/report one inconsistency *)
      let inconsistency = [ (key,proxys) ] in
      `err inconsistency
    | `Ok map -> `ok map

  let create_by_path xs =
    create (List.map xs ~f:(fun (path,v) -> (Pm_key.of_path path,v)))

  let merge =
    let rec loop acc_t = function
      | [] -> `ok acc_t
      | (key,proxy)::xs ->
        match (Map.find acc_t key) with
        | Some proxy' ->
          if Proxy.equal proxy proxy'
          then loop acc_t xs
          else
            (* just find/report one inconsistency *)
            let inconsistency = [ (key,[proxy;proxy']) ] in
            `err inconsistency
        | None ->
          loop (Map.add acc_t ~key:key ~data:proxy) xs
    in
    fun ts ->
      let xs = List.concat_map ts ~f:(fun t -> Map.to_alist t) in
      loop empty xs

  let equal = equal_using_compare compare

  let diff ~before ~after =
    if equal before after then None
    else
      match (
        Sequence.filter_map
          (Map.symmetric_diff before after ~data_equal:Proxy.equal)
          ~f:(fun (k,comp) ->
            match comp with
            | `Left _ -> None
            | `Right _ -> Some k (* additional *)
            | `Unequal _ -> Some k (* changed *)
          )
        |> Sequence.to_list
      ) with
      | [] -> None (* deps only went away, which is not regarded as a difference *)
      | _::_ as xs -> Some xs

  let path_keys t =
    Path.Set.of_list (
      List.filter_map (Pm_key.Map.keys t) ~f:Pm_key.to_path_opt
    )

end

module Problem : sig

  type t

  (** The list of individual errors together with dependency traces.
     (a, [b,c,d]) means
     "error 'a' happened at subgoal 'd',
     which was a part of subgoal 'c',
     which was a part of subgoal 'b'".
  *)
  val reasons : t -> (Reason.t * Progress.Need.t list) list

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
  val needs_in_error : t -> Progress.Need.Set.t

  val create : Reason.t -> t
  val all : t list -> t
  val subgoal : Progress.Need.t -> t -> t

end = struct

  module Id = Unique_id.Int()

  (**
    [reasons] contains all errors collected transitively together with
    dependency paths via which they've been reached;

     [reasons_here] contains only the errors at this subgoal.
  *)
  type t = {
    reasons : (Reason.t * Progress.Need.t list) Id.Map.t
  ; reasons_here : Reason.t Id.Map.t
  ; needs_in_error : Progress.Need.Set.t
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
    ; needs_in_error = Progress.Need.Set.empty
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
    ; needs_in_error = Progress.Need.Set.empty
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
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( *>>| ) : 'a t -> ('a -> 'b) -> 'b t

  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t

  val reify : 'a t -> 'a t

  val error : Reason.t -> 'a t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  val subgoal : Progress.Need.t -> 'a t -> 'a t

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
    Tenacious.bind (Tenacious.desensitize t) (function
    | (Ok x,heart) -> Tenacious.return (Ok (x,heart))
    | (Error e, heart) ->
      Tenacious.lift (fun () ->
        Deferred.return (Error e, heart)
      )
    )

  let sensitize heart =
    Tenacious.lift (fun () -> Deferred.return (Ok (), heart))

  let bracket t ~running ~finished ~cancelled =
    reify (Tenacious.bracket t ~running ~finished ~cancelled)

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

end
let _ = Builder.(wrap,expose)

let return   = Builder.return
let ( *>>| ) = Builder.( *>>| )
let ( *>>= ) = Builder.bind
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
      let builder = Builder.reify (f ()) in
      memo := Some builder;
      builder

(*----------------------------------------------------------------------
 RR - Run_reason
----------------------------------------------------------------------*)

module RR = struct
  type t =
  | No_record_of_being_run_before
  | Action_changed
  | Deps_have_changed               of Pm_key.t list
  | Targets_missing                 of Path.Rel.t list
  | Targets_not_as_expected         of Path.Rel.t list
  with sexp_of

  (*let to_string t = Sexp.to_string (sexp_of_t t)*)

  let to_string config = function
  | No_record_of_being_run_before   -> "initial"
  | Action_changed                  -> "action"
  | Deps_have_changed keys ->
    if Config.show_actions_run_verbose config then
      sprintf "deps: %s" (String.concat ~sep:" " (List.map keys ~f:Pm_key.to_string))
    else
      "deps"

  | Targets_missing _               -> "missing"
  | Targets_not_as_expected _       -> "unexpected"

end

(*----------------------------------------------------------------------
 fixpoint step count
----------------------------------------------------------------------*)

module Fixpoint_iter = struct
  module T = struct
    type t = Iter of int | Star
    with sexp,compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make(T)
  let to_string = function Star -> "star" | Iter i -> "iter-"^Int.to_string i
  let _ = to_string
end


(*----------------------------------------------------------------------
  for memoization of dependent scheme
----------------------------------------------------------------------*)

module Dep_scheme_key = struct
  module T = struct
    type t = {
      dep_u : int;
      fixpoint_iter : Fixpoint_iter.t;
    } with sexp,compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make(T)
  let to_string t = Sexp.to_string (sexp_of_t t)
  let _ = to_string
end

module Scheme_memo = struct
  type t = (Scheme.t Builder.t * DG.Node.t) Dep_scheme_key.Table.t
end


(*----------------------------------------------------------------------
  Memo - dynamic cache - support sharing & cycle detection
----------------------------------------------------------------------*)

module Memo = struct

  (* The value in these hashtable are computations, and so are not sexp convertable.
     But that's ok, because they DONT get saved to file.

     The "ing" suffix is intended to indicate the continuous nature of these
     computatations *)

  type t = {
    reflecting : (Reflected.Trip.t option Builder.t * DG.Node.t) Path.Table.t;
    building  : (Proxy_map.t Builder.t * DG.Node.t) Goal.Table.t;
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

  type t = In_root_dir | Path of Path.t
  let in_root_dir = In_root_dir
  let path x = Path x
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
  artifacts : (t -> dir:Path.t -> Path.Set.t Builder.t);
  (* discovered_graph structure & current node - used for cycle checking *)
  discovered_graph : DG.t;
  node : DG.Node.t;

  fixpoint_iter : Fixpoint_iter.t

} with fields

let generated t = Db.generated (Persist.db t.persist)
let ruled t = Db.ruled (Persist.db t.persist)
let actioned t = Db.actioned (Persist.db t.persist)

let memo_reflecting t = t.memo.Memo.reflecting
let memo_building t = t.memo.Memo.building

let build_sub_goal : (t ->  Goal.t -> Proxy_map.t Builder.t) =
  fun t goal -> t.build_goal t goal

let apply_user_function : ((unit -> 'a) -> 'a Builder.t) =
  fun f ->
    try return (f ()) with
    | exn -> error (Reason.Usercode_raised (sexp_of_exn exn))

let run_user_async_code : ((unit -> 'a Deferred.t) -> 'a Builder.t) =
  fun f ->
    Builder.of_deferred (fun () ->
      Monitor.try_with (fun () ->
        f ()
      )
    ) *>>= function
    | Ok x -> return x
    | Error exn ->
      let exn = Monitor.extract_exn exn in
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

let remove_stale_artifact : (
  t -> dir:Path.t -> Path.t -> unit Deferred.t
) =
  fun t ~dir path ->
    let path_string = Path.to_string path in
    (* Dont remove non-local stale build artifacts
       We no longer allow (api.ml) rules to be created with non-local targets.
       But jenga.db might have been created by ealier version of jenga which did. *)
    if not (Path.(dirname path = dir)) then (
      Deferred.return ()
    ) else
      try_with (fun () -> Sys.remove path_string) >>= function
      | Ok () ->
        if Config.show_actions_run t.config then (
          Message.message "Removed stale build artifact: %s" path_string;
        );
        Fs.clear_watcher_cache t.fs path;
        Deferred.return ()
      | Error _ ->
        Deferred.return ()

let determine_and_remove_stale_artifacts : (
  t -> dir:Path.t -> targets:Path.Set.t
  -> unit Builder.t
) =
  fun t ~dir ~targets ->
    match Path.case dir with
    | `absolute _ -> return ()
    | `relative rel ->
      t.artifacts t ~dir *>>= fun artifacts ->
      let stales = Path.Set.diff artifacts targets in
      begin
        match Set.to_list stales with
        | [] -> return ()
        | _::_ as stales ->
          Builder.of_deferred (fun () ->
            Locking.lock_directory_for_action ~dir (fun () ->
              Deferred.List.iter stales ~f:(fun path ->
                remove_stale_artifact t ~dir path
              )))
      end
      *>>= fun () ->
      let gen_key = Gen_key.create ~dir:rel in
      let prev_targets =
        match (Hashtbl.find (generated t) gen_key) with
        | Some x -> x
        | None -> Path.Set.empty
      in
      if not (Path.Set.equal prev_targets targets) then (
        Hashtbl.set (Persist.modify "generated" (generated t)) ~key:gen_key ~data:targets;
      );
      return ()

(*----------------------------------------------------------------------
  share / share & allow cycle detection
----------------------------------------------------------------------*)

let share_builder : (
  key : 'a ->
  memo : ('a, 'b Builder.t) Hashtbl.t ->
  f : (unit -> 'b Builder.t) ->
  'b Builder.t
) =
  fun ~key ~memo ~f ->
    match (Hashtbl.find memo key) with
    | Some builder -> builder
    | None ->
      let builder = f () in
      let builder = Builder.reify builder in
      Hashtbl.add_exn memo ~key ~data:builder;
      builder

let share_and_check_for_cycles :
    (* memoize / check for cycles, by constructing nodes in the discovered_graph *)
    (t ->
     key : 'a ->
     memo : ('a, 'b Builder.t * DG.Node.t) Hashtbl.t ->
     item : DG.Item.t ->
     f : (t -> 'b Builder.t) ->
     'b Builder.t
    ) =
  fun t ~key ~memo ~item ~f ->
    match (Hashtbl.find memo key) with
    | Some (builder,node) ->
      begin
        (* The existing node just reached is also a dependency of t.node *)
        DG.link_dependants_no_cycle_check t.discovered_graph node ~additional:t.node;
        builder;
      end
    | None ->
      let pre_node = t.node in
      let node = DG.create_dependency t.discovered_graph pre_node item in
      let t = {t with node = node} in
      (* newly created node(item) is a dependency of the t.node we were at before *)
      let builder = f t in (* critical - [f] must be called only once *)
      let builder =
        Builder.bracket builder
          ~running:(fun i ->
            if i >= 1 then
              DG.remove_all_dependencies t.discovered_graph t.node)
          ~finished:ignore
          ~cancelled:ignore
      in
      (* we use add_exn, because each key will be memoized exactly once *)
      Hashtbl.add_exn memo ~key ~data:(builder,t.node);
      builder

(*----------------------------------------------------------------------
 glob - new semantics (on_filesystem or buildable)
----------------------------------------------------------------------*)

let on_filesystem t ~dir =
  (* Treat symbolic-links the same as regular files.
     To allow [source_files] to be used in user-rule for [artifacts].
     Means symbolic-links to directories are not handled correctly.
     But matches [Fs.is_digestable] which also handles [`Link] and [`File] the same. *)
  let glob = Fs.Glob.create ~dir ~kinds:[`File;`Link] "*" in
  glob_fs_only t glob

let buildable_targets_and_clean_when_fixed : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  fun t ~dir ->
    t.buildable_targets t ~dir *>>= fun targets ->
    begin
      match t.fixpoint_iter with
      | Iter _ -> return ()
      | Star -> determine_and_remove_stale_artifacts t ~dir ~targets
    end
    *>>= fun () ->
    return targets

let glob_fs_or_buildable : (t -> Glob.t -> Path.Set.t Builder.t) =
  let module Key = struct
    module T = struct
      type t = Glob.t * Fixpoint_iter.t with sexp, compare
      let hash = Hashtbl.hash
    end
    include Hashable.Make(T)
  end in
  let memo = Key.Table.create() in
  fun t glob ->
    share_builder ~key:(glob, t.fixpoint_iter) ~memo ~f:(fun () ->
      Builder.cutoff
        ~equal:Path.Set.equal
        (begin
          if Glob.kind_allows_file glob
          then
            let dir = Glob.dir glob in
            let pattern = Glob.pattern glob in
            buildable_targets_and_clean_when_fixed t ~dir *>>| fun targets ->
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
        *>>| fun filesystem ->
        Path.Set.union filtered_targets filesystem
       ))

(*----------------------------------------------------------------------
  running actions
----------------------------------------------------------------------*)

let run_action_with_message :
  (t -> Env.t -> Action.t -> message:(unit->unit) ->
   deps:Proxy_map.t -> targets:Path.Rel.t list -> need:string ->
   output:'a Job.Output.t -> 'a Builder.t) =
  fun t env action ~message ~deps ~targets ~need ~output ->
    let action =
      Action_sandbox.maybe_sandbox ~sandbox:t.config.sandbox_actions
        action ~deps ~targets
    in
    let progress = t.progress in
    let config = t.config in
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
          Action.run action ~message ~output ~putenv ~progress ~config ~need
        ))) *>>= function
    | Ok x                                -> return x
    | Error (`command_failed output)     -> error (Reason.Command_failed output)
    | Error (`other_error Job.Shutdown)   -> error Reason.Shutdown
    | Error (`other_error exn)            -> error (Reason.Running_job_raised (sexp_of_exn exn))

let run_action_for_targets :
    (t -> RR.t -> Env.t -> Action.t ->
     deps:Proxy_map.t ->
     targets:Path.Rel.t list -> need:string ->
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

let run_action_for_stdout :
    (t -> RR.t -> Env.t -> deps:Proxy_map.t -> Action.t -> string Builder.t) =
  fun t rr env ~deps action ->
    let job = Action.job action in
    let message() =
      if Config.show_actions_run t.config then
        Message.message "Action(%s): %s [%s]"
          (Path.to_string (Job.dir job))
          (Job.to_sh_ignoring_dir job)
          (RR.to_string t.config rr)
    in
    run_action_with_message t env action ~message ~deps ~targets:[] ~need:"stdout"
      ~output:Job.Output.stdout

(*----------------------------------------------------------------------
 report errors / record status
----------------------------------------------------------------------*)

let set_status t need status =
  Progress.set_status t.progress need status

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
    Reason.messages ~need:(Progress.Need.to_string need) reason);
  if Config.stop_on_first_error t.config && not (Reason.filesystem_related reason) then (
    Quit.quit Exit_code.build_failed;
  )


let report_status t need ore =
  match ore with
  | Ok _ ->
    set_status t need (Some Progress.Status.Built)
  | Error problem -> (
    let reasons = Problem.reasons_here problem in
    List.iter reasons ~f:(fun reason ->
      report_error_for_need t need reason
    );
    set_status t need (Some (Progress.Status.Error reasons))
  )

let build_considering_needed : (t -> Progress.Need.t -> 'a Builder.t -> 'a Builder.t) =
  (* Expose the builder's result/error for reporting *)
  fun t need builder ->
    Builder.subgoal need (
      (* Report considering/re-considering; count as: check/recheck *)
      Builder.bracket builder
        ~running:(fun i ->
          set_status t need (Some Progress.Status.Todo);
          let first_time = i = 0 in
          if Config.show_considering t.config
          || (not first_time && Config.show_reconsidering t.config)
          then (
            Message.message !"%s: %{Progress.Need}"
              (if first_time then "Considering" else "Re-considering") need
          ))
        ~finished:(fun a ->
          Effort.incr Progress.considerations_run;
          report_status t need a)
        ~cancelled:(fun () ->
          set_status t need None;
          if Config.show_considering t.config
          then (
            Message.message !"Not considering anymore: %{Progress.Need}" need
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

let read_then_convert_string_via_reader :
    (
      path : Path.t ->
      contents : (Path.t -> string Builder.t) ->
      do_read : (Reader.t -> 'a Deferred.t) ->
      'a Builder.t
    ) =
  fun ~path ~contents ~do_read ->
    contents path *>>= fun string ->
    Builder.of_deferred (fun () ->
      try_with (fun () ->
        let info = Info.of_string ("read_then_convert_string_via_reader"
                                   ^ Path.to_string path) in
        let pipe = Pipe.init (fun writer -> Pipe.write writer string) in
        Reader.of_pipe info pipe >>= fun reader ->
        do_read reader >>= fun outcome ->
        Reader.close reader >>| fun () ->
        outcome
      )
    ) *>>= function
    | Ok x -> return x
    | Error exn ->
      one_error_for_omake_server ~within:path
        ~extra:(Exn.to_string exn) "failed sexp conversion"


module Jenga_conf_rep : sig
  type t with of_sexp
  val modules : t -> string list
end = struct
  type t = [`modules of string list] with sexp
  let modules (`modules xs) = xs
end


let read_jenga_conf : (t -> conf:Path.t -> Jenga_conf_rep.t Builder.t) =
  fun t ~conf:path ->
    read_then_convert_string_via_reader
      ~path
      ~contents:(get_contents t)
      ~do_read:(fun reader ->
        Reader.read_sexp reader >>| function
        | `Ok sexp -> Jenga_conf_rep.t_of_sexp sexp
        | `Eof -> failwith "Eof"
      )

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

let jenga_load_spec : (t -> Jr_spec.t -> Load_root.Spec.t Builder.t) =
  fun t jr_spec ->
    match jr_spec with
    | Jr_spec.Path path ->
      let is_ml =
        match String.lsplit2 ~on:'.' (Path.basename path) with
        | Some (_,"ml") -> true
        | Some _ | None -> false
      in
      if is_ml
      then jenga_root_load_spec t ~root_ml:path
      else jenga_conf_load_spec t ~conf:path
    | Jr_spec.In_root_dir ->
      let conf = Path.of_relative (Special_paths.jenga_conf) in
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
      | `just_old_style_root -> jenga_root_load_spec t ~root_ml
      | `just_new_style_conf
      | `both_conf_and_root -> jenga_conf_load_spec t ~conf

(*----------------------------------------------------------------------
  jenga_root
----------------------------------------------------------------------*)

let jenga_root : (t -> (Env.t * Scheme_memo.t) Builder.t) =
  (* wrap up the call to [Load_root.get_env] into a tenacious builder,
     which will reload any time the jengaroot is modified *)
  fun t ->
    jenga_load_spec t t.jr_spec *>>= fun load_spec ->
    Builder.of_deferred (fun () -> Load_root.get_env load_spec) *>>= function
    | Error e ->
      let exn = Monitor.extract_exn (Error.to_exn e) in
      error (Reason.Usercode_raised (sexp_of_exn exn))
    | Ok env ->
      let bds_memo = Dep_scheme_key.Table.create() in
      return (env,bds_memo)

let jenga_root : (t -> (Env.t * Scheme_memo.t) Builder.t) =
  fun t ->
    build_considering_needed t Progress.Need.jengaroot (
      jenga_root t
    )

let jenga_root : (t -> (Env.t * Scheme_memo.t) Builder.t) =
  (* Memoization of jenga_root is simpler that other cases, because:
     - There is only one; we don't need a hashtable, just a ref.
     - The root has no deps, so there is no chance of cycles *)
  let memo = ref None in
  fun t ->
    memo_builder ~memo (fun () ->
      jenga_root t
    )

(*----------------------------------------------------------------------
 Mtimes - to check mtimes are unchanged for deps while an action is running
----------------------------------------------------------------------*)

module Mtimes : sig

  type t
  val create : (Path.t * Fs.Mtime.t) list -> t
  val keys : t -> Path.Set.t
  val diff : before:t -> after:t -> Path.t list option

end = struct

  type t = Fs.Mtime.t Path.Map.t with compare

  let create xs = Path.Map.of_alist_exn xs

  let keys t = Path.Set.of_list (Path.Map.keys t)

  let equal = equal_using_compare compare

  let diff ~before ~after =
    (* expect to only be called on [Mtimes.t] values with the same path-keys *)
    if equal before after then None
    else
      match (
        Sequence.filter_map
          (Map.symmetric_diff before after ~data_equal:Fs.Mtime.equal)
          ~f:(fun (k,comp) ->
            match comp with
            | `Left _ -> Some k (* lost path-key.. unexpected *)
            | `Right _ -> Some k (* additional path-key.. unexpected *)
            | `Unequal _ -> Some k (* changed mtime *)
          )
        |> Sequence.to_list
      ) with
      | [] -> assert false
      | _::_ as xs -> Some xs

end

let mtime_file : (
  [`as_cached of t | `right_now] -> Path.t -> (Path.t * Fs.Mtime.t) Builder.t
) =
  fun how path ->
    begin
      match how with
      | `as_cached t -> Builder.of_tenacious (Fs.mtime_file t.fs ~file:path)
      | `right_now -> Builder.of_deferred (fun () -> Fs.mtime_file_right_now ~file:path)
    end *>>= function
    | Some mtime -> return (path,mtime)
    | None -> error (Reason.Misc (sprintf "file disappeared for mtime: %s"
                                    (Path.to_string path)))

let mtimes_of_proxy_map : (t -> Proxy_map.t -> Mtimes.t Builder.t) =
  fun t pm ->
    let paths = Proxy_map_op.path_keys pm in
    Builder.all (
      List.map (Set.to_list paths) ~f:(fun path -> mtime_file (`as_cached t) path)
    )*>>| Mtimes.create

let check_mtimes_unchanged : (t -> Mtimes.t -> unit Builder.t) =
  fun t mtimes_before ->
    let paths = Mtimes.keys mtimes_before in
    begin
      Builder.all (
        List.map (Set.to_list paths) ~f:(fun path -> mtime_file `right_now path)
      )*>>| Mtimes.create
    end *>>= fun mtimes_now ->
    match Mtimes.diff ~before:mtimes_before ~after:mtimes_now with
    | None -> return ()
    | Some paths ->
      (* clearing the watcher cache allows the build to retrigger if running
         without notifiers, or we lost an inotify event *)
      let () = List.iter paths ~f:(Fs.clear_watcher_cache t.fs) in
      error (Reason.Mtimes_changed paths)

(*----------------------------------------------------------------------
 things we can depend upon
----------------------------------------------------------------------*)

let run_action_for_stdout_if_necessary
    : (t -> deps:Proxy_map.t -> Action.t -> string Builder.t) =
  fun t ~deps action ->
    jenga_root t *>>= fun (env,_) ->
    mtimes_of_proxy_map t deps *>>= fun mtimes ->
    let job = Action.job action in
    let run_and_cache rr =
      Builder.uncancellable (
        run_action_for_stdout t rr env action ~deps *>>= fun stdout ->
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
    | `err _inconsistency -> error Reason.Inconsistent_proxies
    | `ok pm -> return pm

(*----------------------------------------------------------------------
 source_files
----------------------------------------------------------------------*)

let source_files : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  (* Set of files (per directory) [on_filesystem] \\ [buildable_targets].
     Likely to be useful for the user-provided [artifacts] definition. *)
  let memo = Path.Table.create() in
  fun t ~dir ->
    share_builder ~memo ~key:dir ~f:(fun () ->
      (* We use [cutoff] to avoid triggering dependents as buildable files are generated
         for the first time & appear on the filesystem. *)
      Builder.cutoff ~equal:(fun set1 set2 -> Path.Set.equal set1 set2) (
        Builder.both (on_filesystem t ~dir) (t.buildable_targets t ~dir)
        *>>| fun (filesystem,buildable) ->
        Path.Set.diff filesystem buildable))

(*----------------------------------------------------------------------
 build_depends
----------------------------------------------------------------------*)

let build_depends : (t -> 'a Dep.t -> ('a * Proxy_map.t) Builder.t) =
  fun t depends ->
    let rec exec : type a. a Dep.t -> (a * Proxy_map.t) Builder.t = fun dep ->
      match dep with

      | Dep.Return x ->
        return (x, Proxy_map_op.empty)

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
        run_action_for_stdout_if_necessary t ~deps:pm action *>>| fun stdout ->
        (stdout, Proxy_map.empty)

      | Dep.Reflect_path path ->
        t.reflect_path t path *>>| fun x ->
        (x,Proxy_map.empty)

      | Dep.Reflect_alias alias ->
        t.reflect_alias t alias *>>| fun x ->
        (x,Proxy_map.empty)

      | Dep.Reflect_putenv ->
        jenga_root t *>>| fun (env,_) ->
        (Env.putenv env, Proxy_map.empty)

      | Dep.On_filesystem dir ->
        on_filesystem t ~dir *>>| fun paths ->
        (paths, Proxy_map.empty)

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

    in
    exec depends

(*----------------------------------------------------------------------
 reflect depends
----------------------------------------------------------------------*)

let reflect_depends : (t -> 'a Dep.t -> ('a * Path.Set.t) Builder.t) =
  fun t depends ->
    let rec exec : type a. a Dep.t -> (a * Path.Set.t) Builder.t = function

      | Dep.Return x ->
        return (x, Path.Set.empty)

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
        build_depends t action_depends *>>= fun (action,deps_proxy) ->
        run_action_for_stdout_if_necessary t ~deps:deps_proxy action *>>| fun stdout ->
        (stdout, Path.Set.empty)

      | Dep.Reflect_path path ->
        t.reflect_path t path *>>| fun x ->
        (x,Path.Set.empty)

      | Dep.Reflect_alias alias ->
        t.reflect_alias t alias *>>| fun x ->
        (x,Path.Set.empty)

      | Dep.Reflect_putenv ->
        jenga_root t *>>| fun (env,_) ->
        (Env.putenv env, Path.Set.empty)

      | Dep.On_filesystem dir ->
        on_filesystem t ~dir *>>| fun paths ->
        (paths, Path.Set.empty)

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

    in
    exec depends

(*----------------------------------------------------------------------
 check_for_non_local_rules
----------------------------------------------------------------------*)

let check_for_non_local_rules ~dir ~targets =
  let targets = Path.Set.to_list targets in
  match
    List.filter targets ~f:(fun path -> not (Path.(dirname path = dir)))
  with
  | [] -> return ()
  | _::_ as non_local_targets ->
    error (Reason.Misc (
      sprintf "non-local rule-targets generated for directory: [%s] - %s"
        (Path.to_string dir)
        (String.concat ~sep:" " (List.map non_local_targets ~f:Path.to_string))
    ))

(*----------------------------------------------------------------------
 dependent schemes
----------------------------------------------------------------------*)

(* [dir] is for error reporting *)
let build_dependent_scheme : (
  t -> dir:Path.t -> int * Scheme.t Dep.t -> Scheme.t Builder.t
) =
  fun t ~dir (dep_u,scheme_dep) ->
    jenga_root t *>>= fun (_,scheme_memo) ->
    let key = {Dep_scheme_key. dep_u; fixpoint_iter = t.fixpoint_iter} in
    let item = DG.Item.Dep_scheme (dir, dep_u) in
    share_and_check_for_cycles t ~key ~memo:scheme_memo ~item ~f:(fun t ->
      build_depends t scheme_dep *>>| fun (scheme,__proxy_map) ->
      scheme
    )

(*----------------------------------------------------------------------
schemes...
----------------------------------------------------------------------*)

let get_scheme : (t -> dir:Path.t -> Scheme.t Builder.t) =
  let memo = Path.Table.create() in
  fun t ~dir ->
    share_builder ~memo ~key:dir ~f:(fun () ->
      jenga_root t *>>= fun (env,_) ->
      apply_user_function (fun () ->
        Env.get_scheme env ~dir
      )
    )

let scheme_rules : (t -> dir:Path.t -> Rule.t list Builder.t) =
  fun t ~dir ->
    match Path.case dir with
    | `absolute _ -> return []
    | `relative _rel ->
      let rec collect = fun s ->
        match s with
        | Scheme.Exclude (_,scheme) -> collect scheme
        | Scheme.All schemes -> Builder.all (List.map schemes ~f:collect) *>>| List.concat
        | Scheme.Dep u_scheme_dep -> build_dependent_scheme ~dir t u_scheme_dep *>>= collect
        | Scheme.Rules ruleset -> return (Ruleset.rules ruleset)
      in
      get_scheme t ~dir *>>= fun scheme ->
      collect scheme

let find_alias_deps : (t -> Alias.t -> unit Dep.t Builder.t) =
  fun t alias ->
    let dir = Path.of_relative (Alias.directory alias) in
    scheme_rules t ~dir *>>= fun rules ->
    match Ruleset.lookup_alias (Ruleset.create rules) alias with
    | [] -> error (Reason.No_definition_for_alias alias)
    | _::_  as deps -> return (Dep.all_unit deps)

let find_target_rule : (t -> Path.Rel.t -> Target_rule.t option Builder.t) =
  fun t rel ->
    let dir = Path.of_relative (Path.Rel.dirname rel) in
    let rec find_list = function
      | [] -> return None
      | scheme::schemes ->
        find scheme *>>= function
        | None -> find_list schemes
        | Some tr -> return (Some tr)
    and find = function
      (* Dont traverse those sub-schemes which exclude the path being sought *)
      | Scheme.Exclude (cond,scheme) ->
          if cond rel
          then return None
          else find scheme
      | Scheme.All schemes -> find_list schemes
      | Scheme.Dep u_scheme_dep ->
        build_dependent_scheme ~dir t u_scheme_dep *>>= fun scheme ->
        find scheme
      | Scheme.Rules ruleset ->
        match (Ruleset.lookup_target ruleset rel) with
        | `ok opt -> return opt
        | `dup -> error (Reason.Multiple_rules_for_path rel)
    in
    get_scheme t ~dir *>>= fun scheme ->
    find scheme

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
    (t -> Target_rule.t -> demanded:Path.Rel.t -> PPs.t Builder.t) =
  fun t tr ~demanded ->
    let need = Path.Rel.basename demanded in
    let targets = Target_rule.targets tr in
    (* Any glob reference within rules has fixpoint semantics *)
    let t = {t with fixpoint_iter = Fixpoint_iter.Star} in
    build_depends t (Target_rule.action_depends tr) *>>= fun (action,deps_proxy_map) ->
    mtimes_of_proxy_map t deps_proxy_map *>>= fun mtimes ->
    (* The persistent caching is keyed of the [head_target] *)
    let head_target,other_targets = Target_rule.head_target_and_rest tr in
    jenga_root t *>>= fun (env,_) ->
    let job = Action.job action in
    let run_and_cache rr =
      Builder.uncancellable (
        run_action_for_targets t rr env action ~deps:deps_proxy_map ~targets ~need *>>= fun () ->
        (* The cache is cleared AFTER the action has been run, and BEFORE we check the
           targets created by the action. Clearing the cache forces the files to be
           re-stated, without relying on any events from inotify to make this happen *)
        let () =
          List.iter targets ~f:(fun rel ->
            Fs.clear_watcher_cache t.fs (Path.of_relative rel)
          )
        in
        check_targets t targets *>>= function
        | `missing paths -> error (Reason.Rule_failed_to_generate_targets paths)
        | `ok path_tagged_proxys ->
          match (Proxy_map_op.create_by_path path_tagged_proxys) with
          | `err _inconsistency -> error Reason.Inconsistent_proxies
          | `ok targets_proxy_map ->
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
            | `err _inconsistency -> error Reason.Inconsistent_proxies
            | `ok targets_proxy_map ->
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

(**
   [demanded] should not affect anything important as it's not
   a part of the key we memoize on!
   Currently we only use it to print error messages.
*)
let build_target_rule :
    (t -> Path.Rel.t -> demanded:Path.Rel.t -> PPs.t Builder.t) =
  let memo = Path.Rel.Table.create() in
  fun t head_target ~demanded ->
    share_builder ~memo ~key:head_target ~f:(fun () ->
      find_target_rule t head_target *>>= function
      | Some tr ->
        build_target_rule t tr ~demanded
      | None ->
        error (Reason.No_rule_or_source (Path.of_relative head_target)))


let expect_source : (t -> Path.Rel.t -> Proxy_map.t Builder.t) =
  fun t demanded ->
    let path = Path.of_relative demanded in
    digest_path t path *>>= fun res ->
    match res with
    | `missing ->  error (Reason.No_rule_or_source path)
    | `is_a_dir -> error (Reason.Unexpected_directory path)
    | `file digest ->
      let proxy = Proxy.of_digest digest in
      let pm = Proxy_map.single (Pm_key.of_rel_path demanded) proxy in
      return pm

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  (* build a goal -- alias or path
     In either case, first get the ruleset applicable to the goal.
     For an alias - there must be a rule in this ruleset.
     For a path
     - If there is a rule, use it.
     - Otherwise, that path had better exists as source. *)
  fun t goal ->
    match goal with
    | Goal.Alias alias ->
      begin
        find_alias_deps t alias *>>= fun depends ->
        build_depends t depends *>>| fun ((),pm) -> pm
      end
    | Goal.Path demanded ->
      begin
        find_target_rule t demanded *>>= function
        | Some tr ->
          let head_target = Target_rule.head_target tr in
          build_target_rule t head_target ~demanded *>>= fun tagged ->
          let (path,proxy) =
            List.find_exn tagged ~f:(fun (path,_) ->
              Path.equal path (Path.of_relative demanded)
            )
          in
          let pm = Proxy_map.single (Pm_key.of_path path) proxy in
          return pm
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
    build_considering_needed t (Progress.Need.goal goal) (
      build_goal t goal
    )

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  fun t goal ->
    share_and_check_for_cycles t
      ~key: goal
      ~memo: (memo_building t)
      ~item: (DG.Item.Goal goal)
      ~f: (fun t -> build_goal t goal)

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
        let need = Progress.Need.goal goal in
        if Config.show_reflecting t.config then (
          Message.message "Reflecting: %s" (Progress.Need.to_string need);
        )
      end;
      find_target_rule t rel *>>= function
      | None -> return None (* scheme has no rule, so path must be source *)
      | Some tr ->
        let targets = List.map (Target_rule.targets tr) ~f:Path.of_relative in
        reflect_depends t (Target_rule.action_depends tr) *>>| fun (action,deps) ->
        let action = Action.job action in
        let deps = Path.Set.to_list deps in
        Some { Reflected.Trip. targets; deps; action; }

let reflect_path : (t -> Path.t -> Reflected.Trip.t option Builder.t) =
  fun t path ->
    share_and_check_for_cycles t
      ~key: path
      ~memo: (memo_reflecting t)
      ~item: (DG.Item.Reflect path)
      ~f: (fun t -> reflect_path t path)

let reflect_alias : (t -> Alias.t -> Path.Set.t Builder.t) =
  fun t alias ->
    find_alias_deps t alias *>>= fun depends ->
    reflect_depends t depends *>>| fun ((),set) ->
    set

(*----------------------------------------------------------------------
 general builder fixpointing procedure
----------------------------------------------------------------------*)

let fixpoint : (
  equal:('a -> 'a -> bool) ->
  check_another_allowed : (int -> last:'a -> curr:'a -> unit Builder.t) ->
  f:(int -> 'a Builder.t) ->
  (* return fixpointed result & number of iterations to get there *)
  ('a * int) Builder.t
) =
  fun ~equal ~check_another_allowed ~f ->
    let rec iterate i ~last =
      assert (Int.(i>=1));
      f i *>>= fun curr ->
      if equal last curr
      then
        return (curr,i)
      else (
        check_another_allowed i ~last ~curr *>>= fun () ->
        iterate (i+1) ~last:curr
      )
    in
    f 0 *>>= fun init ->
    iterate 1 ~last:init

(*----------------------------------------------------------------------
 buildable_targets
----------------------------------------------------------------------*)

let pluralize_count n word =
  sprintf "%d %s%s" n word (if Int.(n<>1) then "s" else "")

let check_next_iteration_allowed t ~dir i ~last ~curr =
  let n = Config.buildable_targets_fixpoint_max t.config in
  if Int.(n = 0) then return () (* no limit *)
  else if Int.(i > n)
  then
    let added_in_last_iteration = Path.Set.diff curr last in
    error (Reason.Misc (
      sprintf
        "buildable_targets for directory [%s] failed to reach fixpoint after %s; \
last iteration added: {%s}"
        (Path.to_string dir)
        (pluralize_count n "step")
        (String.concat ~sep:","
           (List.map (Path.Set.to_list added_in_last_iteration)
              ~f:Path.to_string))))
  else
    return ()

let fixpoint_buildable_targets : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  fun t ~dir ->
    begin
      fixpoint
        ~equal:Path.Set.equal
        ~check_another_allowed:(check_next_iteration_allowed t ~dir)
        ~f:(fun i ->
          let t = {t with fixpoint_iter = Iter i} in
          t.buildable_targets t ~dir *>>= fun targets ->
          return targets
        )
    end
    *>>= fun (targets,iter_count) ->
    assert (Int.(iter_count>=1));
    (* The [iter_count] is always at least 1; and normally at least 2 *)
    (* A count of 1 will only occur is there is nothing buildable *)
    if Int.(iter_count=1) then assert (Path.Set.is_empty targets);
    if Config.show_buildable_discovery t.config then (
      if Path.Set.is_empty targets then (
        Message.message "No buildable targets in: %s" (Path.to_string dir);
      ) else (
        (* subtract 1 from [iter_count] when reporting #steps for user *)
        let steps = iter_count-1 in
        assert (Int.(steps>=1));
        Message.message "Discovered buildable targets in: %s [fixpoint after %s]"
          (Path.to_string dir)
          (pluralize_count steps "step")
      )
    );
    check_for_non_local_rules ~dir ~targets *>>= fun () ->
    return targets

module Buildable_targets_key = struct
  module T = struct
    type t = {
      dir : Path.t;
      fixpoint_iter : Fixpoint_iter.t;
    } with sexp,compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make(T)
  let to_string t = Sexp.to_string (sexp_of_t t)
  let _ = to_string
end

let buildable_targets : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  let memo = Buildable_targets_key.Table.create() in
  fun t ~dir ->
    let key = {Buildable_targets_key. dir; fixpoint_iter = t.fixpoint_iter} in
    let item = DG.Item.Buildable dir in
    share_and_check_for_cycles t ~item ~key ~memo ~f:(fun t ->
    match t.fixpoint_iter with
    | Star -> fixpoint_buildable_targets t ~dir
    | Iter i ->
      assert (Int.(i>=0));
      if Int.(i = 0)
      then return Path.Set.empty
      else
        let t = {t with fixpoint_iter = Iter (i-1)} in
        scheme_rules t ~dir *>>= fun rules ->
        let targets = List.concat_map rules ~f:Rule.targets in
        let targets = List.map targets ~f:Path.of_relative in
        let targets = Path.Set.of_list targets in
        return targets
    )

(*----------------------------------------------------------------------
 artifacts
----------------------------------------------------------------------*)

let internal_artifacts : (t -> dir:Path.t -> Path.Set.t) =
  (* Determine artifacts as what was previously build by jenga. Not completely reliable -
     doesn't work if jenga.db is removed - but avoids any effort from the rule-author. *)
  fun t ~dir ->
    match Path.case dir with
    | `absolute _ -> Path.Set.empty
    | `relative dir ->
      let gen_key = Gen_key.create ~dir in
      let prev_targets =
        match (Hashtbl.find (generated t) gen_key) with
        | Some x -> x
        | None -> Path.Set.empty
      in
      prev_targets

let artifacts : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  (* Determine artifact in [dir] using [Artifact_policy.t] from [env] *)
  let memo = Path.Table.create() in
  fun t ~dir ->
    let item = DG.Item.Artifacts dir in
    share_and_check_for_cycles t ~item ~memo ~key:dir ~f:(fun t ->
      jenga_root t *>>= fun (env,_) ->
      match Env.artifacts_policy env with
      | Artifact_policy.Use_persistent_state -> return (internal_artifacts t ~dir)
      | Artifact_policy.Artifacts f ->
        build_depends t (f ~dir) *>>| fun (paths,__proxy_map) ->
        Path.Set.of_list paths
      )

(*----------------------------------------------------------------------
 buildable
----------------------------------------------------------------------*)

let build_one_root_goal :
    (
      jr_spec: Jr_spec.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      DG.t ->
      dg_root:DG.Node.t ->
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
  fun ~jr_spec fs persist memo discovered_graph ~dg_root
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
      artifacts;
      discovered_graph;
      node = dg_root;
      fixpoint_iter = Fixpoint_iter.Star;
    } in
    build_goal t demanded
    *>>| fun (_ :Proxy_map.t) -> ()

let get_env_option :
    (
      jr_spec: Jr_spec.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      DG.t ->
      dg_root:DG.Node.t ->
      Config.t ->
      Progress.t ->
      Env.t option Tenacious.t
    ) =
  fun ~jr_spec fs persist memo discovered_graph ~dg_root config progress ->
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
      artifacts;
      discovered_graph;
      node = dg_root;
      fixpoint_iter = Fixpoint_iter.Star;
    } in
    let builder = jenga_root t
    in
    let tenacious =
      Tenacious.map (Builder.expose builder) ~f:(function
      | Error _ -> None
      | Ok (env,_) -> (Some env)
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

(*----------------------------------------------------------------------
 deadlock/cycle detection
----------------------------------------------------------------------*)

let look_for_a_cycle : (DG.t -> DG.Node.t list option) =
  (* walk the graph in CPS style to avoid blowing the stack on pathological
     deep examples *)
  fun dg ->

    (* start/finish visiting a node... *)
    let module Visit : sig

      val start : DG.Node.t -> [
      | `seen_already
      | `cycle of DG.Node.t list
      | `started of [ `finish of (unit -> unit) ]
      ]

    end  = struct

      let visiting = DG.Node.Hash_set.create ()
      let path = ref []
      let seen = DG.Node.Hash_set.create ()

      let start node =
        if Hash_set.mem seen node
        then `seen_already
        else
          if Hash_set.mem visiting node
          then `cycle !path
          else (
            Hash_set.add visiting node;
            path := node :: !path;
            `started (
              `finish (fun () ->
                path := (
                  match !path with
                  | [] -> assert false
                  | node'::path ->
                    assert (Int.equal 0 (DG.Node.compare node node'));
                    path
                );
                Hash_set.remove visiting node;
                Hash_set.add seen node
              )
            )
          )
    end
    in
    let rec walk_list nodes ~k =
      match nodes with
      | [] -> k ()
      | node::other_nodes ->
        match Visit.start node with
        | `seen_already -> walk_list other_nodes ~k
        | `cycle path -> Some (node, path)
        | `started (`finish finish) ->
          (* Mark that we are visting this node while walking its dependencies;
             setting up a new continuation for the other_nodes at this level. *)
          walk_list (DG.dependencies node) ~k:(fun () ->
            finish();
            walk_list other_nodes ~k
          )
    in
    match
      walk_list (DG.roots dg) ~k:(fun () -> None)
    with
    | None -> None
    | Some (the_nub,full_path) ->
      let cycle_path =
        let rec loop acc = function
          | [] -> assert false
          | node::nodes ->
            if (DG.Node.equal node the_nub) then List.rev (node::acc) else
              loop (node::acc) nodes
        in
        loop [] full_path
      in
      let cycle_path_with_repeated_nub = the_nub :: cycle_path in
      Some cycle_path_with_repeated_nub


let message_cycle_path dg ~cycle_path_with_repeated_nub =
  Message.message "CYCLIC DEPENDENCIES: %s"
    (String.concat (
      List.map cycle_path_with_repeated_nub ~f:(fun node ->
        let item = DG.lookup_item dg node in
        sprintf "\n- [%s] %s" (DG.id_string node)
        (*sprintf "\n- %s"*)
          (DG.Item.to_string item))))

let message_to_inform_omake_server_we_are_quitting () =
  let pr fmt = ksprintf (fun s -> Core.Std.Printf.printf "%s\n%!" s) fmt in
  pr "*** OMakeroot error:";
  pr "   dependency cycle; jenga.exe quitting\n";
  pr "*** omake error:"

let deadlock_watch_period = sec 3.0

let watch_for_deadlock ~deadlock_found ~fin dg =
  let quit () =
    message_to_inform_omake_server_we_are_quitting();
    Ivar.fill deadlock_found ();
    Deferred.unit
  in
  let rec loop () =
    Clock.after deadlock_watch_period >>= fun () ->
    if (!fin) then Deferred.unit else (
      let cycle_path_opt = look_for_a_cycle dg in
      match cycle_path_opt with
      | Some cycle_path_with_repeated_nub ->
        begin
          Message.error "Cycle found. Quitting.";
          message_cycle_path dg ~cycle_path_with_repeated_nub;
          quit()
        end
      | None ->
        loop()
    )
  in
  loop ()

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

let gc_full_major_and_show_stats =
  fun () ->
    Gc.full_major(); (* might run finalizers, so.. *)
    Gc.full_major(); (* GC again *)
    (*Message.message "Live(Kb-delta) %d #HeartWatching %d"
      (live_kb_delta ())
      (Tenacious.Heart.Watching.count())*)
    ()

(*----------------------------------------------------------------------
  build_once
----------------------------------------------------------------------*)

let compact_zero_overhead () =
  let prev_space_overhead = (Gc.get()).Gc.Control.space_overhead in
  Gc.tune ~space_overhead:0 ();
  Gc.compact();
  Gc.tune ~space_overhead:prev_space_overhead ()

let build_once :
    (Config.t -> DG.t -> Progress.t -> unit Builder.t ->
     save_db_now:(unit -> unit Deferred.t) ->
     (Heart.t * int) Deferred.t) =
  (* Make a single build using the top level tenacious builder
     Where a single build means we've done all we can
     (maybe we are complete, or maybe some targets are in error)
     given the current state of the file-system.
     And now we are just polling of file-system changes.
  *)
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun config __dg progress top_builder ~save_db_now ->
    let u = genU() in (* count of the times we reach polling *)
    let start_time = Time.now() in

    let top_tenacious = Builder.expose top_builder in

    Tenacious.exec top_tenacious >>= fun (ore,heart) ->

    (* to avoid reporting of stale errors etc... *)
    (*Progress.mask_unreachable progress dg;*)

    let errors, needs_in_error =
      match ore with
      | Ok () -> ([], Progress.Need.Set.empty)
      | Error problem -> (Problem.reasons problem, Problem.needs_in_error problem)
    in

    let is_reachable_error need = Set.mem needs_in_error need in
    Progress.mask_unreachable progress ~is_reachable_error;

    let duration = Time.diff (Time.now()) start_time in

    save_db_now() >>| fun () ->

    let snap = Progress.snap progress in

    let () =
      (* NO point doing the GC unless we are in polling mode *)
      if Config.poll_forever config then (
        gc_full_major_and_show_stats();
        let percentage_live =
          let stat = Gc.stat () in
          let live = stat.Gc.Stat.live_words in
          let heap = stat.Gc.Stat.heap_words in
          Float.to_int (100. *. float live /. float heap)
        in
        if Int.(percentage_live < 80) then (
          Message.message "GC.compacting...";
          compact_zero_overhead();
        )
      )
    in

    let effort_string = Progress.Snap.to_effort_string snap in

    let exit_code =
      if Progress.Snap.no_errors snap then (
        let built = Progress.Snap.built snap in
        Message.build_done ~duration ~u ~total:built effort_string;
        Exit_code.build_done
      ) else (
        let fraction = Progress.Snap.fraction snap in
        Message.build_failed ~duration ~u ~fraction effort_string;
        Exit_code.build_failed
      )
    in
    Message.message "%s" (Progress.Snap.to_string snap `jem_style);

    List.iter errors ~f:(fun (reason, stack_trace) ->
      Reason.message_summary
        config
        ~need:(match List.last stack_trace with
          | None -> ""
          | Some n -> Progress.Need.to_string n)
        reason;
      (if Config.show_error_dependency_paths config
       then
         List.iter (List.rev stack_trace) ~f:(fun need ->
           Message.message "Wanted by %s" (Progress.Need.to_string need)
         )));
    heart, exit_code

(*----------------------------------------------------------------------
  entry point -- build_forever
----------------------------------------------------------------------*)

let run_user_function_from_env_opt env_opt tag ~f =
  match env_opt with
  | None -> Deferred.unit
  | Some env ->
    Monitor.try_with (fun () ->
      f env ()
    ) >>= function
    | Ok () -> Deferred.unit
    | Error exn ->
      let exn = Monitor.extract_exn exn in
      Message.error "%s: threw exception:\n%s" tag (Exn.to_string exn);
      Deferred.unit


let build_forever =
  (* co-ordinate the build-forever process *)

  fun config progress ~jr_spec ~top_level_demands fs persist
    ~save_db_now ~when_rebuilding ->

    let memo = Memo.create () in
    let discovered_graph = DG.create config in

    let dg_root = DG.create_root discovered_graph in

    let deadlock_found = Ivar.create () in
    let () =
      don't_wait_for (
        Ivar.read deadlock_found >>= fun () ->
        Quit.quit Exit_code.cycle_abort;
        Deferred.never()
      )
    in

    let get_env_opt =
      get_env_option
        ~jr_spec
        fs persist memo discovered_graph ~dg_root
        config progress
    in

    let rec build_and_poll ()  = (* never finishes if polling *)

      Progress.reset_effort();

      (* start up various asyncronous writers/dumpers *)
      let fin = ref false in

      (* deadlock/cycle detection *)
      don't_wait_for (
        watch_for_deadlock ~deadlock_found ~fin discovered_graph
      );

      show_progress_reports config ~fin progress;

      let top_builder =
        Builder.all_unit (
          List.map top_level_demands ~f:(fun demanded ->
            build_one_root_goal
              ~jr_spec
              fs persist memo discovered_graph ~dg_root
              config progress ~demanded
          )
        )
      in

      Tenacious.exec get_env_opt >>= fun (env_opt,__heart) ->

      (* call user build_begin function *)
      run_user_function_from_env_opt env_opt "build_begin" ~f:Env.build_begin >>= fun () ->

      (* do the build once *)
      build_once config discovered_graph progress top_builder ~save_db_now >>= fun (heart,exit_code) ->

      (* call user build_end function *)
      run_user_function_from_env_opt env_opt "build_end" ~f:Env.build_end >>= fun () ->

      fin := true;

      match Config.poll_forever config with
      | false ->
        Message.message "build finished; not in polling mode so quitting";
        Quit.quit exit_code;
        Deferred.never()
      | true ->
        (* -P *)
        Message.polling ();
        (* wait here until something changes on the file-system *)
        let wait = Heart.when_broken heart in
        exit_code_upon_control_c := exit_code;
        wait >>= fun () ->
        exit_code_upon_control_c := Exit_code.incomplete;
        Message.rebuilding ();
        when_rebuilding() >>= fun () ->
        build_and_poll ()

    in
    build_and_poll ()
