
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

module Heart = Tenacious.Heart

let equal_using_compare compare = fun x1 x2 -> Int.(=) 0 (compare x1 x2)

module Target_rule = Rule.Target_rule
module Generator = Description.Generator
module Scheme = Description.Scheme

module Digest = Fs.Digest
module Glob = Fs.Glob
module DG = Discovered_graph

let exit_code_upon_control_c = ref Exit_code.incomplete

(*----------------------------------------------------------------------
 stale targets
----------------------------------------------------------------------*)

let remove_stale_artifacts ~dir ~stale =
  match stale with [] -> return () | _::_ ->
    Deferred.List.iter stale ~f:(fun path ->
      let path_string = Path.Rel.to_string path in
      (* Dont remove non-local stale build artifcats
         We no longer allow (api.ml) rules to be created with non-local targets.
         But jenga.db might have been created by ealier version of jenga which did.
      *)
      if not (Path.Rel.(dirname path = dir)) then (
        (*Message.message "Not removing stale build artifact in non-local directory: %s"
          path_string;*)
        return ()
      ) else
      try_with (fun () -> Sys.remove path_string) >>= function
      | Ok () ->
        Message.message "Removed stale build artifact: %s" path_string;
        return ()
      | Error _ ->
        try_with (fun () -> Sys.file_exists path_string) >>= function
        | Error _ ->
          Message.error "Sys.file_exists failed: %s" path_string;
          return ()
        | Ok res -> match res with
          | `Unknown
          | `Yes ->
            (* looks like the remove failed *)
            Message.error "Sys.remove failed: %s" path_string;
            return ()
          | `No ->
            (* looks like file was not there - a target we had never built *)
            return ()
    )

(*----------------------------------------------------------------------
 putenv_for_path
----------------------------------------------------------------------*)

let putenv_for_path =
  (* lookup once and remember to avoid repeated pushing on to the front *)
  let orig_path = match (Core.Std.Sys.getenv "PATH") with | None -> "" | Some s -> s in

  let minimal_path_kept_even_after_replacement = "/bin:/usr/bin:/usr/local/bin" in
  (* Without this minimal path, user replacement can terminally break Jenga!
     For example, the ocaml_plugin module replies on the path to find "objcopy"
  *)
  let colonize xs = String.concat (List.map xs ~f:(fun x -> x ^ ":")) in
  fun spec ->
    let replacement_path  =
      match spec with
      | `Extend xs -> colonize xs ^ orig_path
      | `Replace xs -> colonize xs ^ minimal_path_kept_even_after_replacement
    in
    replacement_path

(*----------------------------------------------------------------------
 new - Env1
----------------------------------------------------------------------*)

type scheme_id = string with sexp, bin_io

module Env1 : sig (* TODO: move into [description.ml] *)

  type t
  val of_env : Env.t ->  [ `ok of t | `dups of scheme_id list ]
  val putenv : t -> (string * string) list
  val lookup_gen_key : t -> Goal.t -> Gen_key.t option
  val gen_keys_for_directory : t -> dir:Path.Rel.t -> Gen_key.t list
  val run_generator_scheme : t -> Gen_key.t -> (Generator.t,exn) Result.t
  val build_begin : t -> unit Deferred.t
  val build_end : t -> unit Deferred.t

end = struct

  type t = {
    putenv : (string * string) list;
    lookup_gen_key : Goal.t -> Gen_key.t option;
    gen_keys_for_directory : dir:Path.Rel.t -> Gen_key.t list;
    run_generator_scheme : Gen_key.t -> (Generator.t,exn) Result.t;
    build_begin : unit -> unit Deferred.t;
    build_end : unit -> unit Deferred.t;
  } with fields

  let build_begin t = t.build_begin ()
  let build_end t = t.build_end ()

  let of_env env =
    let {Env. putenv; command_lookup_path;
         build_begin; build_end;
         schemes} = Env.rep env in

    (* Each time we construct the Env1 form the Env, we reset the command lookup path.

       It's kind of odd being here, but the env->env1 conversion is done just once when
       the JR.ml is changed by the user, and this is the only time the command_lookup_path
       might get changed.
    *)
    let putenv = putenv @
      match command_lookup_path with
      | None -> []
      | Some spec -> [("PATH",putenv_for_path spec)]
    in

    (* manipulate the user schemes list into the two lookup stages required
       (1) Goal.t -> Gen_key.t option;
       (2) Gen_key.t -> Generator.t;

       The first stage matches the patterns in order until a match is found, return the
       gen_key constructed from the tag of the scheme-found & the dir of the goal

       The second stage consults a pre-build HT from the scheme-tag to the scheme-body.
       We deconstruct the gen-key back to the scheme-tag & dir.
       We lookup the HT using the scheme-tag. This cant fail!
       The scheme-body is then applied to the dir to get the generator.

       When pre-constructing the HT, we check for duplicate schemes with the same tag
       - this is the reason for the ref on the scheme body -
       and we return an error-indication of this is so.
    *)

    (* stage 1 *)
    let lookup_gen_key goal =
      let key_string = Goal.to_string goal in
      match
        match (
          List.find schemes ~f:(fun (pat,_opt_scheme) ->
            Pattern.matches pat key_string
          )
        ) with
        | None -> None
        | Some (_,opt_scheme) -> opt_scheme
      with
      | None -> None
      | Some scheme ->
        Some (Gen_key.create ~tag:(Scheme.tag scheme) ~dir:(Goal.directory goal))
    in

    let gen_keys_for_directory ~dir =
      List.filter_map schemes ~f:(fun (_,scheme_opt) ->
        match scheme_opt with
        | None -> None
        | Some scheme ->
          Some (Gen_key.create ~tag:(Scheme.tag scheme) ~dir)
      )
    in

    (*let lookup_gen_key goal =
      let opt_gen_key = lookup_gen_key goal in
      Message.message "lookup_gen_key: %s -> %s"
        (Goal.to_string goal)
        (match opt_gen_key with
        | None -> "None"
        | Some gen_key -> Gen_key.to_string gen_key);
      opt_gen_key
    in*)

    (* stage 2 *)
    let dups = ref [] in
    let push_dups tag = (dups := tag :: !dups) in
    let h_schemes = String.Table.create () in
    let () =
      List.iter schemes ~f:(fun (_pat,scheme_opt) ->
        match scheme_opt with
        | None -> ()
        | Some scheme ->
          let tag = Scheme.tag scheme in
          let body = Scheme.body scheme in
          match (Hashtbl.find h_schemes tag) with
          | Some prev -> if phys_equal prev body then () else push_dups tag
          | None -> Hashtbl.set h_schemes ~key:tag ~data:body
      )
    in
    let run_generator_scheme gen_key =
      let {Gen_key. tag; dir} = gen_key in
      match (Hashtbl.find h_schemes tag) with
      | None -> failwith "run_generator_scheme" (* cant happen? *)
      | Some scheme_body ->
        try (
          let generator = (!scheme_body) ~dir in
          Ok generator
        )
        with exn -> Error exn
    in

    let dups = !dups in
    match dups with | _::_ -> `dups dups
    | [] ->
      `ok {
        putenv;
        lookup_gen_key;
        gen_keys_for_directory;
        run_generator_scheme;
        build_begin;
        build_end;
      }

end


(*----------------------------------------------------------------------
  Ruleset
----------------------------------------------------------------------*)

module Ruleset : sig

  type t
  val create : Rule.t list -> [ `ok of t | `dups of Path.Rel.t list ]

  val lookup_target : t -> Path.Rel.t -> Target_rule.t option
  val lookup_alias : t -> Alias.t -> unit Dep.t option

  val targets : t -> Path.Rel.Set.t

end = struct

  type t = {
    rules  : Rule.t list;
    lookup_target : Path.Rel.t -> Target_rule.t option;
    lookup_alias : Alias.t -> unit Dep.t option;
  } with fields

  let targets t =
    Path.Rel.Set.of_list (List.concat_map (rules t) ~f:Rule.targets)

  let create rules =
    let by_target = Path.Rel.Table.create () in
    let by_alias = Alias.Table.create () in
    let dups = ref [] in
    let () =
      List.iter rules ~f:(fun rule ->
        match rule with
        | Rule.Target tr ->
          List.iter (Target_rule.targets tr) ~f:(fun path ->
            match (Hashtbl.add by_target ~key:path ~data:tr) with
            | `Ok -> ()
            | `Duplicate -> dups := path :: !dups
          )
        | Rule.Alias (alias,depends) ->
          let depends =
            match (Hashtbl.find by_alias alias) with
            | None -> depends
            | Some prev ->
              (* merge aliases *)
              Dep.Bind (
                Dep.All [depends; prev], (fun _ -> Dep.Return ())
              )
          in
          Hashtbl.set by_alias ~key:alias ~data:depends

      )
    in
    let dups = !dups in
    match dups with
    | _::_ -> `dups dups
    | [] ->
      let lookup_target = Hashtbl.find by_target in
      let lookup_alias = Hashtbl.find by_alias in
      `ok {
        rules;
        lookup_target;
        lookup_alias;
      }

end

(*----------------------------------------------------------------------
 Pm_key - path or glob
----------------------------------------------------------------------*)

module Pm_key : sig

  type t with sexp, bin_io, compare
  include Comparable_binable with type t := t

  val equal : t -> t -> bool
  val of_abs_path : Path.Abs.t -> t
  val of_rel_path : Path.Rel.t -> t
  val of_path : Path.t -> t
  val of_glob : Glob.t -> t
  val to_string : t -> string
  val to_path_exn : t -> Path.Rel.t (* for targets_proxy_map *)

end = struct

  module T = struct
    type t = Path of Path.t | Glob of Glob.t
    with sexp, bin_io, compare
  end
  include T
  include Comparable.Make_binable(T)

  let equal = equal_using_compare compare

  let of_path x = Path x
  let of_abs_path x = Path (Path.of_absolute x)
  let of_rel_path x = Path (Path.of_relative x)
  let of_glob x = Glob x

  let to_string = function
    | Path path -> Path.to_string path
    | Glob glob -> Glob.to_string glob

  let to_path_exn = function
    | Glob _ -> failwith "Proxy_map.key.to_path_exn/Glob"
    | Path x ->
      match Path.case x with
      | `absolute _ -> failwith "Proxy_map.key.to_path_exn/Abs"
      | `relative path -> path

end

(*----------------------------------------------------------------------
  proxies
----------------------------------------------------------------------*)

module Proxy : sig

  type t with sexp, bin_io, compare
  val of_digest : Digest.t -> t
  val of_listing : Fs.Listing.t -> t
  val equal : t -> t -> bool

end = struct

  type t = Digest of Digest.t | Fs_proxy of Fs.Listing.t with sexp, bin_io, compare
  let of_digest x = Digest x
  let of_listing x = Fs_proxy x

  let equal = equal_using_compare compare

end

module PPs = struct (* list of path-tagged proxies *)
  type t = (Path.t * Proxy.t) list
end

module Proxy_map : sig (* need to be keyed on Path/Glob *)

  type t with sexp, bin_io, compare

  val empty  : t
  val single : Pm_key.t -> Proxy.t -> t

  type inconsistency = (Pm_key.t * Proxy.t list) list
  with sexp_of

  val create_by_path : PPs.t -> [`ok of t | `err of inconsistency]
  val merge : t list -> [ `ok of t | `err of inconsistency]

  val equal : t -> t -> bool
  val diff : before:t -> after:t -> Pm_key.t list option

end = struct

  type t = Proxy.t Pm_key.Map.t with sexp, bin_io, compare

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

end

module Rule_proxy = struct

  type t = {
    targets : Proxy_map.t;
    deps : Proxy_map.t;
    action : Job.t
  } with sexp, bin_io, compare, fields

end

module Output_proxy = struct

  type t = {
    deps : Proxy_map.t;
    stdout : string;
  } with sexp, bin_io, compare, fields

end

module Problem : sig

  type t

  val reasons : t -> Reason.t list
  val needs_in_error : t -> Progress.Need.Set.t

  val create : Reason.t -> t
  val all : t list -> t
  val subgoal : Progress.Need.t -> t -> t

end = struct

  type t = {
    reasons : Reason.t list;
    needs_in_error : Progress.Need.Set.t;
  } with fields

  let create r = {
    reasons = [r];
    needs_in_error = Progress.Need.Set.empty;
  }

  let all ts = {
    reasons = List.concat_map ts ~f:reasons;
    needs_in_error = Progress.Need.Set.union_list (List.map ts ~f:needs_in_error);
  }

  let subgoal need t = {
    reasons = [];
    needs_in_error = Progress.Need.Set.add t.needs_in_error need;
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
  val map : 'a t -> ('a -> 'b) -> 'b t

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

  val before_redo : 'a t -> f:(unit -> unit) -> 'a t

  val with_ore : 'a t -> f:(('a, Problem.t) Result.t -> unit) -> 'a t

  val uncancellable : 'a t -> 'a t

end = struct

  type 'a t = ('a, Problem.t) Result.t Tenacious.t

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

  let return x = Tenacious.return (Ok x)

  let bind t f =
    Tenacious.bind t (function
    | Error e -> Tenacious.return (Error e)
    | Ok x -> f x
    )

  let map t f =
    Tenacious.map t ~f:(function
    | Error e -> Error e
    | Ok x -> Ok (f x)
    )

  let error reason =
    Tenacious.return (Error (Problem.create reason))

  let all xs =
    Tenacious.map (Tenacious.all xs) ~f:(fun ys ->
      let rec collect probs oks = function
        | Ok x :: xs -> collect probs (x::oks) xs
        | Error p :: xs -> collect (p::probs) oks xs
        | [] ->
          match probs with
          | [] -> Ok (List.rev oks)
          | _::_ -> Error (Problem.all probs)
      in
      collect [] [] ys
    )

  let all_unit ts = map (all ts) (fun (_:unit list) -> ())

  let subgoal need builder =
    Tenacious.map builder ~f:(function
    | Ok x -> Ok x
    | Error problem -> Error (Problem.subgoal need problem)
    )

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

  let before_redo t ~f = reify (Tenacious.before_redo t ~f)

  let with_ore builder ~f =
    wrap (
      Tenacious.map (expose builder) ~f:(fun ore -> f ore; ore)
    )

  let uncancellable builder = Tenacious.uncancellable builder

  let ( *>>| ) = map
  let both : ('a t -> 'b t -> ('a * 'b) t) =
    fun a b ->
      all [
        (a *>>| fun a -> `a a);
        (b *>>| fun b -> `b b);
      ] *>>| function
      | [`a a; `b b] -> (a,b)
      | _ -> assert false

end

let return   = Builder.return
let ( *>>= ) = Builder.bind
let ( *>>| ) = Builder.map
let error    = Builder.error

let build_all_in_sequence xs ~f = (* stopping on first error *)
  let rec loop acc = function
    | [] -> return (List.rev acc)
    | x::xs -> f x *>>= fun v -> loop (v::acc) xs
  in
  loop [] xs

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
  Persist - static cache - saved to file between runs
----------------------------------------------------------------------*)

module Persist = struct

  (* The values in these hashtable must be sexp convertable,
     so they can be saved to file.  *)

  type t = {
    generated   : Path.Rel.Set.t Gen_key.Table.t;
    ruled       : Rule_proxy.t Path.Rel.Table.t; (* actions run for target-rules *)
    actioned    : Output_proxy.t Job.Table.t; (* actions run for their stdout *)
  } with sexp, bin_io

  let create () = {
    generated = Gen_key.Table.create();
    ruled = Path.Rel.Table.create();
    actioned = Job.Table.create();
  }

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
    generating : (Ruleset.t Builder.t * DG.Node.t) Gen_key.Table.t;
    reflecting : (Reflected.Trip.t option Builder.t * DG.Node.t) Path.Table.t;
    building  : (Proxy_map.t Builder.t * DG.Node.t) Goal.Table.t;
    root : Env1.t Builder.t option ref;
  }

  let create () = {
    generating = Gen_key.Table.create();
    reflecting = Path.Table.create();
    building = Goal.Table.create();
    root = ref None;
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
  memo : Memo.t; (* dynmaic cache *)
  progress : Progress.t; (* track state of each goal being built *)
  jr_spec : Jr_spec.t; (* access to the jengaroot *)

  (* recursive calls to build_goal go via here - avoids having big mutual let rec *)
  recurse_build_goal : (t -> Goal.t -> Proxy_map.t Builder.t);
  recurse_reflect_path : (t -> Path.t -> Reflected.Trip.t option Builder.t);
  recurse_reflect_alias : (t -> Alias.t -> Path.Set.t Builder.t);
  recurse_buildable_targets : (t -> dir:Path.t -> Path.Set.t Builder.t);

  (* discovered_graph structure & current node - used for cycle checking *)
  discovered_graph : DG.t;
  node : DG.Node.t;

} with fields

let generated t = t.persist.Persist.generated
let ruled t = t.persist.Persist.ruled
let actioned t = t.persist.Persist.actioned

let memo_generating t = t.memo.Memo.generating
let memo_reflecting t = t.memo.Memo.reflecting
let memo_building t = t.memo.Memo.building
let memo_root t = t.memo.Memo.root

let build_sub_goal : (t ->  Goal.t -> Proxy_map.t Builder.t) =
  fun t goal -> t.recurse_build_goal t goal

let run_user_code f =
  Builder.of_deferred (fun () ->
    Monitor.try_with f
  ) *>>= function
  | Ok x -> return x
  | Error exn ->
    let exn = Monitor.extract_exn exn in
    error (Reason.Usercode_raised exn)

let run_rhs_of_bind f =
  match try (Ok (f())) with | exn -> Error exn with
  | Error exn -> error (Reason.Usercode_raised exn)
  | Ok right -> return right

(*----------------------------------------------------------------------
  File system interface
----------------------------------------------------------------------*)

let get_contents
    : (t -> Path.t -> string Builder.t) =
  fun t file ->
    Builder.of_tenacious (Fs.contents_file t.fs ~file)
    *>>= function
    | `file_read_error e    -> error (Reason.File_read_error e)
    | `is_a_dir             -> error (Reason.Unexpected_directory file)
    | `contents s           -> return s

let digest_path
    : (t -> Path.t -> [ `file of Digest.t | `missing | `is_a_dir ] Builder.t) =
  fun t path ->
    Builder.of_tenacious (Fs.digest_file t.fs ~file:path)
    *>>= function
    | `stat_error _   -> return `missing
    | `is_a_dir       -> return `is_a_dir
    | `undigestable k -> error (Reason.Undigestable k)
    | `digest_error e -> error (Reason.Digest_error e)
    | `digest digest  -> return (`file digest)

let look_for_source : (t -> Path.t -> Proxy.t option Builder.t) =
  fun t path ->
    digest_path t path *>>= function
    | `file digest    -> return (Some (Proxy.of_digest digest))
    | `missing        -> return None
    | `is_a_dir       -> return None

let need_glob : (t -> Glob.t -> Fs.Listing.t Builder.t) =
  fun t glob ->
    let err s = Reason.Glob_error (glob,s) in
    Builder.of_tenacious (Fs.list_glob t.fs glob) *>>= function
    | `stat_error _   -> error (err "stat error")
    | `not_a_dir      -> error (err "not a directory")
    | `listing_error _-> error (err "unable to list")
    | `listing listing -> return listing

let ensure_directory : (t -> dir:Path.Rel.t -> unit Builder.t) =
  fun t ~dir ->
    Builder.of_tenacious (Fs.ensure_directory t.fs ~dir:(Path.of_relative dir))
    *>>= function
    | `ok -> return ()
    | `not_a_dir -> error (Reason.No_directory_for_target "not a directory")
    | `failed -> error (Reason.No_directory_for_target "failed to create")

(*----------------------------------------------------------------------
  running things - generator, scanners, actions
----------------------------------------------------------------------*)

let run_action_with_message :
    (t -> Env1.t -> Action.t -> message:(unit->unit) -> need:string ->
     output:'a Job.Output.t -> 'a Builder.t) =
  fun t env1 action ~message ~need ~output ->
    let progress = t.progress in
    let config = t.config in
    let putenv = Env1.putenv env1 in
    Builder.of_deferred (fun () ->
      Action.run action ~message ~output ~putenv ~progress ~config ~need
    ) *>>= function
    | Ok x                                -> return x
    | Error (`non_zero_status output)     -> error (Reason.Non_zero_status output)
    | Error (`other_error Job.Shutdown)   -> error Reason.Shutdown
    | Error (`other_error exn)            -> error (Reason.Running_job_raised exn)

let run_action_for_targets :
    (t -> RR.t -> Env1.t -> Action.t -> Job.t ->
     targets:Path.Rel.t list -> need:string ->
     unit Builder.t) =
  (* After running an action, synchronise until all inotify events triggered while the
     action was run have been delivered (and acted upon) by this process *)
  fun t rr env1 action job ~targets ~need ->
    let message() =
      if Config.show_actions_run t.config then
        Message.message "Building: %s [%s]"
          (String.concat ~sep:" " (List.map targets ~f:Path.Rel.to_string))
          (RR.to_string t.config rr)
    in
    let sync_contents = Job.string_for_sh job in
    Builder.wrap (
      Fs.sync_inotify_delivery t.fs ~sync_contents (
        Builder.expose (
          run_action_with_message t env1 action ~message ~need
            ~output:Job.Output.ignore
        )
      )
    )

let run_action_for_stdout :
    (t -> RR.t -> Env1.t -> Action.t -> Job.t -> string Builder.t) =
  fun t rr env1 action job ->
    let message() =
      if Config.show_actions_run t.config then
        Message.message "Action: %s [%s]"
          (Job.string_for_sh job)
          (RR.to_string t.config rr)
    in
    run_action_with_message t env1 action ~message ~need:"stdout"
      ~output:Job.Output.stdout

(*----------------------------------------------------------------------
 report errors / record status
----------------------------------------------------------------------*)

let set_status t need status =
  Progress.set_status t.progress need status

let report_error_for_need t need reason =
  let show_now =
    match reason with
    | Reason.Non_zero_status _(* we see the error message from the command *)
    | Reason.Shutdown
        -> false
    | _
      -> true
  in
  if show_now then (
    let tag = Progress.Need.to_string need in
    Reason.messages ~tag reason;
  );
  if Config.stop_on_first_error t.config && not (Reason.filesystem_related reason) then (
    Quit.quit Exit_code.build_failed;
  )

let report_status t need ore =
  match ore with
  | Ok _ ->
    set_status t need Progress.Status.Built
  | Error problem -> (
    let reasons = Problem.reasons problem in
    List.iter reasons ~f:(fun reason ->
      report_error_for_need t need reason
    );
    set_status t need (Progress.Status.Error reasons)
  )

let builder_report_for_need t need builder =
  Builder.with_ore builder ~f:(report_status t need)


let build_considering_needed : (t -> Progress.Need.t -> 'a Builder.t -> 'a Builder.t) =
  (* Expose the builder's result/error for reporting *)
  fun t need builder ->
    Builder.subgoal need (
      builder_report_for_need t need (
        (* Report considering/re-considering; count as: check/recheck *)
        if Config.show_considering t.config
        then (
          Message.message "Considering: %s" (Progress.Need.to_string need);
        );
        set_status t need Progress.Status.Todo;
        let builder =
          builder *>>| fun res ->
          let () = Effort.incr Progress.considerations_run in
          res
        in
        Builder.before_redo builder ~f:(fun () ->
          if Config.show_considering t.config || Config.show_reconsidering t.config
          then  (
            Message.message "Re-considering: %s" (Progress.Need.to_string need)
          );
          set_status t need Progress.Status.Todo;
        )
      )
    )


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
        let info = Info.of_string ("Description.convert_using_reader "
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
    need_glob t glob *>>| fun listing ->
    match Fs.Listing.paths listing with
    | [] -> false
    | _::_ -> true

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
    let badly_suffixed_modules =
      List.filter_map (Jenga_conf_rep.modules jc) ~f:(fun m ->
        match (String.lsplit2 ~on:'.' m) with
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
        List.map (Jenga_conf_rep.modules jc) ~f:(fun s ->
          match String.lsplit2 ~on:'.' s with | Some (s,_) -> s | None -> s)
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
              Path.relative ~dir:(Path.dirname conf) (m ^ suf))
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
      let conf = Path.of_relative (Path.Rel.root_relative Misc.jenga_conf_basename) in
      let root_ml = Path.of_relative (Path.Rel.root_relative Misc.jenga_root_basename) in
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

let jenga_root : (t -> Env1.t Builder.t) =
  (* wrap up the call to [Load_root.get_env] into a tenacious builder,
     which will reload any time the jengaroot is modified *)
  fun t ->
    jenga_load_spec t t.jr_spec *>>= fun load_spec ->
    Builder.of_deferred (fun () -> Load_root.get_env load_spec) *>>= function
    | Error e ->
      let exn = Monitor.extract_exn (Error.to_exn e) in
      error (Reason.Usercode_raised exn)
    | Ok env ->
      match (Env1.of_env env) with
      | `dups xs -> error (Reason.Duplicate_scheme_ids xs)
      | `ok env1 -> return env1

let jenga_root : (t -> Env1.t Builder.t) =
  fun t ->
    build_considering_needed t Progress.Need.jengaroot (
      jenga_root t
    )

let jenga_root : (t -> Env1.t  Builder.t) =
  (* Memoization of jenga_root is simpler that other cases, becasuse:
     - There is only one; we don't need a hashtable, just a ref.
     - The root has no deps, so there is no chance of cycles,
  *)
  fun t ->
    match !(memo_root t) with
    | Some builder  -> builder
    | None ->
      let builder = Builder.reify (jenga_root t) in
      memo_root t := Some builder;
      builder


(*----------------------------------------------------------------------
  build
----------------------------------------------------------------------*)

let share_and_check_for_cycles :
    (* memoize / check for cycles...
       Three places where this function is called, wraps for:
       - build_goal
       - generate_ruleset
       - reflect_path
       Which are the places in the build description where sharing can be encountered
       For each sharing point we must ensure that only one builder computation is setup
    *)
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
        Builder.before_redo builder
          ~f:(fun () ->
            DG.remove_all_dependencies t.discovered_graph t.node;
          )
      in
      let builder = Builder.reify builder in
      (* we use add_exn, because each key will be memoized exactly once *)
      Hashtbl.add_exn memo ~key ~data:(builder,t.node);
      builder


let run_action_for_stdout_if_necessary
    : (t -> deps:Proxy_map.t -> Action.t -> string Builder.t) =
  fun t ~deps action ->
    jenga_root t *>>= fun env1 ->
    let job = Action.job action in
    let run_and_cache rr =
      run_action_for_stdout t rr env1 action job *>>= fun stdout ->
      let output_proxy = {Output_proxy. deps; stdout;} in
      Hashtbl.set (Misc.mod_persist (actioned t)) ~key:job ~data:output_proxy;
      return stdout
    in
    match (Hashtbl.find (actioned t) job) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some prev ->
        match (Proxy_map.diff ~before:prev.Output_proxy.deps ~after:deps) with
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
      let pm = Proxy_map.single (Pm_key.of_abs_path abs) proxy in
      return pm

let need_path : (t -> Path.t -> Proxy_map.t Builder.t) =
  fun t path ->
    match Path.case path with
    | `relative path -> build_sub_goal t (Goal.Path path)
    | `absolute path -> need_abs_path t path

let build_merged_proxy_maps : (Proxy_map.t list -> Proxy_map.t Builder.t) =
  fun pms ->
    match (Proxy_map.merge pms) with
    | `err _inconsistency -> error Reason.Inconsistent_proxies
    | `ok pm -> return pm

let build_depends : (t -> 'a Dep.t -> ('a * Proxy_map.t) Builder.t) =
  fun t depends ->
    let rec exec : type a. a Dep.t -> (a * Proxy_map.t) Builder.t = function

      | Dep.Return x ->
        return (x, Proxy_map.empty)

      | Dep.Bind (left,f_right) ->
        exec left *>>= fun (v1,pm1) ->
        run_rhs_of_bind (fun () -> f_right v1) *>>= fun right ->
        exec right *>>= fun (v2,pm2) ->
        build_merged_proxy_maps [pm1;pm2] *>>= fun pm ->
        return (v2, pm)

      | Dep.Cutoff (equal,body) ->
        Builder.cutoff
          ~equal:(fun (x1,pm1) (x2,pm2) -> equal x1 x2 && Proxy_map.equal pm1 pm2)
          (exec body)

      | Dep.All xs ->
        Builder.all (List.map ~f:exec xs) *>>= fun xs ->
        let vs,pms = List.unzip xs in
        build_merged_proxy_maps pms *>>= fun pm ->
        return (vs,pm)

      | Dep.Deferred f ->
        run_user_code f *>>| fun v ->
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
          | `relative path -> build_sub_goal t (Goal.Path path) *>>| fun _ignored_pm -> ()
          | `absolute _ -> return ()
        end *>>= fun () ->
        let pm  = Proxy_map.empty in
        get_contents t path *>>| fun contents ->
        (contents, pm)

      | Dep.Glob_listing glob ->
        (* CARRY the glob listing; dont put into the proxy-map *)
        need_glob t glob *>>| fun listing ->
        let pm  = Proxy_map.empty in
        (Fs.Listing.paths listing, pm)

      | Dep.Glob_change glob ->
        (* Dont carry the glob listing; do put into the PROXY-MAP *)
        need_glob t glob *>>| fun listing ->
        let pm = Proxy_map.single (Pm_key.of_glob glob) (Proxy.of_listing listing) in
        ((), pm)

      | Dep.Action_stdout action_depends ->
        exec action_depends *>>= fun (action,pm) ->
        run_action_for_stdout_if_necessary t ~deps:pm action *>>| fun stdout ->
        (stdout, Proxy_map.empty)

      | Dep.Reflect_path path ->
        t.recurse_reflect_path t path *>>| fun x ->
        (x,Proxy_map.empty)

      | Dep.Reflect_alias alias ->
        t.recurse_reflect_alias t alias *>>| fun x ->
        (x,Proxy_map.empty)

      | Dep.Reflect_putenv ->
        jenga_root t *>>| fun env1 ->
        (Env1.putenv env1, Proxy_map.empty)

      | Dep.Buildable_targets dir ->
        t.recurse_buildable_targets t ~dir *>>| fun paths ->
        (paths, Proxy_map.empty)

    in
    exec depends

let reflect_depends : (t -> 'a Dep.t -> ('a * Path.Set.t) Builder.t) =
  fun t depends ->
    let rec exec : type a. a Dep.t -> (a * Path.Set.t) Builder.t = function

      | Dep.Return x ->
        return (x, Path.Set.empty)

      | Dep.Bind (left,f_right) ->
        exec left *>>= fun (v1,set1) ->
        run_rhs_of_bind (fun () -> f_right v1) *>>= fun right ->
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
        run_user_code f *>>| fun v ->
        (v, Path.Set.empty)

      | Dep.Alias alias ->
        t.recurse_reflect_alias t alias *>>| fun set ->
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
          | `relative path -> build_sub_goal t (Goal.Path path) *>>| fun __pm -> ()
          | `absolute _ -> return ()
        end *>>= fun () ->
        get_contents t path *>>| fun contents ->
        (contents, Path.Set.empty)

      | Dep.Glob_listing glob ->
        (* CARRY the glob listing; dont put into the proxy-map *)
        need_glob t glob *>>| fun listing ->
        (Fs.Listing.paths listing, Path.Set.empty)

      | Dep.Glob_change _ ->
        return ((), Path.Set.empty)

      | Dep.Action_stdout action_depends ->
        (* reflect causes building here by calling [build_depends] *)
        build_depends t action_depends *>>= fun (action,set) ->
        run_action_for_stdout_if_necessary t ~deps:set action *>>| fun stdout ->
        (stdout, Path.Set.empty)

      | Dep.Reflect_path path ->
        t.recurse_reflect_path t path *>>| fun x ->
        (x,Path.Set.empty)

      | Dep.Reflect_alias alias ->
        t.recurse_reflect_alias t alias *>>| fun x ->
        (x,Path.Set.empty)

      | Dep.Reflect_putenv ->
        jenga_root t *>>| fun env1 ->
        (Env1.putenv env1, Path.Set.empty)

      | Dep.Buildable_targets dir  ->
        t.recurse_buildable_targets t ~dir *>>| fun paths ->
        (paths, Path.Set.empty)

    in
    exec depends

let generate_ruleset : (t -> Gen_key.t -> Ruleset.t Builder.t) =
  (* Run rule generation (for a gen_key); always
     Record the generated targets in the persistent state,
     to allow stale targets to be removed later.
  *)
  fun t gen_key ->
    jenga_root t *>>= fun env1 ->
    match (Env1.run_generator_scheme env1 gen_key) with
    | Error exn -> error (Reason.Scheme_raised exn)
    | Ok generator ->
      build_depends t (Generator.run generator) *>>= fun (rules,__proxy_map) ->
      begin
        match (Ruleset.create rules) with
        | `ok ruleset -> return ruleset
        | `dups paths -> error (Reason.Multiple_rules_for_paths (gen_key,paths))
      end
      *>>= fun ruleset ->
      let prev_targets =
        match (Hashtbl.find (generated t) gen_key) with
        | Some x -> x
        | None -> Path.Rel.Set.empty
      in
      let targets = Ruleset.targets ruleset in
      let stale = Set.to_list (Set.diff prev_targets targets) in
      let dir = Gen_key.directory gen_key in
      Builder.of_deferred (fun () -> remove_stale_artifacts ~dir ~stale) *>>= fun () ->
      if not (Path.Rel.Set.equal prev_targets targets) then (
        Hashtbl.set (Misc.mod_persist (generated t)) ~key:gen_key ~data:targets;
      );
      return ruleset

let generate_ruleset : (t -> Gen_key.t -> Ruleset.t Builder.t) =
  fun t gen_key ->
    share_and_check_for_cycles t
      ~key: gen_key
      ~memo: (memo_generating t)
      ~item: (DG.Item.Gen_key gen_key)
      ~f: (fun t -> generate_ruleset t gen_key)

let buildable_targets : (t -> dir:Path.t -> Path.Set.t Builder.t) =
  fun t ~dir ->
    match Path.case dir with
    | `absolute _ -> return Path.Set.empty
    | `relative dir ->
      jenga_root t *>>= fun env1 ->
      let gen_keys = Env1.gen_keys_for_directory env1 ~dir in
      begin
        Builder.all (
          List.map gen_keys ~f:(fun gen_key ->
            generate_ruleset t gen_key *>>| fun ruleset ->
            Ruleset.targets ruleset
          )) *>>| Path.Rel.Set.union_list
      end *>>| fun targets ->
      Path.Set.map targets ~f:Path.of_relative

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

(* Prevent overlapping executions on a specific path *)
module Path_Key : sig
  type 'a t
  val tag    : 'a t -> 'a
  val locked : 'a t -> bool
  val wait   : 'a t -> unit Deferred.t
  val set_cell : 'a t -> unit Deferred.t -> unit
  val get : Path.Rel.t -> Path.Rel.t t
end = struct
  type 'a t = {tag: 'a; mutable cell: unit Deferred.t}

  let create ~tag = {tag; cell = Deferred.unit}
  let tag t = t.tag

  let locked t = not (Deferred.is_determined t.cell)

  let wait t = t.cell

  let set_cell t cell = t.cell <- cell

  let get =
    let tenacious_keys = Weak_hashtbl.create Path.Rel.hashable in
    fun t ->
      let default () = Heap_block.create_exn (create ~tag:t) in
      Heap_block.value (Weak_hashtbl.find_or_add tenacious_keys t ~default)
end

let path_locked path = Path_Key.locked (Path_Key.get path)

module P : sig

  val prevent_overlap
    :  keys: 'k Path_Key.t list
    -> ?notify_wait: ('k -> unit)
    -> ?notify_add:  ('k -> unit)
    -> ?notify_rem:  ('k -> unit)
    -> 'a Tenacious.t
    -> 'a Tenacious.t
end = struct

  let prevent_overlap
      : (
        keys: 'k Path_Key.t list ->
        ?notify_wait: ('k -> unit) ->
        ?notify_add:  ('k -> unit) ->
        ?notify_rem:  ('k -> unit) ->
        'a Tenacious.t ->
        'a Tenacious.t
      ) =
    fun ~keys
      ?(notify_wait = ignore)
      ?(notify_add  = ignore)
      ?notify_rem
      tenacious ->
        let ivar = Ivar.create () in
        let cell = Ivar.read ivar in
        let wait_for_key key =
          if Path_Key.locked key
          then (notify_wait (Path_Key.tag key); Some (Path_Key.wait key))
          else None
        in
        let acquire_key key =
          (* Can't acquire a locked key *)
          assert (not (Path_Key.locked key));
          Path_Key.set_cell key cell;
          notify_add (Path_Key.tag key)
        in
        let rec acquire () =
          match List.rev_filter_map keys ~f:wait_for_key with
          | [] ->
            List.iter ~f:acquire_key keys;
            Deferred.unit
          | defer ->
            Deferred.all_unit defer >>= acquire
        in
        let release () =
          Option.iter notify_rem ~f:(fun f -> List.iter keys
            ~f:(fun key -> f (Path_Key.tag key)));
          Ivar.fill ivar ()
        in
        Tenacious.with_acquire_release tenacious ~acquire ~release

end

let prevent_action_overlap =
  fun ~targets builder ->
    Builder.wrap (
      P.prevent_overlap
        ~keys:(List.map ~f:Path_Key.get targets)
        (*~notify_wait:(fun key ->
          Message.message "waiting for action to complete for: %s"
            (Path.Rel.to_string key);
        )*)
        (*~notify_add:(fun target ->
          Message.message "target: %s, SET active" (Path.Rel.to_string target);
        )
        ~notify_rem:(fun target ->
          Message.message "target: %s, RM active" (Path.Rel.to_string target);
        )*)
        (* FIXME: Useless reification? *)
        (Builder.expose builder)
    )

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
    build_depends t (Target_rule.action_depends tr) *>>= fun (action,deps_proxy_map) ->
    prevent_action_overlap ~targets (
      (* The persistent caching is keyed of the [head_target] *)
      let head_target,other_targets = Target_rule.head_target_and_rest tr in
      jenga_root t *>>= fun env1 ->
      let job = Action.job action in
      let run_and_cache rr =
        Builder.uncancellable (
          run_action_for_targets t rr env1 action job ~targets ~need *>>= fun () ->
          (* The cache is cleared AFTER the action has been run, and BEFORE we check the
             targets created by the action. Clearing the cache forces the files to be
             re-stated, without relying on any events from inotify to make this happen *)
          let () =
            List.iter targets ~f:(fun rel ->
              Fs.clear_cache_for_target t.fs (Path.of_relative rel)
            )
          in
          check_targets t targets *>>= function
          | `missing paths -> error (Reason.Rule_failed_to_generate_targets paths)
          | `ok path_tagged_proxys ->
            match (Proxy_map.create_by_path path_tagged_proxys) with
            | `err _inconsistency -> error Reason.Inconsistent_proxies
            | `ok targets_proxy_map ->
              let rule_proxy = {
                Rule_proxy.
                targets = targets_proxy_map;
                deps = deps_proxy_map;
                action = job;
              }
              in
              Hashtbl.set (Misc.mod_persist (ruled t)) ~key:head_target ~data:rule_proxy;
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
          match (Proxy_map.diff ~before:prev.Rule_proxy.deps ~after:deps_proxy_map) with
          | Some keys -> run_and_cache (RR.Deps_have_changed keys)
          | None ->
            (* de-sensitize to the pre-action state of targets...
               in case we have to run the action
            *)
            Builder.desensitize (check_targets t targets) *>>= fun (opt,heart) ->
            match opt with
            | `missing paths -> run_and_cache (RR.Targets_missing paths)
            | `ok path_tagged_proxys ->
              match (Proxy_map.create_by_path path_tagged_proxys) with
              | `err _inconsistency -> error Reason.Inconsistent_proxies
              | `ok targets_proxy_map ->
                match (Proxy_map.diff ~before:prev.Rule_proxy.targets ~after:targets_proxy_map) with
                | Some keys ->
                  let paths = List.map keys ~f:Pm_key.to_path_exn in
                  run_and_cache (RR.Targets_not_as_expected paths)
                | None ->
                  (* Everything is as it should be! re-sensitize to the targets. *)
                  if Config.show_checked t.config then  (
                    Message.message "NOT RUNNING: %s" (Job.string_for_sh job);
                  );
                  Builder.sensitize heart *>>= fun () ->
                  return path_tagged_proxys
    )

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
    jenga_root t *>>= fun env1 ->
    match (Env1.lookup_gen_key env1 goal) with
    | None ->
      begin
        match goal with
        | Goal.Alias _ -> error Reason.No_definition_for_alias
        | Goal.Path demanded -> expect_source t demanded
      end
    | Some gen_key ->
      generate_ruleset t gen_key *>>= fun ruleset ->
      match goal with
      | Goal.Alias alias ->
        begin
          match (Ruleset.lookup_alias ruleset alias) with
          | None -> error Reason.No_definition_for_alias
          | Some depends ->
            build_depends t depends *>>| fun ((),pm) -> pm
        end
      | Goal.Path demanded ->
        begin
          match (Ruleset.lookup_target ruleset demanded) with
          | Some tr ->
            let head_target = Target_rule.head_target tr in
            begin
              if Path.Rel.equal demanded head_target
              then
                build_target_rule t tr ~demanded
              else (
                (* If not head target, add explicit dependence on it *)
                build_sub_goal t (Goal.Path head_target) *>>= fun _ ->
                build_target_rule t tr ~demanded
              )
            end
            *>>= fun tagged ->
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

let is_path_a_directory : (t -> Path.Rel.t -> bool Builder.t) =
  fun t path ->
    ensure_directory t ~dir:(Path.Rel.dirname path) *>>= fun () ->
    Builder.desensitize (
      Builder.of_tenacious (Fs.digest_file t.fs ~file:(Path.of_relative path))
    ) *>>= fun (res,__heart) ->
    match res with
    | `is_a_dir -> return true
    | _ -> return false

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  (* wrapper for special case:
     translate goal: dir -> dir/.DEFAULT (when dir is a directory) *)
  fun t goal ->
    begin
      match goal with
      | Goal.Alias _ -> return None
      | Goal.Path demanded ->
        is_path_a_directory t demanded *>>= function
        | true -> return (Some demanded)
        | false -> return None
    end
    *>>= function
    | None -> build_goal t goal
    | Some demanded ->
      let goal = Goal.Alias (Alias.default ~dir:(Path.of_relative demanded)) in
      build_sub_goal t goal

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
      jenga_root t *>>= fun env1 ->
      match (Env1.lookup_gen_key env1 goal) with
      | None -> return None (* env indicates no generator, so path must be source *)
      | Some gen_key ->
        generate_ruleset t gen_key *>>= fun ruleset ->
        match (Ruleset.lookup_target ruleset rel) with
        | None -> return None (* generator has no rule, so path must be source *)
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
    let goal = Goal.Alias alias in
    jenga_root t *>>= fun env1 ->
    match (Env1.lookup_gen_key env1 goal) with
    | None -> return Path.Set.empty (* env indicates no generator *)
    | Some gen_key ->
      generate_ruleset t gen_key *>>= fun ruleset ->
      match (Ruleset.lookup_alias ruleset alias) with
      | None -> return Path.Set.empty (* no definition for alias in this generator *)
      | Some depends ->
        reflect_depends t depends *>>| fun ((),set) ->
        set

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
      recurse_build_goal = build_goal;
      recurse_reflect_path = reflect_path;
      recurse_reflect_alias = reflect_alias;
      recurse_buildable_targets = buildable_targets;
      discovered_graph;
      node = dg_root;
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
      Env1.t option Tenacious.t
    ) =
  fun ~jr_spec fs persist memo discovered_graph ~dg_root config progress ->
    let t = {
      config;
      fs;
      persist;
      memo;
      progress;
      jr_spec;
      recurse_build_goal = build_goal;
      recurse_reflect_path = reflect_path;
      recurse_reflect_alias = reflect_alias;
      recurse_buildable_targets = buildable_targets;
      discovered_graph;
      node = dg_root;
    } in
    let builder = jenga_root t
    in
    let tenacious =
      Tenacious.bind (Builder.expose builder) (function
      | Error _ -> Tenacious.return None
      | Ok env1 -> Tenacious.return (Some env1)
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
  let pr fmt = ksprintf (fun s -> Printf.printf "%s\n%!" s) fmt in
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
  build_once
----------------------------------------------------------------------*)

let compact_zero_overhead () =
  let prev_space_overhead = (Gc.get()).Gc.Control.space_overhead in
  Gc.tune ~space_overhead:0 ();
  Gc.compact();
  Gc.tune ~space_overhead:prev_space_overhead ()

let build_once :
    (Config.t -> DG.t -> Progress.t -> unit Builder.t -> (Heart.t * int) Deferred.t) =
  (* Make a single build using the top level tenacious builder
     Where a single build means we've done all we can
     (maybe we are complete, or maybe some targets are in error)
     given the current state of the file-system.
     And now we are just polling of file-system changes.
  *)
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun config __dg progress top_builder ->
    let u = genU() in (* count of the times we reach polling *)
    let start_time = Time.now() in

    let top_tenacious = Builder.expose top_builder in

    Tenacious.exec top_tenacious >>| fun (ore,heart) ->

    (* to avoid reporting of stale errors etc... *)
    (*Progress.mask_unreachable progress dg;*)

    let needs_in_error =
      match ore with
      | Ok () -> Progress.Need.Set.empty
      | Error problem -> Problem.needs_in_error problem
    in

    let needs_in_error = Set.to_list needs_in_error in

    let is_reachable_error =
      let hs = Progress.Need.Hash_set.of_list needs_in_error in
      fun need -> Hash_set.mem hs need
    in
    Progress.mask_unreachable progress ~is_reachable_error;

    let duration = Time.diff (Time.now()) start_time in

    let snap = Progress.snap progress in

    let () =
      (* NO point doing the GC unless we are in polling mode *)
      if Config.poll_forever config then (
        Gc.full_major();
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
    Progress.message_errors config progress;
    heart, exit_code

(*----------------------------------------------------------------------
  entry point -- build_forever
----------------------------------------------------------------------*)

let run_user_function_from_env_opt env_opt tag ~f =
  match env_opt with
  | None -> Deferred.unit
  | Some env1 ->
    Monitor.try_with (fun () ->
      f env1
    ) >>= function
    | Ok () -> Deferred.unit
    | Error exn ->
      let exn = Monitor.extract_exn exn in
      Message.error "%s: threw exception:\n%s" tag (Exn.to_string exn);
      Deferred.unit


let build_forever =
  (* co-ordinate the build-forever process *)

  fun config progress ~jr_spec ~top_level_demands fs persist
    ~when_polling ~when_rebuilding ->

    let memo = Memo.create () in
    let discovered_graph = DG.create config in

    let dg_root = DG.create_root discovered_graph in

    let deadlock_found = Ivar.create () in
    let () =
      don't_wait_for (
        Ivar.read deadlock_found >>= fun () ->
        Quit.quit Exit_code.cycle_abort;
        Deferred.return ()
      )
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

      let get_env_opt =
        get_env_option
          ~jr_spec
          fs persist memo discovered_graph ~dg_root
          config progress
      in

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
      run_user_function_from_env_opt env_opt "build_begin" ~f:Env1.build_begin >>= fun () ->

      (* do the build once *)
      build_once config discovered_graph progress top_builder >>= fun (heart,exit_code) ->

      (* call user build_end function *)
      run_user_function_from_env_opt env_opt "build_end" ~f:Env1.build_end >>= fun () ->

      fin := true;
      when_polling() >>= fun () ->

      match Config.poll_forever config with
      | false ->
        Message.message "build finished; not in polling mode so quitting";
        Quit.quit exit_code;
        Deferred.return ()
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
