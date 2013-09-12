
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let equal_using_compare compare = fun x1 x2 -> Int.(=) 0 (compare x1 x2)

let (<>) = Int.(<>)

open Description

module Digest = Fs.Digest
module Glob = Fs.Glob
module DG = Discovered_graph

let exit_code_upon_control_c = ref Exit_code.incomplete

(*----------------------------------------------------------------------
  effort
----------------------------------------------------------------------*)

let persist_saves_done = Effort.Counter.create "db-save" (* for use in persist.ml *)

(* jem -work *)
let all_effort =
  Effort.create [
    Fs.lstat_counter;
    Fs.digest_counter;
    (*Fs.mkdir_counter;*)
    Fs.ls_counter;
    Job.external_jobs_run;
    persist_saves_done;
  ]

let snap_all_effort () = Effort.snap all_effort

(*----------------------------------------------------------------------
 stale targets
----------------------------------------------------------------------*)

let remove_stale_artifacts ~stale =
  match stale with [] -> return () | _::_ ->
    Deferred.List.iter stale ~f:(fun path ->
      let path_string = Path.to_string path in
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

module Env1 : sig

  type t
  val of_env : Env.t ->  [ `ok of t | `dups of scheme_id list ]
  val rel_path_semantics : t -> Forker.Rel_path_semantics.t
  val putenv : t -> (string * string) list
  val lookup_gen_key : t -> Goal.t -> Gen_key.t option
  val run_generator_scheme : t -> Gen_key.t -> (Rule_generator.t,exn) Result.t
  val build_begin : t -> unit Deferred.t
  val build_end : t -> unit Deferred.t

end = struct

  type t = {
    version : Version.t;
    putenv : (string * string) list;
    lookup_gen_key : Goal.t -> Gen_key.t option;
    run_generator_scheme : Gen_key.t -> (Rule_generator.t,exn) Result.t;
    build_begin : unit -> unit Deferred.t;
    build_end : unit -> unit Deferred.t;
  } with fields

  let rel_path_semantics t =
    match t.version with
    | Version.Pre_versioning
      -> Forker.Rel_path_semantics.Old_wrt_repo_root
    | Version.V_2013_07_09
      -> Forker.Rel_path_semantics.New_wrt_working_dir

  let build_begin t = t.build_begin ()
  let build_end t = t.build_end ()

  let of_env env =
    let {Env. version; putenv; command_lookup_path;
         build_begin; build_end;
         schemes} = env in

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
       (2) Gen_key.t -> Rule_generator.t;

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
        Some (Gen_key.create ~tag:(Rule_scheme.tag scheme) ~dir:(Goal.directory goal))
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
          let tag = Rule_scheme.tag scheme in
          let body = Rule_scheme.body scheme in
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
        version;
        putenv;
        lookup_gen_key;
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
  val create : Rule.t list -> [ `ok of t | `dups of Path.t list ]

  val lookup_target : t -> Path.t -> Target_rule.t option
  val lookup_alias : t -> Alias.t -> unit Depends.t option

  val targets : t -> Path.t list

end = struct

  type t = {
    rules  : Rule.t list;
    lookup_target : Path.t -> Target_rule.t option;
    lookup_alias : Alias.t -> unit Depends.t option;
  } with fields

  let targets t =
    List.concat_map (rules t) ~f:Rule.targets

  let create rules =
    let by_target = Path.Table.create () in
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
            | Some prev -> Depends.all_unit [depends; prev] (* merge aliases *)
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
  val of_path : Path.t -> t
  val of_glob : Glob.t -> t
  val to_string : t -> string
  val to_path_exn : t -> Path.t (* for targets_proxy_map *)
  val to_path_opt : t -> Path.t option (* for cat-build-script *)

end = struct

  module T = struct
    type t = Path of Path.X.t | Glob of Glob.t
    with sexp, bin_io, compare
  end
  include T
  include Comparable.Make_binable(T)

  let equal = equal_using_compare compare

  let of_abs_path x = Path (Path.X.of_absolute x)
  let of_path x = Path (Path.X.of_relative x)
  let of_glob x = Glob x

  let to_string = function
    | Path path -> Path.X.to_string path
    | Glob glob -> Glob.to_string glob

  let to_path_exn = function
    | Glob _ -> failwith "Proxy_map.key.to_path_exn/Glob"
    | Path x ->
      match Path.X.case x with
      | `absolute _ -> failwith "Proxy_map.key.to_path_exn/Abs"
      | `relative path -> path

  let to_path_opt = function
    | Glob _ -> None
    | Path x ->
      match Path.X.case x with
      | `absolute _ -> None
      | `relative path -> Some path

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

  val diff : t -> t -> Pm_key.t list option

  val to_path_keys : t -> Path.t list

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

  let diff t1 t2 =
    if equal t1 t2 then None
    else
      Some (
        List.filter_map
          (Map.symmetric_diff t1 t2 ~data_equal:Proxy.equal)
          ~f:(fun (k,comp) ->
            match comp with
            | `Left _ -> None
            | `Right _ -> Some k (* additional *)
            | `Unequal _ -> Some k (* changed *)
          )
      )

  let to_path_keys t =
    List.filter_map (Map.keys t) ~f:Pm_key.to_path_opt

end

module Action_proxy : sig

  type t with sexp, bin_io, compare
  include Hashable_binable with type t := t

  val create : Action.t-> t
  val diff : t -> t -> [`action_changed ] option
  val to_string : t -> string

end = struct

  module T = struct
    type t =
    | X of Xaction.t
    | I of Sexp.t
    with sexp, bin_io, compare
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make_binable(T)

  let create action =
    match (Action.case action) with
    | `xaction x -> X x
    | `iaction i -> I (Iaction.tag i)

  let diff t1 t2 =
    match t1,t2 with
    | I i1, I i2 ->
      if Sexp.compare i1 i2 <> 0 then
        Some `action_changed
      else
        None
    | X x1, X x2 ->
      if Xaction.compare x1 x2 <> 0 then
        Some `action_changed
      else
        None
    | X _,I _
    | I _,X _ ->
      Some `action_changed

  let to_string = function
    | X x -> Xaction.to_string x
    | I sexp -> sprintf "INTERNAL:%s"(Sexp.to_string sexp)

end

module Rule_proxy = struct

  type t = {
    targets : Proxy_map.t;
    deps : Proxy_map.t;
    action : Action_proxy.t
  } with sexp, bin_io, compare, fields

end

module Output_proxy = struct

  type t = {
    deps : Proxy_map.t;
    stdout : string;
  } with sexp, bin_io, compare, fields

end


(*----------------------------------------------------------------------
  (error) reason
----------------------------------------------------------------------*)

module Reason = struct

  type t =
  | Shutdown
  | Error_in_deps
  | Digest_error
  | Undigestable                      of Fs.Kind.t
  | Glob_error                        of string
  | Jenga_root_problem                of string
  | No_definition_for_alias
  | No_source_at_abs_path
  | No_rule_or_source
  | Unexpected_directory
  | Non_zero_status
  | No_directory_for_target           of string
  | Inconsistent_proxies              of Proxy_map.inconsistency
  | Duplicate_scheme_ids              of scheme_id list
  | Scheme_raised                     of exn
  | Running_job_raised    of exn
  | Multiple_rules_for_paths          of Gen_key.t * Path.t list
  | Rule_failed_to_generate_targets   of Path.t list
  | Usercode_raised                   of exn
  (* | Cycle... *)
  with sexp_of

  let to_string_one_line = function
    | Shutdown                          -> "Shutdown"
    | Error_in_deps                     -> "Unable to build dependencies"
    | Digest_error                      -> "unable to digest file"
    | Glob_error s                      -> sprintf "glob error: %s" s
    | Jenga_root_problem s              -> sprintf "Problem with %s: %s" (Misc.jenga_root_basename) s
    | No_definition_for_alias           -> "No definition found for alias"
    | No_source_at_abs_path             -> "No source at absolute path"
    | No_rule_or_source                 -> "No rule or source found for target"
    | Unexpected_directory              -> "Unexpected directory found for target"
    | Non_zero_status                   -> "External command has non-zero exit code"
    | No_directory_for_target s         -> sprintf "No directory for target: %s" s
    | Scheme_raised _                   -> "Generator scheme raised exception"
    | Running_job_raised _              -> "Running external job raised exception"
    | Rule_failed_to_generate_targets _ -> "Rule failed to generate targets"

    | Multiple_rules_for_paths (gen_key,_) ->
      sprintf "Multiple rules generated for some paths (by: %s)" (Gen_key.to_string gen_key)

    | Usercode_raised _ ->
      "User-code raised exception"
    | Undigestable k                    ->
      sprintf "undigestable file kind: %s" (Fs.Kind.to_string k)
    | Duplicate_scheme_ids xs           ->
      sprintf "Duplicate schemes with ids: %s"
        (String.concat ~sep:" " (List.map xs ~f:(sprintf "%S")))
    | Inconsistent_proxies inconsistency ->
      sprintf "Inconsistency proxies on keys: %s"
        (String.concat ~sep:" " (List.map inconsistency ~f:(fun (key,_) ->
          Pm_key.to_string key)))

  let to_extra_lines = function
    | Shutdown
    | Error_in_deps
    | Digest_error
    | Undigestable _
    | Glob_error _
    | Jenga_root_problem _
    | No_definition_for_alias
    | No_rule_or_source
    | No_source_at_abs_path
    | Unexpected_directory
    | Non_zero_status
    | No_directory_for_target _
    | Duplicate_scheme_ids _
    | Inconsistent_proxies _
      -> []

    | Scheme_raised exn
    | Usercode_raised exn
    | Running_job_raised exn
      -> [Exn.to_string exn]

    | Multiple_rules_for_paths (_,paths)
    | Rule_failed_to_generate_targets paths
      -> List.map paths ~f:(fun path -> "- " ^ Path.to_string path)

  let messages ~tag t =
    Message.error "%s: %s" tag (to_string_one_line t);
    List.iter (to_extra_lines t) ~f:(fun s -> Message.message "%s" s);

end

(*----------------------------------------------------------------------
  Progress - progress monitor
----------------------------------------------------------------------*)

module What = struct (* what was build *)
  type t = Source | Target | Alias with sexp_of
end

module Status = struct
  type t =
  | Checking (* the default status we return to after doing anything of significance, such
                as running a generator/scanner or action *)
  | Blocked (* for deps to be checked *)

  (* Jwait/Running show when an external job is in/though the -j throttle *)
  | Jwait
  | Running
  | Usercode

  | Built of What.t
  | Error of Reason.t
  with sexp_of

  let to_string_one_line = function
    | Error reason -> Reason.to_string_one_line reason
    | status -> Sexp.to_string (sexp_of_t status)

end

module Progress : sig

  type t
  val create : Fs.t -> t

  val set_status : t -> key:Goal.t -> data:Status.t -> unit
  val mask_unreachable : t -> DG.t -> unit
  val message_errors : t -> unit
  val message_everything : t -> unit
  val snap : t -> Mon.Progress.t

end = struct

  type t = {
    fs : Fs.t;
    status : Status.t Goal.Table.t;
    mutable mask : Goal.Hash_set.t;
  }

  let create fs = {
    fs;
    status = Goal.Table.create();
    mask = Goal.Hash_set.create () ;
  }

  let set_status t = Hashtbl.set t.status

  let mask_unreachable t dg =
    let mask_candidates = Goal.Hash_set.create () in
    Hashtbl.iter t.status ~f:(fun ~key:goal ~data:_ ->
      Hash_set.add mask_candidates goal; (* add goal as candiate for mask *)
    );
    DG.iter_reachable dg ~f:(fun node ->
      match (DG.lookup_item dg node) with
      | DG.Item.Goal goal ->
        Hash_set.remove mask_candidates goal (* goal is reachable; dont remove *)
      | _ -> ()
    );
    t.mask <- mask_candidates

  let iter_unmasked t ~f =
    Hashtbl.iter (t.status) ~f:(fun ~key ~data ->
      if not (Hash_set.mem t.mask key) then
        f ~key ~data
    )

  let message_errors t =
    iter_unmasked t ~f:(fun ~key:goal ~data:status ->
      match (
        match status with
        (* suppress shutdown/dep-errors errors *)
        | Status.Error Reason.Shutdown
        | Status.Error Reason.Error_in_deps
          -> None
        | Status.Error reason -> Some reason
        | _ -> None
      ) with
      | None -> ()
      | Some reason ->
        Message.error "(summary) %s: %s" (Goal.to_string goal)
          (Reason.to_string_one_line reason)
    )

  let message_everything t =
    Hashtbl.iter (t.status) ~f:(fun ~key:goal ~data:status ->
      let masked = Hash_set.mem t.mask goal in
      Message.message "(everything) %s: %s%s"
        (Goal.to_string goal)
        (Status.to_string_one_line status)
        (if masked then " (MASKED)" else "")
    )

  let snap t =
    let checking = ref 0 in
    let blocked = ref 0 in
    let jwait = ref 0 in
    let running = ref 0 in
    let usercode = ref 0 in
    let source = ref 0 in
    let target = ref 0 in
    let alias = ref 0 in
    let scanner = ref 0 in
    let glob = ref 0 in
    let error = ref 0 in
    let failure = ref 0 in
    iter_unmasked t
      ~f:(fun ~key:_ ~data:status ->
        let x =
          match status with
          | Status.Checking                         -> checking
          | Status.Blocked                          -> blocked
          | Status.Jwait                            -> jwait
          | Status.Running                          -> running
          | Status.Usercode                         -> usercode
          | Status.Built What.Source                -> source
          | Status.Built What.Target                -> target
          | Status.Built What.Alias                 -> alias
          | Status.Error Reason.Error_in_deps       -> failure
          | Status.Error _                          -> error
        in incr x
      );
    {Mon.Progress.
     checking   = !checking;
     blocked    = !blocked;
     jwait      = !jwait;
     running    = !running;
     usercode   = !usercode;
     source     = !source;
     target     = !target;
     alias      = !alias;
     scanner    = !scanner;
     glob       = !glob;
     error      = !error;
     failure    = !failure;
    }

end

(*----------------------------------------------------------------------
  Builder
----------------------------------------------------------------------*)

module Builder : sig (* layer error monad within tenacious monad *)
  include Tenacious.REIFIABLE

  val wrap : ('a, Reason.t) Result.t Tenacious.t -> 'a t
  val expose : 'a t -> ('a, Reason.t) Result.t Tenacious.t

  val of_tenacious : 'a Tenacious.t -> 'a t
  val expose_unit : 'a t -> unit Tenacious.t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : 'a t -> ('a -> 'b) -> 'b t

  val error : Reason.t -> 'a t

  val reify : 'a t -> 'a node

  val for_all_without_errors :
    'a list -> f:('a -> 'b t) -> 'b list option t

  val of_deferred : (unit -> 'a Deferred.t) -> 'a t

  val desensitize : 'a t -> ('a * Heart.t) t
  val sensitize : Heart.t -> unit t

  val before_redo : 'a t -> f:(unit -> unit) -> 'a t

end = struct

  include Tenacious.Reifiable(struct
    type ('a,'b) t' = ('a, Reason.t) Result.t Tenacious.t
  end)

  let wrap t = t
  let expose t = t

  let reify x = Tenacious.((reify x :> _ t))

  let of_tenacious tenacious =
    Tenacious.bind tenacious (fun x ->
      Tenacious.return (Ok x)
    )

  let expose_unit t =
    Tenacious.bind t (fun _ -> Tenacious.return ())

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

  let error e = Tenacious.return (Error e)

  let without_errors errs =
    let rec loop acc = function
      | [] -> Some (List.rev acc)
      | Error _  :: _ -> None
      | Ok x :: xs -> loop (x::acc) xs
    in loop [] errs

  let for_all_without_errors xs ~f =
    Tenacious.bind (Tenacious.all (List.map xs ~f)) (
      fun ys -> return (without_errors ys)
    )

  let of_deferred f =
    Tenacious.lift (fun () ->
      f () >>| fun x -> (Ok x, Heart.unbreakable)
    )

  let desensitize t = (* if not error *)
    Tenacious.lift_cancelable (fun ~cancel ->
      Deferred.map (Tenacious.exec_cancelable ~cancel t)
      ~f:(function None -> None
          | Some (ore,heart) -> match ore with
             | Error e -> Some (Error e, heart)
             | Ok x -> Some (Ok (x,heart), Heart.unbreakable)))

  let sensitize heart =
    Tenacious.lift (fun () -> Deferred.return (Ok (), heart))

  let before_redo = Tenacious.before_redo

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
  | Targets_missing                 of Path.t list
  | Targets_not_as_expected         of Path.t list
  with sexp

  (*let to_string t = Sexp.to_string (sexp_of_t t)*)

  let to_string config = function
  | No_record_of_being_run_before   -> "initial"
  | Action_changed                  -> "action"
  | Deps_have_changed keys ->
    if Config.run_reason_verbose config then
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
    generated   : Path.Set.t Gen_key.Table.t;
    ruled       : Rule_proxy.t Path.Table.t; (* actions run for target-rules *)
    actioned    : Output_proxy.t Action_proxy.Table.t; (* actions run for their stdout *)
  } with sexp, bin_io

  let create () = {
    generated = Gen_key.Table.create();
    ruled = Path.Table.create();
    actioned = Action_proxy.Table.create();
  }

  let cat_build_script t paths =
    (* construct mapping from a target to the head target of the rule.
       We need the head_target to key into t.ruled.
    *)
    let head_target =
      let table = Path.Table.create () in
      List.iter (Hashtbl.data t.ruled) ~f:(fun rule_proxy ->
        let targets = Proxy_map.to_path_keys (Rule_proxy.targets rule_proxy) in
        match targets with
        | [] -> assert false
        | head::rest ->
          List.iter rest ~f:(fun path ->
            Hashtbl.add_exn table ~key:path ~data:head
          );
      );
      fun path ->
        match (Hashtbl.find table path) with
        | Some head -> head
        | None -> path
    in
    (* collect action and target dirs *)
    let sources = Path.Hash_set.create () in
    let target_dirs = Path.Hash_set.create () in
    let actions = ref [] in (* proxies *)
    let seen = Path.Hash_set.create () in
    let rec walk_path path =
      let path = head_target path in
      if (Hash_set.mem seen path) then () else (
        Hash_set.add seen path;
        match (Hashtbl.find t.ruled path) with
        | None -> Hash_set.add sources path
        | Some rule_proxy ->
          let targets = Proxy_map.to_path_keys (Rule_proxy.targets rule_proxy) in
          List.iter targets ~f:(fun path -> Hash_set.add target_dirs (Path.dirname path));
          let deps = Proxy_map.to_path_keys (Rule_proxy.deps rule_proxy) in
          List.iter deps ~f:walk_path;
          let action = Rule_proxy.action rule_proxy in
          actions := action :: !actions
      )
    in
    (* walk the tree from the roots demanded *)
    List.iter paths ~f:walk_path;
    (* no point ensuring dir for any dir which has source in it *)
    List.iter (Hash_set.to_list sources) ~f:(fun path ->
      Hash_set.remove target_dirs (Path.dirname path)
    );
    (* generate output *)
    let write_line fmt = ksprintf (fun s -> Printf.printf "%s\n%!" s) fmt in
    List.iter (Hash_set.to_list sources) ~f:(fun source ->
      write_line "#SOURCE: %s" (Path.to_string source);
    );
    List.iter (Hash_set.to_list target_dirs) ~f:(fun dir ->
      write_line "mkdir -p %s" (Path.to_string dir)
    );
    List.iter (List.rev (!actions)) ~f:(fun action ->
      write_line "%s" (Action_proxy.to_string action);
    );
    ()


end


(*----------------------------------------------------------------------
  Memo - dynamic cache - support sharing & cycle detection
----------------------------------------------------------------------*)

type memo_ruling_t = (PPs.t Builder.node) Path.Table.t

module Memo = struct

  (* The value in these hashtable are computations, and so are not sexp convertable.
     But that's ok, because they DONT get saved to file.

     The "ing" suffix is intended to indicate the continuous nature of these
     computatations *)

  type t = {
    generating  : ((Ruleset.t * memo_ruling_t) Builder.node * DG.Node.t) Gen_key.Table.t;
    building    : (Proxy_map.t  Builder.node * DG.Node.t) Goal.Table.t;
    root        : Env1.t Builder.node option ref;
  }

  let create () = {
    generating = Gen_key.Table.create();
    building = Goal.Table.create();
    root = ref None;
  }

end

(*----------------------------------------------------------------------
  t - The downwards bucket type
----------------------------------------------------------------------*)

type t = {

  config : Config.t; (* messaging choices *)
  fs : Fs.t; (* file system *)
  job_throttle : unit Throttle.t; (* -j throttle for external jobs *)
  persist : Persist.t; (* persisant cache *)
  memo : Memo.t; (* dynmaic cache *)
  progress : Progress.t; (* track state of each goal being built *)
  jenga_root_path : Path.X.t; (* access to the jengaroot *)

  (* recursive calls to build_goal go via here - avoids having big mutual let rec *)
  recurse_build_goal : (t -> Goal.t -> Proxy_map.t Builder.t);

  (* know goal being worked on, so can set the status *)
  me : Goal.t option; (* None means jengaroot *)

  (* discovered_graph structure & current node - used for cycle checking *)
  discovered_graph : DG.t;
  node : DG.Node.t;

} with fields

let set_status t status =
  match t.me with
  | None -> ()
  | Some key -> Progress.set_status t.progress ~key ~data:status

let generated t = t.persist.Persist.generated
let ruled t = t.persist.Persist.ruled
let actioned t = t.persist.Persist.actioned

let memo_generating t = t.memo.Memo.generating
let memo_building t = t.memo.Memo.building
let memo_root t = t.memo.Memo.root

let build_sub_goal : (t ->  Goal.t -> Proxy_map.t Builder.t) =
  fun t goal -> t.recurse_build_goal t goal


let enqueue_external_job t f =
  set_status t Status.Jwait;
  Throttle.enqueue t.job_throttle (fun () ->
    if Quit.is_quitting()
    then (
      Deferred.return (Error (`other_error Job.Shutdown));
    )
    else (
      set_status t Status.Running;
      f () >>= fun res ->
      set_status t Status.Checking;
      Deferred.return res
    )
  )

let error t reason =
  let show_now =
    match reason with
    | Reason.Non_zero_status (* we see the error message from the command *)
    | Reason.Error_in_deps
    | Reason.Shutdown
        -> false
    | _
      -> true
  in
  if show_now then (
    let tag =
      match t.me with
      | Some goal -> Goal.to_string goal
      | None -> Path.X.to_string t.jenga_root_path
    in
    Reason.messages ~tag reason;
  );
  if Config.stop_on_first_error t.config then (
    (*Message.message "calling quit on first (every!) error";*)
    Quit.quit Exit_code.build_failed; (* we still go on to report the error *)
  );
  error reason


let try_deferred t ~reason f =
  Builder.of_deferred (fun () ->
    Monitor.try_with f
  ) *>>= function
  | Ok x -> return x
  | Error exn ->
    let exn = Monitor.extract_exn exn in
    error t (reason exn)


let run_user_code t env1 f =
  let reason = (fun exn -> Reason.Usercode_raised exn) in
  try_deferred t ~reason (fun () ->
    set_status t Status.Usercode;
    let putenv = Env1.putenv env1 in
    List.iter putenv ~f:(fun (key,data) -> Core.Std.Unix.putenv ~key ~data);
    f() >>= fun res ->
    set_status t Status.Checking;
    Deferred.return res
  )


(*----------------------------------------------------------------------
  File system interface
----------------------------------------------------------------------*)

let digest_path
    : (t -> Path.X.t -> [ `file of Digest.t | `missing | `is_a_dir ] Builder.t) =
  fun t path ->
    Builder.of_tenacious (Fs.digest_file t.fs ~file:path)
    *>>= function
    | `stat_error _   -> return `missing
    | `is_a_dir       -> return `is_a_dir
    | `undigestable k -> error t (Reason.Undigestable k)
    | `digest_error _ -> error t Reason.Digest_error
    | `digest digest  -> return (`file digest)

let look_for_source : (t -> Path.t -> Proxy.t option Builder.t) =
  fun t path ->
    digest_path t (Path.X.of_relative path) *>>= function
    | `file digest    -> return (Some (Proxy.of_digest digest))
    | `missing        -> return None
    | `is_a_dir       -> return None

let need_glob : (t -> Glob.t -> Fs.Listing.t Builder.t) =
  fun t glob ->
    Builder.of_tenacious (Fs.list_glob t.fs glob) *>>= function
    | `stat_error _   -> error t (Reason.Glob_error "no such directory")
    | `not_a_dir      -> error t (Reason.Glob_error "not a directory")
    | `listing_error _-> error t (Reason.Glob_error "unable to list")
    | `listing listing -> return listing

let ensure_directory : (t -> dir:Path.t -> unit Builder.t) =
  fun t ~dir ->
    Builder.of_tenacious (Fs.ensure_directory t.fs ~dir:(Path.X.of_relative dir))
    *>>= function
    | `ok -> return ()
    | `not_a_dir -> error t (Reason.No_directory_for_target "not a directory")
    | `failed -> error t (Reason.No_directory_for_target "failed to create")


(*----------------------------------------------------------------------
  running things - generator, scanners, actions
----------------------------------------------------------------------*)

let run_action_with_message :
    (t -> Env1.t -> Action.t -> message:(unit->unit) -> need:string ->
     output:'a Job.Output.t -> 'a Builder.t) =
  fun t env1 action ~message ~need ~output ->
    match Action.case action with
    | `iaction iaction ->
      run_user_code t env1 (fun ()->
        message();
        Iaction.func iaction () >>= fun () ->
        Deferred.return (Job.Output.none output)
      )
    | `xaction xaction ->
      (* The putenv is determined from the env defined in jengaroot
         but we really DONT want every command to be sensitize to jengaroot
         (its a compromise, but a necessary one!)
      *)
      let rel_path_semantics = Env1.rel_path_semantics env1 in
      let putenv = Env1.putenv env1 in
      Builder.of_deferred (fun () ->
        enqueue_external_job t (fun () ->
          message();
          Job.run ~config:t.config ~need ~rel_path_semantics ~putenv ~xaction ~output
        )
      ) *>>= function
      | Ok x                                -> return x
      | Error `non_zero_status              -> error t Reason.Non_zero_status
      | Error (`other_error Job.Shutdown)   -> error t Reason.Shutdown
      | Error (`other_error exn)            -> error t (Reason.Running_job_raised exn)



let run_action_for_targets :
    (t -> RR.t -> Env1.t -> Action.t -> Action_proxy.t ->
     targets:Path.t list -> need:string ->
     unit Builder.t) =
  (* After running an action, synchronise until all inotify events triggered while the
     action was run have been delivered (and acted upon) by this process *)
  fun t rr env1 action action_proxy ~targets ~need ->
    let message() =
      if Config.show_actions_run t.config then
        Message.message "Building: %s [%s]"
          (String.concat ~sep:" " (List.map targets ~f:Path.to_string))
          (RR.to_string t.config rr)
    in
    let sync_contents = Action_proxy.to_string action_proxy in
    Builder.wrap (
      Fs.sync_inotify_delivery t.fs ~sync_contents (
        Builder.expose (
          run_action_with_message t env1 action ~message ~need
            ~output:Job.Output.ignore
        )
      )
    )

let run_action_for_stdout :
    (t -> RR.t -> Env1.t -> Action.t -> Action_proxy.t -> string Builder.t) =
  fun t rr env1 action action_proxy ->
    let message() =
      if Config.show_actions_run t.config then
        Message.message "Action: %s [%s]"
          (Action_proxy.to_string action_proxy)
          (RR.to_string t.config rr)
    in
    run_action_with_message t env1 action ~message ~need:"stdout"
      ~output:Job.Output.stdout


(*----------------------------------------------------------------------
  jenga_root
----------------------------------------------------------------------*)

let jenga_root : (t -> Env1.t Builder.t) =
  (* wrap up the call to [Load_root.get_env] into a tenacious builder,
     which will reload any time the jengaroot is modified *)
  fun t ->
    digest_path t t.jenga_root_path *>>= function
    | `missing -> error t (Reason.Jenga_root_problem "missing")
    | `is_a_dir -> error t (Reason.Jenga_root_problem "is-a-directory")
    | `file __digest ->
      Builder.of_deferred (fun () -> Load_root.get_env t.jenga_root_path) *>>= function
      | Error e ->
        let exn = Monitor.extract_exn (Error.to_exn e) in
        error t (Reason.Usercode_raised exn)
      | Ok env ->
        match (Env1.of_env env) with
        | `dups xs -> error t (Reason.Duplicate_scheme_ids xs)
        | `ok env1 -> return env1

let jenga_root : (t -> Env1.t  Builder.t) =
  (* Memoization of jenga_root is simpler that other cases, becasuse:
     - There is only one; we don't need a hashtable, just a ref.
     - The root has no deps, so there is no chance of cycles,
  *)
  fun t ->
    (match !(memo_root t) with
    | Some builder  -> builder
    | None ->
      let builder = Builder.reify (jenga_root t) in
      memo_root t := Some builder;
      builder
    :> _ Builder.t)


(*----------------------------------------------------------------------
  build
----------------------------------------------------------------------*)

let share_builder :
    (key : 'a ->
     memo : ('a, 'b Builder.node) Hashtbl.t ->
     f : (unit -> 'b Builder.t) ->
     'b Builder.t
    ) =
  (* simpler kind of sharing - no new nodes in discovered_graph *)
  fun ~key ~memo ~f ->
    (match (Hashtbl.find memo key) with
    | Some builder -> builder
    | None ->
      let builder = Builder.reify (f ()) in
      (* we use add_exn, because each key will be memoized exactly once *)
      Hashtbl.add_exn memo ~key ~data:builder;
      builder
    :> _ Builder.t)


let share_and_check_for_cycles :
    (* memoize / check for cycles...
       3 places where this function is called, wraps for:
       - build_goal
       - generate_ruleset
       - build_target_rule
       Which are the 3 places in the build description where sharing can be encountered
       - deps can be shared betweenmultiple rules/scanner/aliase
       - rule-generation is shared betwen all goals in a gen_key (scheme+dir)
       - rules are shared between the multiple targets that the rule generates
       For each sharing point we must ensure that only one builder computation is setup
    *)
    (t ->
     key : 'a ->
     memo : ('a, 'b Builder.node * DG.Node.t) Hashtbl.t ->
     item : DG.Item.t ->
     f : (t -> 'b Builder.t) ->
     'b Builder.t
    ) =
  fun t ~key ~memo ~item ~f ->
    (match (Hashtbl.find memo key) with
    | Some (builder,node) ->
      begin
        set_status t Status.Blocked;
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
    :> _ Builder.t)


let run_action_for_stdout_if_necessary
    : (t -> deps:Proxy_map.t -> Action.t -> string Builder.t) =
  fun t ~deps action ->
    jenga_root t *>>= fun env1 ->
    let action_proxy = Action_proxy.create action in
    let run_and_cache rr =
      run_action_for_stdout t rr env1 action action_proxy *>>= fun stdout ->
      let output_proxy = {Output_proxy. deps; stdout;} in
      Hashtbl.set (Misc.mod_persist (actioned t)) ~key:action_proxy ~data:output_proxy;
      return stdout
    in
    match (Hashtbl.find (actioned t) action_proxy) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some prev ->
        match (Proxy_map.diff prev.Output_proxy.deps deps) with
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
    digest_path t (Path.X.of_absolute abs) *>>= fun res ->
    match res with
    | `missing ->  error t Reason.No_source_at_abs_path
    | `is_a_dir -> error t Reason.Unexpected_directory
    | `file digest ->
      let proxy = Proxy.of_digest digest in
      let pm = Proxy_map.single (Pm_key.of_abs_path abs) proxy in
      return pm


let build_merged_proxy_maps : (t -> Proxy_map.t list -> Proxy_map.t Builder.t) =
  fun t pms ->
    match (Proxy_map.merge pms) with
    | `err inconsistency -> error t (Reason.Inconsistent_proxies inconsistency)
    | `ok pm -> return pm

let build_depends : (t -> 'a Depends.t -> ('a * Proxy_map.t) Builder.t) =
  fun t depends ->
    let rec exec : type a. a Depends.t -> (a * Proxy_map.t) Builder.t = function

      | Depends.Return x ->
        return (x, Proxy_map.empty)

      | Depends.Bind (left,f_right) ->
        exec left *>>= fun (v1,pm1) ->
        begin
          match try (Ok (f_right v1)) with | exn -> Error exn with
          | Error exn -> error t (Reason.Usercode_raised exn)
          | Ok right ->
            exec right *>>= fun (v2,pm2) ->
            build_merged_proxy_maps t [pm1;pm2] *>>= fun pm ->
            return (v2, pm)
        end

      | Depends.All xs ->
        begin
          Builder.for_all_without_errors xs ~f:(exec) *>>= function
          | None -> error t Reason.Error_in_deps
          | Some xs ->
            let vs,pms = List.unzip xs in
            build_merged_proxy_maps t pms *>>= fun pm ->
            return (vs,pm)
        end

      | Depends.Deferred f ->
        jenga_root t *>>= fun env1 ->
        run_user_code t env1 (fun () ->
          f ()
        ) *>>| fun v ->
        (v, Proxy_map.empty)

      | Depends.Path path ->
        build_sub_goal t (Goal.Path path) *>>| fun pm -> ((), pm)

      | Depends.Absolute path ->
        need_abs_path t path *>>| fun pm -> ((), pm)

      | Depends.Alias alias ->
        build_sub_goal t (Goal.Alias alias) *>>| fun pm -> ((), pm)

      | Depends.Glob glob ->
        need_glob t glob *>>| fun listing ->
        let pm = Proxy_map.single (Pm_key.of_glob glob) (Proxy.of_listing listing) in
        (Fs.Listing.paths listing, pm)

      | Depends.Stdout action_depends ->
        exec action_depends *>>= fun (action,pm) ->
        run_action_for_stdout_if_necessary t ~deps:pm action *>>| fun stdout ->
        let pm = Proxy_map.empty in
        (stdout, pm)

    in
    exec depends



let generate_ruleset : (t -> Gen_key.t -> (Ruleset.t * memo_ruling_t) Builder.t) =
  (* Run rule generation (for a gen_key); always
     Record the generated targets in the persistent state,
     to allow stale targets to be removed later.
  *)
  fun t gen_key ->
    jenga_root t *>>= fun env1 ->
    match (Env1.run_generator_scheme env1 gen_key) with
    | Error exn -> error t (Reason.Scheme_raised exn)
    | Ok generator ->
      build_depends t (Rule_generator.rules generator) *>>= fun (rules,__proxy_map) ->
      begin
        match (Ruleset.create rules) with
        | `ok ruleset -> return ruleset
        | `dups paths -> error t (Reason.Multiple_rules_for_paths (gen_key,paths))
      end
      *>>= fun ruleset ->
      let prev_targets =
        match (Hashtbl.find (generated t) gen_key) with
        | Some x -> x
        | None -> Path.Set.empty
      in
      let targets = Path.Set.of_list (Ruleset.targets ruleset) in
      let stale = Set.to_list (Set.diff prev_targets targets) in
      Builder.of_deferred (fun () -> remove_stale_artifacts ~stale) *>>= fun () ->
      set_status t Status.Checking;
      if not (Path.Set.equal prev_targets targets) then (
        Hashtbl.set (Misc.mod_persist (generated t)) ~key:gen_key ~data:targets;
      );
      let memo_ruling = Path.Table.create() in
      return (ruleset,memo_ruling)


let generate_ruleset : (t -> Gen_key.t -> (Ruleset.t * memo_ruling_t) Builder.t) =
  (* memo *)
  fun t gen_key ->
    share_and_check_for_cycles t
      ~key: gen_key
      ~memo: (memo_generating t)
      ~item: (DG.Item.Gen_key gen_key)
      ~f: (fun t -> generate_ruleset t gen_key)


let check_targets :
    (t -> Path.t list -> [ `ok of PPs.t | `missing of Path.t list] Builder.t) =
  (* Check the targets a rule claims to build.
     - this function may be called before the action is run.
     - and will definitely be called after an action successfully completes.
  *)
  fun t paths ->
    build_all_in_sequence paths ~f:(fun path ->
      look_for_source t path *>>= fun res -> return (path,res)
    ) *>>= fun tagged ->
    let good,bad =
      List.partition_map tagged ~f:(function
      | (path,Some proxy) -> `Fst (path, proxy)
      | (path,None) -> `Snd path
      )
    in
    match bad with
    | _::_ -> return (`missing bad)
    | [] -> return (`ok good)


let prevent_action_overlap
    : (Fs.t -> targets:Path.t list -> unit Builder.t -> unit Builder.t) =
  (* Prevent running overlapping actions for the same targets *)
  fun fs ~targets builder ->
    let builder = Builder.((reify builder :> _ t)) in
    Builder.of_tenacious (
      Tenacious.prevent_overlap
        ~table:(Fs.active_targets fs)
        ~keys:targets
        ~notify_wait:(fun key ->
          Message.trace "waiting for action to complete for: %s"
            (Path.to_string key);
        )
        (*~notify_add:(fun target ->
          Message.message "target: %s, SET active" (Path.to_string target);
        )
        ~notify_rem:(fun target ->
          Message.message "target: %s, RM active" (Path.to_string target);
        )*)
        (* FIXME: Useless reification? *)
        (Tenacious.reify (Builder.expose_unit builder))
    ) *>>= fun () ->
    builder


let build_target_rule :
  (* run a rule/action, iff:
     - we have no record of running it before
     - one of its dependencies has changed
     - the action has changed (for an internal action; jengaroot has changed)
     - one of the targets is missing
     - one of the targets is different from expected
     Record a successful run in the persistent state.
  *)
    (t -> Target_rule.t -> demanded:Path.t -> PPs.t Builder.t) =
  fun t tr ~demanded ->
    let need = Path.basename demanded in
    let targets = Target_rule.targets tr in
    build_depends t (Target_rule.action_depends tr) *>>= fun (action,deps_proxy_map) ->
    (* The persistent caching is keyed of the [head_target] *)
    let head_target,other_targets = Target_rule.head_target_and_rest tr in
    jenga_root t *>>= fun env1 ->
    let action_proxy = Action_proxy.create action in
    let run_and_cache rr =
      prevent_action_overlap t.fs ~targets (
        run_action_for_targets t rr env1 action action_proxy ~targets ~need
      ) *>>= fun () ->
      check_targets t targets *>>= function
      | `missing paths -> error t (Reason.Rule_failed_to_generate_targets paths)
      | `ok path_tagged_proxys ->
        match (Proxy_map.create_by_path path_tagged_proxys) with
        | `err inconsistency -> error t (Reason.Inconsistent_proxies inconsistency)
        | `ok targets_proxy_map ->
          let rule_proxy = {
            Rule_proxy.
            targets = targets_proxy_map;
            deps = deps_proxy_map;
            action = action_proxy;
          }
          in
          Hashtbl.set (Misc.mod_persist (ruled t)) ~key:head_target ~data:rule_proxy;
          (* We remove data associated with the [other_targets]. Its not essential for
             correctness, but it avoid cruft from building up in the persistent state *)
          List.iter other_targets ~f:(fun other -> Hashtbl.remove (ruled t) other);
          return path_tagged_proxys
    in
    match (Hashtbl.find (ruled t) head_target) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some prev ->
      match (Action_proxy.diff prev.Rule_proxy.action action_proxy ) with
      | Some `action_changed -> run_and_cache RR.Action_changed
      | None ->
        match (Proxy_map.diff prev.Rule_proxy.deps deps_proxy_map) with
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
            | `err inconsistency -> error t (Reason.Inconsistent_proxies inconsistency)
            | `ok targets_proxy_map ->
              match (Proxy_map.diff prev.Rule_proxy.targets targets_proxy_map) with
              | Some keys ->
                let paths = List.map keys ~f:Pm_key.to_path_exn in
                run_and_cache (RR.Targets_not_as_expected paths)
              | None ->
                (* Everything is as it should be! re-sensitize to the targets. *)
                if Config.show_checked t.config then  (
                  Message.message "NOT RUNNING: %s" (Action_proxy.to_string action_proxy);
                );
                Builder.sensitize heart *>>= fun () ->
                return path_tagged_proxys



let build_target_rule :
    (t -> memo_ruling_t -> Target_rule.t -> demanded:Path.t -> PPs.t Builder.t) =
  fun t memo_ruling tr ~demanded ->
    (* memo! *)
    share_builder
      ~key: (Target_rule.head_target tr)
      ~memo: memo_ruling
      ~f: (fun () -> build_target_rule t tr ~demanded)


let expect_source : (t -> Path.t -> (What.t * Proxy_map.t) Builder.t) =
  fun t demanded ->
    digest_path t (Path.X.of_relative demanded) *>>= fun res ->
    match res with
    | `missing ->  error t Reason.No_rule_or_source
    | `is_a_dir -> error t Reason.Unexpected_directory
    | `file digest ->
      let proxy = Proxy.of_digest digest in
      let pm = Proxy_map.single (Pm_key.of_path demanded) proxy in
      return (What.Source, pm)


let build_goal : (t -> Goal.t -> (What.t * Proxy_map.t) Builder.t) =
  (* build a goal -- alias or path
     In either case, first get the ruleset applicable to the goal.
     For an alias - there must be a rule in this ruleset.
     For a path
     - If there is a rule, use it.
     - Otherwise, that path had better exists as source.
  *)
  fun t goal ->
    jenga_root t *>>= fun env1 ->
    match (Env1.lookup_gen_key env1 goal) with
    | None ->
      begin
        match goal with
        | Goal.Alias _ -> error t Reason.No_definition_for_alias
        | Goal.Path demanded -> expect_source t demanded
      end
    | Some gen_key ->
      generate_ruleset t gen_key *>>= fun (ruleset,memo_ruling) ->
      match goal with
      | Goal.Alias alias_id ->
        begin
          match (Ruleset.lookup_alias ruleset alias_id) with
          | None -> error t Reason.No_definition_for_alias
          | Some depends ->
            build_depends t depends *>>= fun ((),pm) ->
            return (What.Alias, pm)
        end
      | Goal.Path demanded ->
        begin
          match (Ruleset.lookup_target ruleset demanded) with
          | Some tr ->
            (* Call build_deps here as well as in build_target_rule, so polling-rebuilds
               re-create the correct edges in the discovered_graph.

               Turns out calling build_depends below is VERY slow. Need to rethink a
               better way of getting the correct discovered_graph on polling builds.
               For now: comment out the call.
            *)
            (
              if Config.avoid_target_count_bug_very_slow t.config
              then build_depends t (Target_rule.action_depends tr)
                *>>= fun _ -> return ()
              else return ()
            )
            *>>= fun () ->

            build_target_rule t memo_ruling tr ~demanded *>>= fun tagged ->
            let (path,proxy) =
              List.find_exn tagged ~f:(fun (path,_) -> Path.equal path demanded)
            in
            let pm = Proxy_map.single (Pm_key.of_path path) proxy in
            return (What.Target, pm)
          | None ->
            expect_source t demanded
        end


let is_path_a_directory : (t -> Path.t -> bool Builder.t) =
  fun t path ->
    ensure_directory t ~dir:(Path.dirname path) *>>= fun () ->
    Builder.desensitize (
      Builder.of_tenacious (Fs.digest_file t.fs ~file:(Path.X.of_relative path))
    ) *>>= fun (res,__heart) ->
    match res with
    | `is_a_dir -> return true
    | _ -> return false

let build_goal : (t -> Goal.t -> (What.t * Proxy_map.t) Builder.t) =
  (* wrapper for special case:
     translate goal: dir -> dir/.DEFAULT (when dir is a directory)
  *)
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
      let goal = Goal.Alias (Alias.default ~dir:demanded) in
      build_sub_goal t goal *>>= fun pm ->
      return (What.Alias, pm)


(* now follows a sequence of functions named [build_goal] which shadow each other &
   extend with different behaviour... *)

let build_goal : (t -> Goal.t -> (What.t * Proxy_map.t) Builder.t) =
  (* Report considering/re-considering *)
  fun t goal ->
    if Config.show_considering t.config
    then (
      Message.message "Considering: %s" (Goal.to_string goal);
    );
    set_status t Status.Checking;
    let builder = build_goal t goal in
    let builder = Builder.before_redo builder
      ~f:(fun () ->
        if Config.show_considering t.config || Config.show_reconsidering t.config
        then  (
          Message.message "Re-considering: %s" (Goal.to_string goal)
        );
        set_status t Status.Checking;
      )
    in
    builder


let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  (* Expose the builder's result/error for reporting *)
  fun t goal ->
    let builder = build_goal t goal in
    Builder.wrap (
      Tenacious.bind (
        Builder.expose builder
      ) (fun ore ->
        match ore with
        | Ok (what, pm) ->
          set_status t (Status.Built what);
          Tenacious.return (Ok pm)
        | Error reason ->
          set_status t (Status.Error reason);
          Tenacious.return (Error reason)
      )
    )

let build_goal : (t ->  Goal.t -> Proxy_map.t Builder.t) =
  fun t goal -> build_goal {t with me = Some goal} goal

let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  fun t goal ->
    (* memo! *)
    share_and_check_for_cycles t
      ~key: goal
      ~memo: (memo_building t)
      ~item: (DG.Item.Goal goal)
      ~f: (fun t -> build_goal t goal)


let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
  fun t goal ->
    if Quit.is_quitting()
    then error t Reason.Shutdown
    else build_goal t goal


let build_one_root_goal :
    (
      jenga_root_path: Path.X.t ->
      job_throttle: unit Throttle.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      DG.t ->
      Config.t ->
      Progress.t ->
      demanded:Goal.t ->
      unit Tenacious.t
    ) =
  (* Entry point to build a single root goal
     Here the downwards "t" parameter is constructed from various components.
     And here we break out of the Builder monad, and revert to the plain
     tenacious monad - ignoring the proxy map or any errors.
  *)
  fun ~jenga_root_path ~job_throttle fs persist memo discovered_graph
    config progress ~demanded ->
    let node = DG.create_root discovered_graph in
    let t = {
      config;
      fs;
      job_throttle;
      persist;
      memo;
      progress;
      jenga_root_path;
      recurse_build_goal = build_goal;
      me = Some demanded;
      discovered_graph;
      node;
    } in
    let builder = build_goal t demanded in
    let tenacious =
      Tenacious.bind (Builder.expose builder) (function
      | Error _ -> Tenacious.return ()
      | Ok (_ :Proxy_map.t) -> Tenacious.return ()
      )
    in
    tenacious


let get_env_option :
    (
      jenga_root_path: Path.X.t ->
      job_throttle: unit Throttle.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      DG.t ->
      Config.t ->
      Progress.t ->
      Env1.t option Tenacious.t
    ) =
  fun ~jenga_root_path ~job_throttle fs persist memo discovered_graph config progress ->
    let node = DG.create_root discovered_graph in
    let t = {
      config;
      fs;
      job_throttle;
      persist;
      memo;
      progress;
      jenga_root_path;
      recurse_build_goal = build_goal;
      me = None;
      discovered_graph;
      node;
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
  asyncronous writers/dumpers
----------------------------------------------------------------------*)

let progress_report_period = sec 1.0

let show_progress_fraction_reports ~fin progress =
  let rec loop () =
    Clock.after progress_report_period >>= fun () ->
    if (!fin) then Deferred.unit else (
      let progress = Progress.snap progress in
      let fraction = Mon.Progress.fraction progress in
      Message.progress ~fraction;
      loop ()
    )
  in
  loop ()


(*----------------------------------------------------------------------
 cycle detection
----------------------------------------------------------------------*)

let look_for_a_cycle : (DG.t -> (DG.Node.t * DG.Node.t list) option) =
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
    walk_list (DG.roots dg) ~k:(fun () ->
      None (* no cycles found *)
    )


let print_a_cycle_if_found dg =
  match (look_for_a_cycle dg) with
  | None -> false
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
    let () =
      let cycle_path_with_repeated_nub = the_nub :: cycle_path in
      Message.message "CYCLIC DEPENDENCIES: %s"
        (String.concat (
          List.map cycle_path_with_repeated_nub ~f:(fun node ->
            let item = DG.lookup_item dg node in
            sprintf "\n- [%s] %s"
              (DG.id_string node)
              (DG.Item.to_string item))))
    in
    true


let cycle_watch_period = sec 10.0

let watch_for_cycles ~cycle_found ~fin dg =
  let rec loop () =
    Clock.after cycle_watch_period >>= fun () ->
    if (!fin) then Deferred.unit else (
      match (print_a_cycle_if_found dg) with
      | false -> loop ()
      | true ->
        let () =
          (* Write message in format suitable for omake-server *)
          let pr fmt = ksprintf (fun s -> Printf.printf "%s\n%!" s) fmt in
          pr "*** OMakeroot error:";
          pr "   dependency cycle; jenga.exe quitting\n";
          pr "*** omake error:";
        in
        Ivar.fill cycle_found ();
        Deferred.unit
    )
  in
  loop ()


(*----------------------------------------------------------------------
  build_once
----------------------------------------------------------------------*)

let build_once :
    (Config.t -> DG.t -> Progress.t -> unit Tenacious.t -> (Heart.t * int) Deferred.t) =
  (* Make a single build using the top level tenacious builder
     Where a single build means we've done all we can
     (maybe we are complete, or maybe some targets are in error)
     given the current state of the file-system.
     And now we are just polling of file-system changes.
  *)
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun config dg progress top_tenacious ->
    let u = genU() in (* count of the times we reach polling *)
    let start_time = Time.now() in
    Tenacious.exec top_tenacious >>| fun ((),heart) ->

    (* to avoid reporting of stale errors etc... *)
    Progress.mask_unreachable progress dg;

    let counts = Progress.snap progress in
    let duration = Time.diff (Time.now()) start_time in
    let effort_string = Effort.Snapped.to_string (snap_all_effort()) in

    let () =
      if Config.full_gc_when_build_done config then (
        Message.message "GC.full_major...";
        Gc.full_major()
      )
    in
    let exit_code =
      if Mon.Progress.completed counts then (
        let total = Mon.Progress.total counts in
        Message.build_done ~duration ~u ~total effort_string;
        Exit_code.build_done
      ) else (
        let fraction = Mon.Progress.fraction counts in
        Message.build_failed ~duration ~u ~fraction effort_string;
        Exit_code.build_failed
      )
    in
    let quiet = Config.quiet config in
    if not quiet then (
      if Config.show_status_all config
      then Progress.message_everything progress
      else Progress.message_errors progress
    );
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

  fun config progress ~jenga_root_path ~top_level_demands fs persist
    ~when_polling ~when_rebuilding ->

    let memo = Memo.create () in
    let discovered_graph = DG.create config in

    let job_throttle =
      let max_concurrent_jobs = Config.j_number config in
      Throttle.create ~continue_on_error:true ~max_concurrent_jobs
    in

    let get_env_opt () =
      get_env_option
        ~jenga_root_path
        ~job_throttle
        fs persist memo discovered_graph
        config progress
    in

    (* construct the top-level tenacious builder only once *)
    let top_tenacious =
      Tenacious.all_unit (
        List.map top_level_demands ~f:(fun demanded ->
          build_one_root_goal
            ~jenga_root_path
            ~job_throttle
            fs persist memo discovered_graph
            config progress ~demanded
        )
      )
    in

    let cycle_found = Ivar.create () in
    let () =
      don't_wait_for (
        Ivar.read cycle_found >>= fun () ->
        Quit.quit Exit_code.cycle_abort;
        Deferred.return ()
      )
    in

    let rec build_and_poll ()  = (* never finishes if polling *)

      (* start up various asyncronous writers/dumpers *)
      let fin = ref false in

      (* async cycle detection; but NO deadlock breaking *)
      don't_wait_for (
        watch_for_cycles ~cycle_found ~fin discovered_graph
      );

      if Config.progress config then (
        don't_wait_for (
          show_progress_fraction_reports ~fin progress
        )
      );

      Tenacious.exec (get_env_opt()) >>= fun (env_opt,__heart) ->

      (* call user build_begin function *)
      run_user_function_from_env_opt env_opt "build_begin" ~f:Env1.build_begin >>= fun () ->

      (* do the build once *)
      build_once config discovered_graph progress top_tenacious >>= fun (heart,exit_code) ->

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
        if Config.show_sensitized config then (
          List.iter (Heart.to_sensitivity_list heart) ~f:(fun desc ->
            Message.sensitized_on ~desc
          )
        );
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
