
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let equal_using_compare compare = fun x1 x2 -> Int.(=) 0 (compare x1 x2)
let equal_pair f g = fun (a1,b1) (a2,b2) -> f a1 a2 && g b1 b2
let equal_list f = fun xs ys -> List.equal xs ys ~equal:f

let (<>) = Int.(<>)

open Description

module Digest = Fs.Digest
module Glob = Fs.Glob
module DG = Discovered_graph



let insensitive_scanners_and_internal_actions =
  match Core.Std.Sys.getenv "JENGA_INSENSITIVE_SCANNERS_AND_INTERNAL_ACTIONS" with
  | None -> false
  | Some _ -> true

(*----------------------------------------------------------------------
  effort
----------------------------------------------------------------------*)

let generation_calls = Effort.Counter.create "gen"
let scanner_calls = Effort.Counter.create "scan"
let external_action_counter = Effort.Counter.create "act"

let the_effort =
  Effort.create [
    Fs.lstat_counter;
    Fs.digest_counter;
    Fs.ls_counter;
    Fs.mkdir_counter;
    generation_calls;
    scanner_calls;
    external_action_counter;
  ]

let effort_string() =
  Effort.Snapped.to_string (Effort.snap the_effort)

let zero_effort() =
  Effort.reset_to_zero the_effort

(*----------------------------------------------------------------------
 setup_command_lookup_path
----------------------------------------------------------------------*)

let setup_command_lookup_path =
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
    Core.Std.Unix.putenv ~key:"PATH" ~data:replacement_path

(*----------------------------------------------------------------------
 new - Env1
----------------------------------------------------------------------*)

type scheme_id = string with sexp, bin_io

module Env1 : sig

  type t
  val of_env : Env.t ->  [ `ok of t | `dups of scheme_id list ]
  val lookup_gen_key : t -> Goal.t -> Gen_key.t option
  val run_generator_scheme : t -> Gen_key.t -> (Rule_generator.t,exn) Result.t
  val run_scanner : t -> Scan_id.t -> Dep.t list Deferred.t
  val run_internal_action : t -> Action_id.t -> unit Deferred.t

end = struct

  type t = {
    lookup_gen_key : Goal.t -> Gen_key.t option;
    run_generator_scheme : Gen_key.t -> (Rule_generator.t,exn) Result.t;
    run_scanner : Scan_id.t -> Dep.t list Deferred.t;
    run_internal_action : Action_id.t -> unit Deferred.t;
  } with fields

  let of_env env =
    let {Env. command_lookup_path; action; scan; schemes} = env in

    (* Each time we construct the Env1 form the Env, we reset the command lookup path.

       It's kind of odd being here, but the env->env1 conversion is done just once when
       the JR.ml is changed by the user, and this is the only time the command_lookup_path
       might get changed.
    *)
    let () =
      match command_lookup_path with
      | None -> ()
      | Some spec -> setup_command_lookup_path spec
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

    (* scanner / internal-action wrapping *)
    let run_scanner id =
      scan (Sexp.of_string (Scan_id.to_string id))
    in
    let run_internal_action id =
      action (Sexp.of_string (Action_id.to_string id))
    in

    let dups = !dups in
    match dups with | _::_ -> `dups dups
    | [] ->
      `ok {
        lookup_gen_key;
        run_generator_scheme;
        run_scanner;
        run_internal_action;
      }

end


(*----------------------------------------------------------------------
  Ruleset
----------------------------------------------------------------------*)

module Ruleset : sig

  type t
  val empty : t
  val create : Rule.t list -> [ `ok of t | `dups of Path.t list ]
  val create_exn : Rule.t list -> t

  val lookup_target : t -> Path.t -> Target_rule.t option
  val lookup_alias : t -> Alias.t -> Dep.t list option

  val rules : t -> Rule.t list

end = struct

  type t = {
    rules  : Rule.t list;
    lookup_target : Path.t -> Target_rule.t option;
    lookup_alias : Alias.t -> Dep.t list option;
  } with fields

  let empty = {
    rules = [];
    lookup_target = (fun _ -> None);
    lookup_alias = (fun _ -> None);
  }

  let create rules =
    let by_target = Path.Table.create () in
    let by_alias = Alias.Table.create () in
    let dups = ref [] in
    let () =
      List.iter rules ~f:(fun rule ->
        match Rule.case rule with
        | `target tr ->
          List.iter (Target_rule.targets tr) ~f:(fun path ->
            match (Hashtbl.add by_target ~key:path ~data:tr) with
            | `Ok -> ()
            | `Duplicate -> dups := path :: !dups
          )
        | `alias (alias,deps) ->
          let deps =
            match (Hashtbl.find by_alias alias) with
            | None -> deps
            | Some prev -> deps @ prev (* merge aliases *)
          in
          Hashtbl.set by_alias ~key:alias ~data:deps
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

  let create_exn xs =
    match create xs with
    | `dups _ -> failwith "Ruleset.create_exn reports dups"
    | `ok x -> x

end

(*----------------------------------------------------------------------
 Pm_key - path or glob
----------------------------------------------------------------------*)

module Pm_key : sig

  type t with sexp, bin_io, compare
  include Comparable_binable with type t := t

  val of_path : Path.t -> t
  val of_glob : Glob.t -> t
  val to_path_exn : t -> Path.t (* for targets_proxy_map *)
  val equal : t -> t -> bool

  val to_string : t -> string

end = struct

  module T = struct
    type t = Path of Path.t | Glob of Glob.t
    with sexp, bin_io, compare
  end
  include T
  include Comparable.Make_binable(T)

  let equal = equal_using_compare compare

  let of_path t = Path t
  let of_glob t = Glob t
  let to_path_exn = function
    | Path x -> x
    | Glob _ -> failwith "Proxy_map.key.to_path_exn"

  let to_string = function
    | Path path -> Path.to_rrr_string path
    | Glob glob -> Glob.to_string glob

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
  val single : Pm_key.t -> Proxy.t -> t
  val empty : t

  type inconsistency = (Pm_key.t * Proxy.t list) list
  with sexp_of

  val create_by_path : PPs.t -> [`ok of t | `err of inconsistency]
  val merge : t list -> [ `ok of t | `err of inconsistency]

  val diff : t -> t -> Pm_key.t list option

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
            | `Left _
            | `Right _ -> None
            | `Unequal _ -> Some k
          )
      )

end

module Root_proxy : sig

  type t with sexp, bin_io, compare
  val create : Digest.t option -> t
  val equal : t -> t -> bool

end = struct

  type t = {
    root : Digest.t option;
  } with sexp, bin_io, compare

  let create root = { root }

  let equal = equal_using_compare compare

end

module Rooted_proxy : sig

  type t with sexp, bin_io, compare
  val create : Root_proxy.t -> Proxy_map.t -> t
  val equal : t -> t -> bool
  val diff : insensitive:bool ->
    t -> t -> [`root_changed | `proxy_map_changed of Pm_key.t list] option

end = struct

  type t = {
    rp : Root_proxy.t;
    pm : Proxy_map.t;
  } with sexp, bin_io, compare

  let create rp pm = { rp; pm; }

  let equal = equal_using_compare compare

  let diff ~insensitive t1 t2 =
    if not (insensitive || Root_proxy.equal t1.rp t2.rp) then Some `root_changed
    else
      match (Proxy_map.diff t1.pm t2.pm) with
      | None -> None
      | Some changed_keys -> Some (`proxy_map_changed changed_keys)

end

module Action_proxy : sig

  type t with sexp, bin_io, compare
  val extern : Xaction.t -> t
  val intern : Action_id.t -> Root_proxy.t -> t
  val to_action : t -> Action.t
  val diff : t -> t -> [`root_changed | `action_changed ] option

end = struct

  type t = X of Xaction.t | I of Action_id.t * Root_proxy.t with sexp, bin_io, compare

  let extern x = X x
  let intern a rp = I (a,rp)

  let diff t1 t2 =
    match t1,t2 with
    | I (i1,rp1), I (i2,rp2) ->
      let insensitive = insensitive_scanners_and_internal_actions in
      if not (insensitive || Root_proxy.equal rp1 rp2) then
        Some `root_changed
      else if Action_id.compare i1 i2 <> 0 then
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

  let to_action = function
    | X x -> Action.xaction x
    | I (i,_) -> Action.internal1 i

end

module Rule_proxy = struct

  type t = {
    targets : Proxy_map.t;
    deps : Proxy_map.t;
    action : Action_proxy.t
  } with sexp, bin_io, compare

  let equal = equal_using_compare compare

end

(*----------------------------------------------------------------------
  (error) reason
----------------------------------------------------------------------*)

(*let tr_to_string tr = Target_rule.to_string tr*)
let tr_to_string tr = (* just show targets or else too verbose *)
  String.concat ~sep:" " (List.map (Target_rule.targets tr) ~f:Path.to_rrr_string)

let item_to_string = function
  | DG.Item.Root -> "ROOT"
  | DG.Item.Scanner scanner -> sprintf "SCANNER: %s" (Scanner.to_string scanner)
  | DG.Item.Dep dep -> (*sprintf "DEP: %s"*) (Dep.to_string dep)
  | DG.Item.Target_rule tr -> sprintf "RULE: %s"  (tr_to_string tr)
  | DG.Item.Gen_key g -> sprintf "GEN: %s" (Gen_key.to_string g)

module Reason = struct

  type t =
  | Error_in_deps                     of (Dep.t * t) list
  | Digest_error
  | Undigestable                      of Fs.Kind.t
  | Glob_error                        of string
  | Jenga_root_problem                of string

  | No_definition_for_alias
  | No_rule_or_source
  | Non_zero_status
  | No_directory_for_target           of string

  | Inconsistent_proxies              of Proxy_map.inconsistency
  | Duplicate_scheme_ids              of scheme_id list

  | Scheme_raised                     of exn
  | Generator_raised                  of exn
  | Internal_action_raised            of exn
  | Running_external_action_raised    of exn
  | Scanner_raised                    of exn

  | Duplicate_rules_for_paths         of Path.t list
  | Rule_failed_to_generate_targets   of Path.t list
  (* | Cycle... *)


  let to_string = function

    | Error_in_deps _ -> "Unable to build dependencies"
    | Digest_error -> "unable to digest file"
    | Undigestable k -> sprintf "undigestable file kind: %s" (Fs.Kind.to_string k)
    | Glob_error s -> sprintf "glob error: %s" s
    | Jenga_root_problem s -> sprintf "Problem with JengaRoot.ml: %s" s

    | No_definition_for_alias -> "No definition found for alias"
    | No_rule_or_source -> "No rule or source found for target"
    | Non_zero_status -> "External action completed with non-zero exit code"
    | No_directory_for_target s -> sprintf "No directory for target: %s" s

    | Duplicate_scheme_ids xs ->
      sprintf "Duplicate schemes with ids: %s"
        (String.concat ~sep:" " (List.map xs ~f:(sprintf "%S")))

    | Inconsistent_proxies inconsistency ->
      sprintf "Inconsistency proxies on keys: %s"
        (String.concat ~sep:" " (List.map inconsistency ~f:(fun (key,_) ->
          Pm_key.to_string key)))

    (* multi line errors *)
    | Scheme_raised exn ->
      sprintf "Generator scheme raised exception:\n%s" (Exn.to_string exn)

    | Generator_raised exn ->
      sprintf "Generator raised exception:\n%s" (Exn.to_string exn)

    | Internal_action_raised exn ->
      sprintf "Internal action raised exception:\n%s" (Exn.to_string exn)

    | Running_external_action_raised exn ->
      sprintf "Running external action raised exception:\n%s" (Exn.to_string exn)

    | Scanner_raised exn ->
      sprintf "Scanner raised exception:\n%s" (Exn.to_string exn)

    | Duplicate_rules_for_paths paths ->
      sprintf "Duplicate rules for paths:\n- %s"
        (String.concat ~sep:"\n- " (List.map paths ~f:Path.to_rrr_string))

    | Rule_failed_to_generate_targets paths ->
      sprintf "Rule failed to generate targets:\n- %s"
        (String.concat ~sep:"\n- " (List.map paths ~f:Path.to_rrr_string))

end

(*----------------------------------------------------------------------
  Progress - progress monitor
----------------------------------------------------------------------*)

(* TODO: Make this status be the same type as Dot.Status *)

module What = struct (* what was build *)
  type t = Source | Something_else
end

module Run_kind = struct
  type t = Generator | Scanner | Action
  with sexp_of
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Status = struct
  type t =
  | Checking
  | Blocked         (* for deps to be checked *)
  | Throttled of Run_kind.t
  | Running of Run_kind.t
  | Internal of Run_kind.t
  | Built of What.t
  | Error of Reason.t
end

module Progress : sig

  type t
  val create : unit -> t

  val set_status : t -> key:Dep.t -> data:Status.t -> unit
  val mask_unreachable : t -> DG.t -> unit

  val message_errors : t -> unit

  val dg_status : t  -> Dep.t -> Dot.Status.t

  module Counts : sig
    type t with bin_io
    val to_string : t -> string
    val fraction : t -> (int*int)
    val completed : t -> bool
    val total : t -> int
  end

  val snap : t -> Counts.t
  val readme : string list

end = struct

  type t = {
    status : Status.t Dep.Table.t;
    mutable mask : Dep.Hash_set.t;
  }

  let create () = {
    status = Dep.Table.create();
    mask = Dep.Hash_set.create () ;
  }

  let set_status t = Hashtbl.set t.status

  let mask_unreachable t dg =
    let mask_candidates = Dep.Hash_set.create () in
    Hashtbl.iter t.status ~f:(fun ~key:dep ~data:_ ->
      Hash_set.add mask_candidates dep; (* add dep as candiate for mask *)
    );
    DG.iter_reachable dg ~f:(fun node ->
      match (DG.lookup_item dg node) with
      | DG.Item.Dep dep ->
        Hash_set.remove mask_candidates dep (* dep is reachable; dont remove *)
      | _ -> ()
    );
    t.mask <- mask_candidates

  let find_unmasked t dep =
    if not (Hash_set.mem t.mask dep)
    then Hashtbl.find t.status dep
    else None

  let iter_unmasked t ~f =
    Hashtbl.iter (t.status) ~f:(fun ~key ~data ->
      if not (Hash_set.mem t.mask key) then
        f ~key ~data
    )

  let dg_status t dep =
    match (find_unmasked t dep) with
    | None -> Dot.Status.Unknown
    | Some status ->
      let cat x =  Dot.Status.Cat x in
      match status with
      | Status.Checking                       -> cat Dot.Catagory.Waiting
      | Status.Blocked                        -> cat Dot.Catagory.Waiting
      | Status.Throttled _                    -> cat Dot.Catagory.Waiting
      | Status.Running _                      -> cat Dot.Catagory.Working
      | Status.Internal _                     -> cat Dot.Catagory.Working
      | Status.Built _                        -> cat Dot.Catagory.Good
      | Status.Error (Reason.Error_in_deps _) -> cat Dot.Catagory.Child_error
      | Status.Error _                        -> cat Dot.Catagory.Error

  let final_status_to_string = function
    | Status.Checking       -> "unexpectedly Checking"
    | Status.Blocked        -> "unexpectedly Blocked"
    | Status.Throttled k    -> sprintf "unexpectedly Throttled: %s" (Run_kind.to_string k)
    | Status.Running k      -> sprintf "unexpectedly Running: %s" (Run_kind.to_string k)
    | Status.Internal k     -> sprintf "unexpectedly Internal: %s" (Run_kind.to_string k)
    | Status.Built _        -> "built"
    | Status.Error reason   -> Reason.to_string reason

  let suppress_reports_for = function
    (* suppress reporting errors for
       - stuff which was built ok
       - error_in_deps errors (they'll be reported at the child)
    *)
    | Status.Built _ -> true
    | Status.Error (Reason.Error_in_deps _) -> true
    | _ -> false

  let message_errors t =
    iter_unmasked t ~f:(fun ~key:dep ~data:status ->
      if suppress_reports_for status then () else
        Message.error "%s: %s" (Dep.to_string dep) (final_status_to_string status)
    )

  module Counts = struct

    type t = {
      (* still working... *)
      checking  : int;
      blocked   : int;
      throttled : int;
      running   : int; (* external scanner/action *)
      internal  : int; (* internal generator/scanner/action *)
      (* done... *)
      source    : int;
      built     : int; (* non source *)
      error     : int;
      failure   : int; (* error in dep *)
    } with bin_io

    let total {
      checking;
      blocked;
      throttled;
      running;
      internal;
      source;
      built;
      error;
      failure
    } =
      source + built + error + failure +
      checking + blocked + throttled + running + internal

    let good t = t.built + t.source
    let fraction t = (good t) , (total t)

    let completed t = Int.equal (good t) (total t)

    let to_labelled {
      checking;
      blocked;
      throttled;
      running;
      internal;
      source;
      built;
      error;
      failure
    } = [

      ("built", built,
       "target is built and up-to-date");

      ("source", source,
       "target is a source file");

      ("error", error,
       "leaf build error; i.e. non-zero exit or target not created");

      ("fail", failure,
       "unable to build because one or more (transitive) dependencies has an error");

      ("check", checking,
       "target and its dependencies are being checked to ensure they are up-to-date");

      ("block", blocked,
       "the build of this target is blocked on one or more dependencies");

      ("wait", throttled,
       "target is ready to run an external command, but is limited by the -j threshold");

      ("run", running,
       "target is running an external build command (action or scanner)");

      ("user", internal,
       "target is running user ML code in the JengaRoot.ml");

    ]

    let to_string t =
      let justify =
        let (>) = Int.(>) in
        let len n =
          if n>99999 then 6 else
          if n>9999 then 5 else
          if n>999 then 4 else
          if n>99 then 3 else
          if n>9 then 2 else
            1
        in
        let max = len (total t) in
        fun n ->
          let i = max - len n in
          assert(not (0>i));
          let spaces = String.make i ' ' in
          spaces
      in
      String.concat ~sep:", "
        (List.map (to_labelled t) ~f:(fun (s,n,_) -> sprintf "%s=%s%d" s (justify n) n))

  end

  let snap t =
    let checking = ref 0 in
    let blocked = ref 0 in
    let throttled = ref 0 in
    let running = ref 0 in
    let internal = ref 0 in
    let source = ref 0 in
    let built = ref 0 in
    let error = ref 0 in
    let failure = ref 0 in
    iter_unmasked t
      ~f:(fun ~key:_ ~data:status ->
        let x =
          match status with
          | Status.Checking                         -> checking
          | Status.Blocked                          -> blocked
          | Status.Throttled _                      -> throttled
          | Status.Running _                        -> running
          | Status.Internal _                       -> internal
          | Status.Built What.Source                -> source
          | Status.Built _                          -> built
          | Status.Error (Reason.Error_in_deps _)   -> failure
          | Status.Error _                          -> error
        in incr x
      );
    {Counts.
     checking   = !checking;
     blocked    = !blocked;
     throttled  = !throttled;
     running    = !running;
     internal   = !internal;
     source     = !source;
     built      = !built;
     error      = !error;
     failure    = !failure;
    }

  let readme =
    let progress = create() in (* dummy needed to get the count labels & descriptions *)
    let counts = snap progress in
    let labelled = Counts.to_labelled counts in
    let justify =
      let max =
        List.fold ~init:0 ~f:(fun x y -> if Int.(x > y) then x else y)
          (List.map labelled ~f:(fun (lab,_,_) -> String.length lab))
      in
      fun s ->
        let i = max - String.length s in
        assert(Int.(i >= 0));
        let spaces = String.make i ' ' in
        spaces
    in
    List.map labelled ~f:(fun (lab,_,desc) ->
      sprintf "- %s %s: %s" lab (justify lab) desc
    )

end

(*----------------------------------------------------------------------
 Run_external_job
----------------------------------------------------------------------*)

exception Shutdown

module Run_external_job : sig

  val shell :
    delay_for_dev:Time.Span.t option ->
    need:string ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    (unit, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t

  val shell_stdout :
    delay_for_dev:Time.Span.t option ->
    need:string ->
    dir:Path.t ->
    prog:string ->
    args:string list ->
    (string, [ `non_zero_status | `other_error of exn ]) Result.t Deferred.t

end = struct

  let lines s =
    match s with
    | "" -> []
    | "\n" -> [""]
    | _ ->
      let s =
        match String.chop_suffix s ~suffix:"\n" with
        | None -> s
        | Some s -> s
      in
      String.split s ~on:'\n'

  let run =
    fun ~stdout_expected ~get_result ~delay_for_dev ~need ~dir ~prog ~args ->

      let where = Path.to_rrr_string dir in

      let job_start =
        Message.job_started ~need ~stdout_expected ~where ~prog ~args
      in
      let start_time = Time.now() in

      Effort.track external_action_counter (fun () ->
        try_with (fun () ->
          (match delay_for_dev with
          | None -> return ()
          | Some seconds -> Clock.after seconds
          ) >>= fun () ->
          Async_shell.run_full_and_error
            ~working_dir:(Path.to_absolute_string dir) prog args
        )
      ) >>= fun result ->

      match Heart.is_broken Heart.is_shutdown with
      | true  ->
        return (Error (`other_error Shutdown))

      | false ->

        let duration = Time.diff (Time.now()) start_time in

        match result with
        | Ok (stdout,stderr) ->
          let result = get_result ~stdout in
          let stdout = lines stdout in
          let stderr = lines stderr in
          let outcome = `success in
          Message.job_finished job_start ~outcome ~duration ~stdout ~stderr;
          return (Ok result)

        | Error exn ->
          let exn = Monitor.extract_exn exn in
          let module SP = Async_shell.Process in

          match exn with
          | Async_shell.Process.Failed res ->
            let outcome = `error (SP.status_to_string res.SP.status) in
            let stdout = lines res.SP.stdout in
            let stderr = lines res.SP.stderr in
            Message.job_finished job_start ~outcome ~duration ~stdout ~stderr;
            return (Error `non_zero_status)

          | _ -> (* what? *)
            let outcome = `error (Exn.to_string exn) in
            let stdout = [] in
            let stderr = [] in
            Message.job_finished job_start ~outcome ~duration ~stdout ~stderr;
            return (Error (`other_error exn))



  let shell =
    (* ignore stdout for commands run for effect *)
    run
      ~stdout_expected:false
      ~get_result:(fun ~stdout:_ -> ())

  let shell_stdout =
    (* grab stdout for commands run as scanner *)
    run
      ~stdout_expected:true
      ~get_result:(fun ~stdout -> stdout)

end

(*----------------------------------------------------------------------
  Builder
----------------------------------------------------------------------*)

module Builder : sig (* layer error monad within tenacious monad *)

  type 'a t
  val wrap : ('a, Reason.t) Result.t Tenacious.t -> 'a t
  val expose : 'a t -> ('a, Reason.t) Result.t Tenacious.t

  val of_tenacious : 'a Tenacious.t -> 'a t
  val expose_unit : 'a t -> unit Tenacious.t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val error : Reason.t -> 'a t

  val for_all_collect_errors :
    'a list -> f:('a -> 'b t) -> ('b list * ('a * Reason.t) list) t

  val of_deferred : (unit -> 'a Deferred.t) -> 'a t
  val try_deferred : (unit -> 'a Deferred.t) -> err:(exn -> Reason.t) -> 'a t

  val desensitize : 'a t -> ('a * Heart.t) t
  val sensitize : Heart.t -> unit t

  val before_redo : 'a t -> f:(unit -> unit) -> 'a t

end = struct

  type 'a t = ('a, Reason.t) Result.t Tenacious.t

  let wrap t = t
  let expose t = t

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

  let error e = Tenacious.return (Error e)

  let for_all_collect_errors xs ~f =
    Tenacious.bind
      (Tenacious.all (List.map xs ~f:(fun x ->
        Tenacious.bind (f x) (fun ore ->
          Tenacious.return (x, ore)
        ))))
      (fun tagged ->
        let good,bad =
          List.partition_map tagged ~f:(function
          | (_,Ok res) -> `Fst res
          | (x,Error e) -> `Snd (x,e))
        in
        return (good,bad)
      )

  let of_deferred f =
    Tenacious.lift (fun () ->
      f () >>= fun x ->
      Deferred.return (Ok x,Heart.unbreakable)
    )

  let try_deferred f ~err =
    Tenacious.lift (fun () ->
      Monitor.try_with f >>= function
      | Ok x -> Deferred.return (Ok x,Heart.unbreakable)
      | Error exn -> Deferred.return (Error (err exn), Heart.unbreakable)
    )

  let desensitize t = (* if not error *)
    Tenacious.lift_cancelable (fun ~cancel ->
      Deferred.bind (Tenacious.exec_cancelable ~cancel t) (function
      | None -> Deferred.return None
      | Some (ore,heart) ->
        match ore with
        | Error e -> Deferred.return (Some (Error e, heart))
        | Ok x -> Deferred.return (Some (Ok (x,heart), Heart.unbreakable))
      )
    )

  let sensitize heart =
    Tenacious.lift (fun () -> Deferred.return (Ok (), heart))

  let before_redo = Tenacious.before_redo

end

let return = Builder.return
let ( *>>= ) = Builder.bind
let error = Builder.error

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
  | Jenga_root_changed
  | Action_changed_was              of Action.t
  | Deps_have_changed               of Pm_key.t list
  | Targets_missing                 of Path.t list
  | Targets_not_as_expected         of Path.t list
  with sexp

  (*let to_string t = Sexp.to_string (sexp_of_t t)*)

  let to_string = function
  | No_record_of_being_run_before   -> "first"
  | Jenga_root_changed              -> "JengaRoot"
  | Action_changed_was _            -> "action"
  | Deps_have_changed _             -> "deps"
  | Targets_missing _               -> "missing"
  | Targets_not_as_expected _       -> "unexpected"

end

(*----------------------------------------------------------------------
  File system interface
----------------------------------------------------------------------*)

let digest_path
    : (Fs.t -> Path.t -> [ `file of Digest.t | `missing ] Builder.t) =
  fun fs path ->
    Builder.of_tenacious (Fs.digest_file fs ~file:path) *>>= function
    | `stat_error _   -> return `missing
    | `undigestable k -> error (Reason.Undigestable k)
    | `digest_error _ -> error Reason.Digest_error
    | `digest digest  -> return (`file digest)

let look_for_source : (Fs.t -> Path.t -> Proxy.t option Builder.t) =
  fun fs path ->
    digest_path fs path *>>= function
    | `file digest    -> return (Some (Proxy.of_digest digest))
    | `missing        -> return None

let need_glob : (Fs.t -> Glob.t -> (What.t * Proxy_map.t) Builder.t) =
  fun fs glob ->
    Builder.of_tenacious (Fs.list_glob fs glob) *>>= function
    | `stat_error _   -> error (Reason.Glob_error "no such directory")
    | `not_a_dir      -> error (Reason.Glob_error "not a directory")
    | `listing_error _-> error (Reason.Glob_error "unable to list")
    | `listing listing ->
      return (What.Something_else,
              Proxy_map.single (Pm_key.of_glob glob) (Proxy.of_listing listing))

let ensure_directory : (Fs.t -> dir:Path.t -> unit Builder.t) =
  fun fs ~dir ->
    Builder.of_tenacious (Fs.ensure_directory fs ~dir) *>>= function
    | `ok -> return ()
    | `not_a_dir -> error (Reason.No_directory_for_target "not a directory")
    | `failed -> error (Reason.No_directory_for_target "failed to create")

(*----------------------------------------------------------------------
  Persist - static cache - saved to file between runs
----------------------------------------------------------------------*)

module Persist = struct

  (* The values in these hashtable must be sexp convertable,
     so they can be saved to file.  *)

  type t = {
    scanned     : (Rooted_proxy.t * Dep.t list) Scanner.Table.t;
    generated   : (Rooted_proxy.t * Rule.t list) Gen_key.Table.t;
    actioned    : Rule_proxy.t Path.Table.t;
  } with sexp, bin_io

  let create () = {
    scanned = Scanner.Table.create();
    generated = Gen_key.Table.create();
    actioned = Path.Table.create();
  }

  let equal t1 t2 =
    Hashtbl.equal t1.scanned t2.scanned
      (equal_pair Rooted_proxy.equal (equal_list Dep.equal))
    &&
    Hashtbl.equal t1.generated t2.generated
      (equal_pair Rooted_proxy.equal (equal_list Rule.equal))
    &&
    Hashtbl.equal t1.actioned t2.actioned
      Rule_proxy.equal

  let copy t = {
    scanned = Scanner.Table.copy t.scanned;
    generated = Gen_key.Table.copy t.generated;
    actioned = Path.Table.copy t.actioned;
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
    generating  : (Ruleset.t   Builder.t * DG.Node.t) Gen_key.Table.t;
    scanning    : (Dep.t list   Builder.t * DG.Node.t) Scanner.Table.t;
    building    : (Proxy_map.t  Builder.t * DG.Node.t) Dep.Table.t;
    ruling      : (PPs.t        Builder.t * DG.Node.t) Target_rule.Table.t;
    root        : (Env1.t * Root_proxy.t) Builder.t option ref;
  }

  let create () = {
    generating = Gen_key.Table.create();
    scanning = Scanner.Table.create();
    building = Dep.Table.create();
    ruling = Target_rule.Table.create();
    root = ref None;
  }

end

(*----------------------------------------------------------------------
  t - The downwards bucket type
----------------------------------------------------------------------*)

type t = {

  config : Config.t; (* messaging choices *)
  fs : Fs.t; (* file system *)
  throttle : unit Throttle.t; (* for external jobs *)
  persist : Persist.t; (* persisant cache *)
  memo : Memo.t; (* dynmaic cache *)
  progress : Progress.t; (* track state of each dep being built *)
  jenga_root_path : Path.LR.t; (* access to the JengaRoot.ml *)

  (* recursive calls to build_dep go via here - avoids having big mutual let rec *)
  recurse_build_dep : (t -> Dep.t -> Proxy_map.t Builder.t);

  (* know dep being worked on, so can set the status *)
  me : Dep.t;

  (* discovered_graph structure & current node - used for cycle checking *)
  discovered_graph : DG.t;
  node : DG.Node.t;

} with fields

let set_status t status =
  Progress.set_status t.progress ~key:(t.me) ~data:status

let push_dependency t item =
  {t with node = DG.create_dependency t.discovered_graph t.node item}


let scanned t = t.persist.Persist.scanned
let generated t = t.persist.Persist.generated
let actioned t = t.persist.Persist.actioned

let memo_generating t = t.memo.Memo.generating
let memo_scanning t = t.memo.Memo.scanning
let memo_building t = t.memo.Memo.building
let memo_ruling t = t.memo.Memo.ruling
let memo_root t = t.memo.Memo.root

let build_dep : (t ->  Dep.t -> Proxy_map.t Builder.t) =
  fun t dep -> t.recurse_build_dep t dep


let enqueue_with_shutdown t run_kind f =
  set_status t (Status.Throttled run_kind);
  Throttle.enqueue t.throttle (fun () ->
    if Heart.is_broken Heart.is_shutdown
    then (
      Deferred.return (Error (`other_error Shutdown));
    )
    else (
      set_status t (Status.Running run_kind);
      f () >>= fun res ->
      set_status t Status.Checking;
      Deferred.return res
    )
  )

(*----------------------------------------------------------------------
  running things - generator, scanners, actions
----------------------------------------------------------------------*)

let run_generator : (t -> RR.t -> Gen_key.t -> Rule_generator.t -> Ruleset.t Builder.t) =
  fun t rr gen_key generator ->
    set_status t (Status.Internal Run_kind.Generator);
    Message.reason "Generating rules: %s [%s]"
      (Gen_key.to_string gen_key)
      (RR.to_string rr);
    Builder.try_deferred ~err:(fun exn -> Reason.Generator_raised exn) (fun () ->
      Effort.track generation_calls (fun () ->
        Rule_generator.gen generator
      )
    ) *>>= fun rules ->
    match (Ruleset.create rules) with
    | `ok ruleset -> return ruleset
    | `dups paths -> error (Reason.Duplicate_rules_for_paths paths)


exception Scanning_with_internal_action_not_supported of Action_id.t

let run_scanner :
    (t -> RR.t -> Env1.t -> Scanner.t -> Dep.t list Builder.t) =
  fun t rr env1 scanner ->
    let message() =
      Message.reason "Scanning: %s [%s]" (Scanner.to_string scanner) (RR.to_string rr)
    in
    match scanner with
    | `old_internal scan_id ->
      Builder.try_deferred ~err:(fun exn -> Reason.Scanner_raised exn) (fun () ->
        message();
        set_status t (Status.Internal Run_kind.Scanner);
        Effort.track scanner_calls (fun () ->
          Env1.run_scanner env1 scan_id
        )
      )
    | `local_deps (dir1,action) ->
      match Action.case action with
      (* todo - think about combining (internal) scanner-actions with internal action *)
      | `id action_id ->
        error (Reason.Scanner_raised
                 (Scanning_with_internal_action_not_supported action_id))
      | `xaction x ->
        let delay_for_dev = Config.delay_for_dev t.config in
        let need = "scanner" in
        let {Xaction.dir;prog;args} = x in
        Builder.of_deferred (fun () ->
          enqueue_with_shutdown t Run_kind.Scanner (fun () ->
            message();
            Run_external_job.shell_stdout ~delay_for_dev ~need ~dir ~prog ~args
          )
        ) *>>= function
        | Ok stdout                  -> return (Dep.parse_string_as_deps ~dir:dir1 stdout)
        | Error `non_zero_status     -> error Reason.Non_zero_status
        | Error (`other_error exn)   -> error (Reason.Running_external_action_raised exn)


let run_action :
    (t -> RR.t -> Env1.t -> Action.t -> targets:Path.t list -> need:string ->
     unit Builder.t) =
  fun t rr env1 action ~targets ~need ->
    let message() =
      Message.reason "Building: %s [%s]"
        (String.concat ~sep:" " (List.map targets ~f:Path.to_rrr_string))
        (RR.to_string rr)
    in
    match Action.case action with
    | `id action_id ->
      Builder.try_deferred ~err:(fun exn -> Reason.Internal_action_raised exn) (fun ()->
        message();
        set_status t (Status.Internal Run_kind.Action);
        Env1.run_internal_action env1 action_id
      )
    | `xaction x ->
      let delay_for_dev = Config.delay_for_dev t.config in
      let {Xaction.dir;prog;args} = x in
      Builder.of_deferred (fun () ->
        enqueue_with_shutdown t Run_kind.Action (fun () ->
          message();
          Run_external_job.shell ~delay_for_dev ~need ~dir ~prog ~args
        )
      ) *>>= function
      | Ok ()                      -> return ()
      | Error `non_zero_status     -> error Reason.Non_zero_status
      | Error (`other_error exn)   -> error (Reason.Running_external_action_raised exn)


let run_action :
    (t -> RR.t -> Env1.t -> Action.t -> targets:Path.t list -> need:string ->
     unit Builder.t) =
  (* After running an action, synchronise until all inotify events triggered while the
     action was run have been delivered (and acted upon) by this process *)
  fun t rr env1 action ~targets ~need ->
    let sync_contents = Action.to_string action in
    Builder.wrap (
      Fs.sync_inotify_delivery t.fs ~sync_contents (
        Builder.expose (
          run_action t rr env1 action ~targets ~need
        )
      )
    )


(*----------------------------------------------------------------------
  jenga_root
----------------------------------------------------------------------*)

let digest_path_lr fs path_lr =
  match Path.LR.case path_lr with
  | `local path -> digest_path fs path *>>= fun x -> return (Some x)
  | `remote _ ->
    (* remote paths are not properly supported yet
       - just here to allow regression tests to use external_jenga_root
       Not working means
       - we wont be sensitive to changes in a remote jenga_root
       - either for polling or restart
    *)
    Builder.return None

let jenga_root : (t -> (Env1.t * Root_proxy.t) Builder.t) =
  (* wrap up the call to [Load_root.get_env] into a tenacious builder,
     which will reload any time the JengaRoot.ml is modified *)
  fun t ->
    (
      digest_path_lr t.fs t.jenga_root_path *>>= function
      | None -> return None
      | Some res -> match res with
        (*| `directory -> error (Reason.Jenga_root_problem "is a directory")*)
        | `missing -> error (Reason.Jenga_root_problem "missing")
        | `file digest -> return (Some digest)
    )
    *>>= fun digest_opt ->
    Builder.of_deferred (fun () -> Load_root.get_env t.jenga_root_path) *>>= function
    | Error _ -> error (Reason.Jenga_root_problem "failed to load")
    | Ok env ->
      match (Env1.of_env env) with
      | `dups xs -> error (Reason.Duplicate_scheme_ids xs)
      | `ok env1 -> return (env1,Root_proxy.create digest_opt) (* all ok *)

let jenga_root : (t -> (Env1.t * Root_proxy.t) Builder.t) =
  (* Memoization of jenga_root is simpler that other cases, becasuse:
     - There is only one; we don't need a hashtable, just a ref.
     - The root has no deps, so there is no chance of cycles,
  *)
  fun t ->
    match !(memo_root t) with
    | Some builder  -> builder
    | None ->
      let builder = jenga_root t in
      memo_root t := Some builder;
      builder


(*----------------------------------------------------------------------
  build
----------------------------------------------------------------------*)

let share_and_check_for_cycles :
    (* memoize / check for cycles...
       3 places where this function is called, wraps for:
       - build_dep
       - generate_ruleset_if_necessary
       - run_target_rule_action_if_necessary
       Which are the 3 places in the build description where sharing can be encountered
       - deps can be shared betweenmultiple rules/scanner/aliase
       - rule-generation is shared betwen all goals in a gen_key (scheme+dir)
       - rules are shared between the multiple targets that the rule generates
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
        set_status t Status.Blocked;
        DG.link_dependants_no_cycle_check node ~additional:t.node;
        builder;
      end
    | None ->
      let t = push_dependency t item in
      let builder = f t in (* critical - [f] must be called only once *)
      let builder =
        Builder.before_redo builder
          ~f:(fun () ->
            (*Message.message "remove_all_dependencies: %s" (item_to_string item);*)
            DG.remove_all_dependencies t.node;
          )
      in
      (* we use add_exn, because each key will be memoized exactly once *)
      Hashtbl.add_exn memo ~key ~data:(builder,t.node);
      builder


let build_deps : (t -> Dep.t list -> Proxy_map.t Builder.t) =
  (* Build a collection of [deps] in parallel.
     Error if any of [deps] errors
     - but for a new reason: [Error_in_deps], listing those deps in error.
  *)
  fun t deps ->
    set_status t Status.Blocked;
    Builder.for_all_collect_errors deps ~f:(build_dep t) *>>= fun (pms,errors) ->
    set_status t Status.Checking;
    match errors with
    | _::_ -> error (Reason.Error_in_deps errors)
    | [] ->
      match (Proxy_map.merge pms) with
      | `err inconsistency -> error (Reason.Inconsistent_proxies inconsistency)
      | `ok pm -> return pm


let generate_ruleset_if_necessary : (t -> Gen_key.t -> Ruleset.t Builder.t) =
  (* Run rule generation (for a goal) iff:
     - it has never been run before
     - or the proxy indicated it is out of date.
     Record a successful run in the persistent state.
  *)
  fun t gen_key ->
    jenga_root t *>>= fun (env1,root_proxy) ->
    match (Env1.run_generator_scheme env1 gen_key) with
    | Error exn -> error (Reason.Scheme_raised exn)
    | Ok generator ->
      let generator_deps = Rule_generator.deps generator in
      build_deps t generator_deps *>>= fun proxy_map ->
      let rooted_proxy = Rooted_proxy.create root_proxy proxy_map in
      let run_and_cache rr =
        run_generator t rr gen_key generator *>>= fun ruleset ->
        let rules = Ruleset.rules ruleset in
        set_status t Status.Checking;
        Hashtbl.set (generated t) ~key:gen_key ~data:(rooted_proxy,rules);
        return ruleset
      in
      match (Hashtbl.find (generated t) gen_key) with
      | None -> run_and_cache RR.No_record_of_being_run_before
      | Some (prev,rules) ->
        let insensitive = false in (* always be correct/conservative for generators *)
        match Rooted_proxy.diff ~insensitive prev rooted_proxy with
        | Some `root_changed -> run_and_cache RR.Jenga_root_changed
        | Some (`proxy_map_changed keys) -> run_and_cache (RR.Deps_have_changed keys)
        | None ->
          (* ok to use create_exn because there can be no duplicates
             in the rules we saved in the generated cache *)
          let ruleset = Ruleset.create_exn rules in
          return ruleset (* Up to date; dont run anything *)


let generate_ruleset_if_necessary : (t -> Gen_key.t -> Ruleset.t Builder.t) =
  (* memo *)
  fun t gen_key ->
    share_and_check_for_cycles t
      ~key: gen_key
      ~memo: (memo_generating t)
      ~item: (DG.Item.Gen_key gen_key)
      ~f: (fun t -> generate_ruleset_if_necessary t gen_key)


let run_scanner_if_necessary : (t -> Dep.t list -> Scanner.t -> Dep.t list Builder.t) =
  (*
    Build the scanner's dependencies
    Maybe run the scanner;

    Run a scanner iff
     - it has never been run before
     - or the scanner_proxy indicated it is out of date.
     Record a successful run in the persistent state.

  *)
  fun t deps scanner ->
    jenga_root t *>>= fun (env1,root_proxy) ->
    let root_proxy =
      match scanner with
      | `old_internal _ -> root_proxy
      (* external scanners are not dependant on the JengaRoot.ml *)
      | `local_deps _ -> Root_proxy.create None
    in
    build_deps t deps *>>= fun proxy_map ->
    let rooted_proxy = Rooted_proxy.create root_proxy proxy_map in
    let run_and_cache rr =
      run_scanner t rr env1 scanner *>>= fun scanned_deps ->
      set_status t Status.Checking;
      Hashtbl.set (scanned t) ~key:scanner ~data:(rooted_proxy,scanned_deps);
      return scanned_deps
    in
    match (Hashtbl.find (scanned t) scanner) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some (prev,scanned_deps) ->
      let insensitive = insensitive_scanners_and_internal_actions in
      match Rooted_proxy.diff ~insensitive prev rooted_proxy with
      | Some `root_changed -> run_and_cache RR.Jenga_root_changed
      | Some (`proxy_map_changed keys) -> run_and_cache (RR.Deps_have_changed keys)
      | None ->
        return scanned_deps (* Up to date; dont run anything *)


let run_scanner_if_necessary : (t -> Dep.t list -> Scanner.t -> Dep.t list Builder.t) =
  (* memo!

     Scanners must have their own node in the build graph, even though this node can have
     only one dependant.

     This is because when reconsidering the dependant of the scanner, all dependency edges
     are removed from the discovered_graph, with the expectation that they will be set-up
     again (maybe differently) when the scanner is reconsidered. But if there is no change
     in the scanner itself the graph-links for static-dependencies of the scanner wont get
     reinstated for the dependant.

     By having a separate node for the scanner-dep itself. The scanners single dependant
     will depend on the scanner-node, and only this link will be removed & reinstated.
  *)
  fun t deps scanner ->
    share_and_check_for_cycles t
      ~key: scanner
      ~memo: (memo_scanning t)
      ~item: (DG.Item.Scanner scanner)
      ~f: (fun t -> run_scanner_if_necessary t deps scanner)


let build_scanner : (t -> Dep.t list -> Scanner.t -> (What.t * Proxy_map.t) Builder.t) =
  (*
    Build the scanner's dependencies
    Maybe run the scanner;
    Build the scanned dependencies
  *)
  fun t deps scanner ->
    build_deps t deps *>>= fun _proxy_map ->
    run_scanner_if_necessary t deps scanner *>>= fun scanned_deps ->
    build_deps t scanned_deps *>>= fun pm ->
    return (What.Something_else,
            pm)



let check_targets :
    (t -> Path.t list -> [ `ok of PPs.t | `missing of Path.t list] Builder.t) =
  (* Check the targets a rule claims to build.
     - this function may be called before the action is run.
     - and will definitely be called after an action successfully completes.
  *)
  fun t paths ->
    build_all_in_sequence paths ~f:(fun path ->
      look_for_source t.fs path *>>= fun res -> return (path,res)
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


let ensure_target_dirs : (t -> Path.t list -> unit Builder.t) =
  fun t targets ->
    build_all_in_sequence targets ~f:(fun target ->
      let dir = Path.dirname target in
      ensure_directory t.fs ~dir
    ) *>>= fun (_:unit list) ->
    return ()


let prevent_action_overlap
    : (Fs.t -> targets:Path.t list -> unit Builder.t -> unit Builder.t) =
  (* Prevent running overlapping actions for the same targets *)
  fun fs ~targets builder ->
    Builder.of_tenacious (
      Tenacious.prevent_overlap
        ~table:(Fs.active_targets fs)
        ~keys:targets
        ~notify_wait:(fun key ->
          Message.message "waiting for action to complete for: %s"
            (Path.to_rrr_string key);
        )
        (*~notify_add:(fun target ->
          Message.message "target: %s, SET active" (Path.to_rrr_string target);
        )
        ~notify_rem:(fun target ->
          Message.message "target: %s, RM active" (Path.to_rrr_string target);
        )*)
        (Builder.expose_unit builder)
    ) *>>= fun () ->
    builder


let run_target_rule_action_if_necessary
    : (t -> Target_rule.t -> need:string -> PPs.t Builder.t) =
  (* run a rule/action, iff:
     - we have no record of running it before
     - one of its dependencies has changed
     - the action has changed (for an internal action; JengaRoot.ml has changed)
     - one of the targets is missinge
     - one of the targets is different from expected
     Record a successful run in the persistent state.
  *)
  fun t tr ~need ->
    let (targets,deps,action) = Target_rule.triple tr in
    ensure_target_dirs t targets *>>= fun () ->
    build_deps t deps *>>= fun deps_proxy_map ->
    jenga_root t *>>= fun (env1,root_proxy) ->
    let action_proxy =
      match Action.case action with
      | `xaction x -> Action_proxy.extern x
      | `id i -> Action_proxy.intern i root_proxy
    in
    (* The persistent caching is keyed of the [head_target] *)
    let head_target,other_targets = Target_rule.head_and_rest_targets tr in
    let run_and_cache rr =
      prevent_action_overlap t.fs ~targets (
        run_action t rr env1 action ~targets ~need
      ) *>>= fun () ->
      check_targets t targets *>>= function
      | `missing paths -> error (Reason.Rule_failed_to_generate_targets paths)
      | `ok path_tagged_proxys ->
        match (Proxy_map.create_by_path path_tagged_proxys) with
        | `err inconsistency -> error (Reason.Inconsistent_proxies inconsistency)
        | `ok targets_proxy_map ->
          let rule_proxy = {
            Rule_proxy.
            targets = targets_proxy_map;
            deps = deps_proxy_map;
            action = action_proxy;
          }
          in
          Hashtbl.set (actioned t) ~key:head_target ~data:rule_proxy;
          (* We remove data associated with the [other_targets]. Its not essential for
             correctness, but it avoid cruft from building up in the persistent state *)
          List.iter other_targets ~f:(fun other -> Hashtbl.remove (actioned t) other);
          return path_tagged_proxys
    in
    match (Hashtbl.find (actioned t) head_target) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some prev ->
      match (Action_proxy.diff prev.Rule_proxy.action action_proxy ) with
      | Some `action_changed -> let old = Action_proxy.to_action prev.Rule_proxy.action in
                                run_and_cache (RR.Action_changed_was old)
      | Some `root_changed -> run_and_cache RR.Jenga_root_changed
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
            | `err inconsistency -> error (Reason.Inconsistent_proxies inconsistency)
            | `ok targets_proxy_map ->
              match (Proxy_map.diff prev.Rule_proxy.targets targets_proxy_map) with
              | Some keys ->
                let paths = List.map keys ~f:Pm_key.to_path_exn in
                run_and_cache (RR.Targets_not_as_expected paths)
              | None ->
                (* Everything is as it should be! re-sensitize to the targets. *)
                if Config.show_checked t.config then  (
                  Message.message "NOT RUNNING: %s" (Action.to_string action);
                );
                Builder.sensitize heart *>>= fun () ->
                return path_tagged_proxys


let run_target_rule_action_if_necessary
    : (t -> Target_rule.t -> need:string -> PPs.t Builder.t) =
  fun t tr ~need ->
    (* memo! *)
    share_and_check_for_cycles t
      ~key: tr
      ~memo: (memo_ruling t)
      ~item: (DG.Item.Target_rule tr)
      ~f: (fun t -> run_target_rule_action_if_necessary t tr ~need)


let build_using_target_rule :
    (* build a path, using the a specified rule
       return a proxy_map specific to the target of interest
    *)
    (t -> Target_rule.t -> demanded:Path.t -> Proxy_map.t Builder.t) =
  fun t tr ~demanded ->
    let need = Path.basename demanded in
    run_target_rule_action_if_necessary t tr ~need *>>= fun path_tagged_proxys ->
    let (path,proxy) =
      List.find_exn path_tagged_proxys ~f:(fun (path,_) -> Path.equal path demanded)
    in
    return (Proxy_map.single (Pm_key.of_path path) proxy)


let build_goal : (t -> Goal.t -> (What.t * Proxy_map.t) Builder.t) =
  (* build a goal -- alias or path
     In either case, first get the ruleset applicable to the goal.
     For an alias - there must be a rule in this ruleset.
     For a path
     - If there is a rule, use it.
     - Otherwise, that path had better exists as source.
  *)
  fun t goal ->
    jenga_root t *>>= fun (env1,__root_proxy) ->
    begin match (Env1.lookup_gen_key env1 goal) with
    | None -> return Ruleset.empty
    | Some gen_key -> generate_ruleset_if_necessary t gen_key
    end *>>= fun ruleset ->
    match Goal.case goal with
    | `alias alias_id ->
      begin
        match (Ruleset.lookup_alias ruleset alias_id) with
        | None -> error Reason.No_definition_for_alias
        | Some deps ->
          build_deps t deps *>>= fun pm ->
          return (What.Something_else, pm)
      end
    | `path demanded ->
      begin
        match (Ruleset.lookup_target ruleset demanded) with
        | Some tr ->
          build_using_target_rule t tr ~demanded *>>= fun pm ->
          return (What.Something_else, pm)
        | None ->
          look_for_source t.fs demanded *>>= function
          | None -> error Reason.No_rule_or_source
          | Some proxy ->
            let pm = Proxy_map.single (Pm_key.of_path demanded) proxy in
            return (What.Source, pm)

      end


(* now follows a sequence of functions named [build_dep] which shadow each other &
   extend with different behaviour... *)


let build_dep : (t -> Dep.t -> (What.t * Proxy_map.t) Builder.t) =
  (* Build a dependency by considering cases *)
  fun t dep ->
    match (Dep.case dep) with
    | `scan (deps,scanner)      -> build_scanner t deps scanner
    | `path path                -> build_goal t (Goal.path path)
    | `alias alias              -> build_goal t (Goal.alias alias)
    | `glob glob                -> need_glob t.fs glob
    | `null                     -> return (What.Something_else, Proxy_map.empty)


let build_dep : (t -> Dep.t -> (What.t * Proxy_map.t) Builder.t) =
  (* Wrapper to check invariant that we never traverse the same dep more than once
     i.e. that the dynamic memoization is working!
  *)
  let seen = Dep.Hash_set.create () in
  fun t dep ->
    let again = Hash_set.mem seen dep in
    if again then (
      Message.error "build_dep: unexpected repeated traversal for: %s" (Dep.to_string dep);
    );
    Hash_set.add seen dep;
    build_dep t dep


let build_dep : (t -> Dep.t -> (What.t * Proxy_map.t) Builder.t) =
  (* Report considering/re-considering *)
  fun t dep ->
    if Config.show_considering t.config
    then (
      Message.message "Considering: %s" (Dep.to_string dep);
    );
    set_status t Status.Checking;
    let builder = build_dep t dep in
    let builder = Builder.before_redo builder
      ~f:(fun () ->
        if Config.show_considering t.config || Config.show_reconsidering t.config
        then  (
          Message.message "Re-considering: %s" (Dep.to_string dep)
        );
        set_status t Status.Checking;
      )
    in
    builder


let build_dep : (t -> Dep.t -> Proxy_map.t Builder.t) =
  (* Expose the builder's result/error for reporting *)
  fun t dep ->
    let builder = build_dep t dep in
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

let build_dep : (t ->  Dep.t -> Proxy_map.t Builder.t) =
  fun t dep -> build_dep {t with me = dep} dep

let build_dep : (t -> Dep.t -> Proxy_map.t Builder.t) =
  fun t dep ->
    (* memo! *)
    share_and_check_for_cycles t
      ~key: dep
      ~memo: (memo_building t)
      ~item: (DG.Item.Dep dep)
      ~f: (fun t -> build_dep t dep)


let build_one_root_dep :
    (
      jenga_root_path: Path.LR.t ->
      throttle: unit Throttle.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      DG.t ->
      Config.t ->
      Progress.t ->
      demanded:Dep.t ->
      unit Tenacious.t
    ) =
  (* Entry point to build a single root/
     Here the downwards "t" parameter is constructed from various components.
     And here we break out of the Builder monad, and revert to the plain
     tenacious monad - ignoring the proxy map or any errors.
  *)
  fun ~jenga_root_path ~throttle fs persist memo discovered_graph
    config progress ~demanded ->
    let node = DG.create_root discovered_graph in
    let t = {
      config;
      fs;
      throttle;
      persist;
      memo;
      progress;
      jenga_root_path;
      recurse_build_dep = build_dep;
      me = demanded;
      discovered_graph;
      node;
    } in
    let builder = build_dep t demanded in
    let tenacious =
      Tenacious.bind (Builder.expose builder) (function
      | Error _ -> Tenacious.return ()
      | Ok (_ :Proxy_map.t) -> Tenacious.return ()
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
    if (!fin) then Deferred.return () else (
      let counts = Progress.snap progress in
      let fraction = Progress.Counts.fraction counts in
      Message.progress ~fraction;
      loop ()
    )
  in
  loop ()

let working_on_report_period = sec 0.3

let message_effort_and_progress progress =
  let counts = Progress.snap progress in
  (* more detailed message than just progress-fraction *)
  let num,den = Progress.Counts.fraction counts in
  Message.message "[ %s ] { %s } -- %d / %d"
    (effort_string())
    (Progress.Counts.to_string counts)
    num den

let show_effort_and_progress_reports ~fin progress =
  let rec loop () =
    Clock.after working_on_report_period >>= fun () ->
    if (!fin) then Deferred.return () else (
      message_effort_and_progress progress;
      loop ()
    )
  in
  loop ()




let make_graph_dumper discovered_graph progress =
  let status_of_dep dep = Progress.dg_status progress dep in
  `dump (fun () ->
    Message.message "dumping graph...";
    Dot.dump_graph discovered_graph ~status_of_dep >>= function
    | Error e ->
      Message.message "dumping graph - error: %s" (Error.to_string_hum e);
      Deferred.unit
    | Ok () ->
      Message.message "dumping graph...done";
      Deferred.unit
  )

let periodically_while_not_fin ~span ~fin ~f =
  let rec loop () =
    Clock.after span >>= fun () ->
    if (!fin) then Deferred.return () else (
      f () >>= fun () ->
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


let break_a_cycle_if_found dg =
  match (look_for_a_cycle dg) with
  | None -> ()
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
              (item_to_string item))))
    in
    ()


let cycle_watch_period = sec 10.0

let watch_for_cycles ~fin dg =
  let rec loop () =
    Clock.after cycle_watch_period >>= fun () ->
    if (!fin) then Deferred.return () else (
      break_a_cycle_if_found dg;
      loop ()
    )
  in
  loop ()


(*----------------------------------------------------------------------
  build_once
----------------------------------------------------------------------*)

let build_once :
    (Config.t -> DG.t -> Progress.t -> unit Tenacious.t -> Heart.t Deferred.t) =
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
    Tenacious.exec top_tenacious >>= fun ((),heart) ->

    (* to avoid reporting of stale errors etc... *)
    Progress.mask_unreachable progress dg;

    let counts = Progress.snap progress in
    let duration = Time.diff (Time.now()) start_time in

    let effort_string = effort_string() in
    zero_effort();

    if Progress.Counts.completed counts then (
      let total = Progress.Counts.total counts in
      Message.build_done ~duration ~u ~total effort_string
    ) else (
      let fraction = Progress.Counts.fraction counts in
      Message.build_failed ~duration ~u ~fraction effort_string
    );
    let quiet = Config.quiet config in
    if not quiet then (
      Progress.message_errors progress;
    );
    Deferred.return heart

(*----------------------------------------------------------------------
  entry point -- build_forever
----------------------------------------------------------------------*)

let build_forever =
  (* co-ordinate the build-forever process *)

  fun config progress ~jenga_root_path ~top_level_demands fs persist
    ~when_polling ~when_rebuilding ->

    let memo = Memo.create () in
    let discovered_graph = DG.create () in

    let throttle =
      let max_concurrent_jobs = Config.j_number config in
      Throttle.create ~continue_on_error:false ~max_concurrent_jobs

    in

    (* construct the top-level tenacious builder only once *)
    let top_tenacious =
      Tenacious.all_unit (
        List.map top_level_demands ~f:(fun demanded ->
          build_one_root_dep
            ~jenga_root_path ~throttle fs persist memo discovered_graph
            config progress ~demanded
        )
      )
    in

    let `dump dump_graph = make_graph_dumper discovered_graph progress in

    let rec build_and_poll ()  = (* never finishes if polling *)

      (* start up various asyncronous writers/dumpers *)
      let fin = ref false in

      (* async cycle detection; but NO deadlock breaking *)
      don't_wait_for (
        watch_for_cycles ~fin discovered_graph
      );

      if Config.progress config then (
        don't_wait_for (
          show_progress_fraction_reports ~fin progress
        )
      );
      if Config.show_working_on config then (
        don't_wait_for (
          show_effort_and_progress_reports ~fin progress
        )
      );

      don't_wait_for (
        if Config.continuous_graph_dump config
        then periodically_while_not_fin ~span:(sec 1.5) ~fin ~f:dump_graph
        else Deferred.unit
      );

      (* do the build once *)
      build_once config discovered_graph progress top_tenacious >>= fun heart ->

      fin := true;
      when_polling() >>= fun () ->
      begin
        if Config.continuous_graph_dump config
        then dump_graph ()
        else Deferred.return ()
      end >>= fun () ->

      match Config.poll_forever config with
      | false -> Deferred.return ()
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
        wait >>= fun () ->
        Message.rebuilding ();
        when_rebuilding() >>= fun () ->
        build_and_poll ()

    in
    build_and_poll () >>= fun () ->
    Deferred.return ()

(*----------------------------------------------------------------------
 Run_now
----------------------------------------------------------------------*)

module Run_now = struct

  exception Run_now_of_internal_action_not_supported of Action_id.t
  exception Non_zero_status_from_action_run_now of Action.t

  let lift_for_run_now ~shell action =
    match Action.case action with
    | `id id -> raise (Run_now_of_internal_action_not_supported id)
    | `xaction x ->
      let need = "run_now" in
      let delay_for_dev = None in
      let {Xaction.dir;prog;args} = x in
      shell ~delay_for_dev ~need ~dir ~prog ~args >>= function
      | Error `non_zero_status     -> raise (Non_zero_status_from_action_run_now action)
      | Error (`other_error exn)   -> raise exn
      | Ok x                       -> Deferred.return x

  let run_action_now        = lift_for_run_now ~shell:Run_external_job.shell
  let run_action_now_stdout = lift_for_run_now ~shell:Run_external_job.shell_stdout


end
