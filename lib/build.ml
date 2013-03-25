
open Core.Std
open No_polymorphic_compare let _ = _squelch_unused_module_warning_
open Async.Std

let (<>) = Int.(<>)

open Description

module Digest = Fs.Digest
module Glob = Fs.Glob
module DG = Discovered_graph


(*----------------------------------------------------------------------
  Effort
----------------------------------------------------------------------*)

let generation_calls = Effort.Counter.create "gen"
let scanner_calls = Effort.Counter.create "scan"
let external_action_counter = Effort.Counter.create "act"

let the_effort =
  Effort.create ~tag:"effort" [
    Fs.lstat_counter;
    Fs.digest_counter;
    Fs.ls_counter;
    generation_calls;
    scanner_calls;
    external_action_counter;
  ]

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

type scheme_id = string with sexp

module Env1 : sig

  type t
  val of_env : Env.t ->  [ `ok of t | `dups of scheme_id list ]
  val lookup_gen_key : t -> Goal.t -> Gen_key.t option
  val lookup_generator : t -> Gen_key.t -> Rule_generator.t
  val run_scanner : t -> Scan_id.t -> Dep.t list Deferred.t
  val run_internal_action : t -> Action_id.t -> unit Deferred.t

end = struct

  type t = {
    lookup_gen_key : Goal.t -> Gen_key.t option;
    lookup_generator : Gen_key.t -> Rule_generator.t;
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
      (*Message.verbose "lookup_gen_key: %s" key_string;*)
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
    let lookup_generator gen_key =
      (*Message.verbose "lookup_generator: %s" (Gen_key.to_string gen_key);*)
      let {Gen_key. tag; dir} = gen_key in
      match (Hashtbl.find h_schemes tag) with
      | None -> failwith "lookup_generator" (* cant happen? *)
      | Some scheme_body ->
        let generator = (!scheme_body) ~dir in
        generator
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
        lookup_generator;
        run_scanner;
        run_internal_action;
      }

end


(*----------------------------------------------------------------------
  Rule_set
----------------------------------------------------------------------*)

module Rule_set : sig

  type t with sexp
  val empty : t
  val create : Rule.t list -> [ `ok of t | `dups of Path.t list ]
  val lookup_target : t -> Path.t -> Target_rule.t option
  val lookup_alias : t -> Alias.t -> Dep.t list option

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

  type x = Rule.t list with sexp

  let sexp_of_t t = sexp_of_x t.rules

  let t_of_sexp sexp =
    match create (x_of_sexp sexp) with
    | `dups _ -> failwith "Rule_set.create reports dups" (* cant happen! *)
    | `ok x -> x

end

(*----------------------------------------------------------------------
 Pm_key - path or glob
----------------------------------------------------------------------*)

module Pm_key : sig

  type t with sexp
  include Comparable with type t := t

  val of_path : Path.t -> t
  val of_glob : Glob.t -> t
  val to_path_exn : t -> Path.t (* for targets_proxy_map *)
  val compare : t -> t -> int

  val to_string : t -> string

end = struct

  module T = struct
    type t = Path of Path.t | Glob of Glob.t with sexp, compare
  end
  include T
  include Comparable.Make(T)

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

  type t with sexp
  val is_directory : t
  val of_digest : Digest.t -> t
  val of_listing : Fs.Listing.t -> t
  val equal : t -> t -> bool

end = struct

  type t = Is_directory | Digest of Digest.t | Fs_proxy of Fs.Listing.t with sexp
  let is_directory = Is_directory
  let of_digest x = Digest x
  let of_listing x = Fs_proxy x
  let equal t1 t2 =
    match t1,t2 with
    | Is_directory, Is_directory -> true
    | Digest x1, Digest x2 -> Fs.Digest.equal x1 x2
    | Fs_proxy x1, Fs_proxy x2 -> Fs.Listing.equal x1 x2
    | _,_ -> false

end


module PPs = struct (* list of path-tagged proxies *)
  type t = (Path.t * Proxy.t) list
end


module Proxy_map : sig (* need to be keyed on Path/Glob *)

  type t with sexp
  val single : Pm_key.t -> Proxy.t -> t
  val empty : t

  type inconsistency = (Pm_key.t * Proxy.t list) list
  with sexp_of

  val create_by_path : PPs.t -> [`ok of t | `err of inconsistency]
  val merge : t list -> [ `ok of t | `err of inconsistency]

  val diff : t -> t -> Pm_key.t list option

end = struct

  type t = Proxy.t Pm_key.Map.t with sexp

  type inconsistency = (Pm_key.t * Proxy.t list) list with sexp_of

  let empty = Pm_key.Map.empty
  let single key proxy = Pm_key.Map.of_alist_exn [(key,proxy)]

  let create xs =
    match (Pm_key.Map.of_alist xs) with
    | `Duplicate_key key ->
      let proxys = List.filter_map xs ~f:(fun (key',proxy) ->
        if Int.(=) 0 (Pm_key.compare key key') then Some proxy else None
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

  let equal t1 t2 = Map.equal Proxy.equal t1 t2

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

  type t with sexp
  val create : Digest.t option -> t
  val equal : t -> t -> bool

end = struct

  type t = {
    root : Digest.t option;
  } with sexp

  let create root = { root }
  let equal t1 t2 = Option.equal Fs.Digest.equal t1.root t2.root

end

module Rooted_proxy : sig

  type t with sexp
  val create : Root_proxy.t -> Proxy_map.t -> t
  val diff : t -> t -> [`root_changed | `proxy_map_changed of Pm_key.t list] option

end = struct

  type t = {
    rp : Root_proxy.t;
    pm : Proxy_map.t;
  } with sexp

  let create rp pm = { rp; pm; }

  let diff t1 t2 =
    if not (Root_proxy.equal t1.rp t2.rp) then Some `root_changed
    else
      match (Proxy_map.diff t1.pm t2.pm) with
      | None -> None
      | Some changed_keys -> Some (`proxy_map_changed changed_keys)

end

module Action_proxy : sig

  type t with sexp
  val extern : Xaction.t -> t
  val intern : Action_id.t -> Root_proxy.t -> t
  val to_action : t -> Action.t
  val diff : t -> t -> [`root_changed | `action_changed ] option

end = struct

  type t = X of Xaction.t | I of Action_id.t * Root_proxy.t with sexp
  let extern x = X x
  let intern a rp = I (a,rp)

  let diff t1 t2 =
    match t1,t2 with
    | I (i1,rp1), I (i2,rp2) ->
      if not (Root_proxy.equal rp1 rp2) then
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
  } with sexp
end

(*----------------------------------------------------------------------
  (error) reason
----------------------------------------------------------------------*)

module Reason = struct
  type t =
  | Error_in_deps                     of (Dep.t * t) list
  | Digest_error
  | Glob_error                        of string
  | Jenga_root_problem                of string

  | No_definition_for_alias
  | No_rule_or_source
  | Non_zero_status

  | Inconsistent_proxies              of Proxy_map.inconsistency
  | Duplicate_scheme_ids              of scheme_id list

  | Generator_raised                  of exn
  | Internal_action_raised            of exn
  | Running_external_action_raised    of exn
  | Scanner_raised                    of exn

  | Duplicate_rules_for_paths         of Path.t list
  | Rule_failed_to_generate_targets   of Path.t list

  | Cycle_report                      of Dep.t * DG.Item.t list


  let item_to_string = function
    | DG.Item.Root -> "ROOT"
    | DG.Item.Dep dep -> (*sprintf "DEP: %s"*) (Dep.to_string dep)
    | DG.Item.Target_rule tr -> sprintf "RULE: %s" (Target_rule.to_string tr)
    | DG.Item.Gen_key g -> sprintf "GEN: %s" (Gen_key.to_string g)

  let rec to_string = function

    | Error_in_deps xs ->
      sprintf "Unable to build dependencies: [ %s ]"
        (String.concat ~sep:" " (List.map xs ~f:(fun (dep,t) ->
          sprintf "%s: %s"
            (Dep.to_string dep)
            (to_string t)
         )))

    | Digest_error -> "unable to digest file"
    | Glob_error s -> sprintf "glob error: %s" s
    | Jenga_root_problem s -> sprintf "Problem with JengaRoot: %s" s

    | No_definition_for_alias -> "No definition found for alias"
    | No_rule_or_source -> "No rule or source found for target"
    | Non_zero_status -> "External action completed with non-zero exist exit code"

    | Duplicate_scheme_ids xs ->
      sprintf "Duplicate schemes with ids: %s"
        (String.concat ~sep:" " (List.map xs ~f:(sprintf "%S")))

    | Inconsistent_proxies inconsistency ->
      sprintf "Inconsistency proxies on keys: %s"
        (String.concat ~sep:" " (List.map inconsistency ~f:(fun (key,_) ->
          Pm_key.to_string key)))

    (* multi line errors *)
    | Generator_raised exn ->
      sprintf "Generator raised exception:\n%s" (Exn.to_string exn)

    | Internal_action_raised exn ->
      sprintf "Internal action raised exception:\n%s" (Exn.to_string exn)

    | Running_external_action_raised exn ->
      sprintf "Running external action raised exception:\n%s" (Exn.to_string exn)

    | Scanner_raised exn ->
      sprintf "Scanner raised exception:\n%s" (Exn.to_string exn)

    | Duplicate_rules_for_paths paths ->
      sprintf "Duplicate rules for paths:\n%s"
        (String.concat ~sep:"\n- " (List.map paths ~f:Path.to_rrr_string))

    | Rule_failed_to_generate_targets paths ->
      sprintf "Rule failed to generate targets:\n%s"
        (String.concat ~sep:"\n- " (List.map paths ~f:Path.to_rrr_string))

    | Cycle_report (_,items) ->
      sprintf "Cyclic dependencies: %s"
        (String.concat (
          List.map items ~f:(fun item ->
            sprintf "\n- %s" (item_to_string item))))


end

(*----------------------------------------------------------------------
  Progress - progress monitor
----------------------------------------------------------------------*)

(* TODO: special case Error for Error_in_dep *)
(* distinuish Source as special case of Built *)
(* Make this status be the same type as Dot.Status *)
module Status = struct
  type t =
  | Considering | Waiting | Running of string
  | Built (* or source *) | Error of Reason.t
end

module Progress : sig

  type t
  val create : unit -> t
  val ht : t -> Status.t Dep.Table.t
  val message_errors : t -> unit

  val dg_status : t  -> Dep.t -> Dot.Status.t

  module Counts : sig
    type t
    val to_string : t -> string
    val fraction : t -> (int*int)
    val completed : t -> bool
    val total : t -> int
  end

  val snap : t -> Counts.t

end = struct

  type t = Status.t Dep.Table.t

  let create () = Dep.Table.create()
  let ht t = t

  let dg_status t dep =
    match (Hashtbl.find t dep) with
    | None -> Dot.Status.Unknown
    | Some status ->
      let cat x =  Dot.Status.Cat x in
      match status with
      | Status.Considering                    -> cat Dot.Catagory.Waiting
      | Status.Waiting                        -> cat Dot.Catagory.Waiting
      | Status.Running _                      -> cat Dot.Catagory.Working
      | Status.Built                          -> cat Dot.Catagory.Good
      | Status.Error (Reason.Error_in_deps _) -> cat Dot.Catagory.Child_error
      | Status.Error _                        -> cat Dot.Catagory.Error

  let final_status_to_string = function
    | Status.Considering    -> "unexpectedly Considering"
    | Status.Waiting        -> "unexpectedly Waiting"
    | Status.Running s      -> sprintf "unexpectedly Running: %s" s
    | Status.Built          -> "built"
    | Status.Error reason   -> Reason.to_string reason

  let suppress_reports_for dep = function
    (* suppress reporting errors for
       - stuff which was built ok
       - child errors (they'll be reported at the child)
       - cyclic error at all places except the `nub' (to avoid multiple reports)
    *)
    | Status.Built -> true
    | Status.Error (Reason.Error_in_deps _) -> true
    | Status.Error (Reason.Cycle_report (nub,_)) -> not (Int.(=) 0 (Dep.compare dep nub))
    | _ -> false

  let message_errors t =
    Hashtbl.iter t ~f:(fun ~key:dep ~data:status ->
      if suppress_reports_for dep status then () else
        Message.error "%s: %s" (Dep.to_string dep) (final_status_to_string status)
    )

  module Counts = struct

    type t = {
      c : int;
      w : int;
      r : int;
      b : int;
      e : int;
    }

    let total {c;w;r;b;e} = c + w + r + b + e

    let fraction t = t.b , (total t)

    let completed t = Int.equal t.b (total t)

    let to_labelled {c;w;r;b;e} =
      [
        ("considering"  , c);
        ("waiting"      , w);
        ("running"      , r);
        ("built"        , b);
        ("error"        , e);
      ]

    let to_string t =
      String.concat ~sep:", "
        (List.map (to_labelled t) ~f:(fun (s,n) -> sprintf "%s=%d" s n))

  end

  let snap t =
    let (c,w,r,b,e) =
      Hashtbl.fold t
        ~init:(0,0,0,0,0)
        ~f:(fun ~key:_ ~data:status (c,w,r,b,e) ->
          match status with
          | Status.Considering    -> (c+1,w,r,b,e)
          | Status.Waiting        -> (c,w+1,r,b,e)
          | Status.Running _      -> (c,w,r+1,b,e)
          | Status.Built          -> (c,w,r,b+1,e)
          | Status.Error _        -> (c,w,r,b,e+1)
        )
    in
    {Counts. c;w;r;b;e}

end

(*----------------------------------------------------------------------
  Builder
----------------------------------------------------------------------*)

module Builder : sig (* layer error monad within tenacious monad *)

  type 'a t
  val expose : 'a t -> ('a, Reason.t) Result.t Tenacious.t
  val lift : ('a, Reason.t) Result.t Tenacious.t -> 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val error : Reason.t -> 'a t

  val for_all_collect_errors :
    'a list -> f:('a -> 'b t) -> ('b list * ('a * Reason.t) list) t

  val tenacious : 'a Tenacious.t -> 'a t
  val deferred : 'a Deferred.t -> 'a t
  val try_deferred : (unit -> 'a Deferred.t) -> err:(exn -> Reason.t) -> 'a t

  val desensitize : 'a t -> ('a * Heart.t) t
  val sensitize : Heart.t -> unit t

  val when_do_or_redo : (unit -> 'a t) -> f:(unit -> unit) -> 'a t

end = struct

  type 'a t = ('a, Reason.t) Result.t Tenacious.t
  let expose t = t
  let lift t = t

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

  let tenacious ten =
    Tenacious.bind ten (fun x ->
      Tenacious.return (Ok x)
    )

  let deferred def =
    Tenacious.lift (fun () ->
      def >>= fun x ->
      Deferred.return (Ok x,Heart.unbreakable)
    )

  let try_deferred f ~err =
    Tenacious.lift (fun () ->
      Monitor.try_with f >>= function
      | Ok x -> Deferred.return (Ok x,Heart.unbreakable)
      | Error exn -> Deferred.return (Error (err exn), Heart.unbreakable)
    )

  let desensitize t = (* if not error *)
    Tenacious.lift (fun () ->
      Deferred.bind (Tenacious.exec t) (fun (ore,heart) ->
        match ore with
        | Error e -> Deferred.return (Error e, heart)
        | Ok x -> Deferred.return (Ok (x,heart), Heart.unbreakable)
      )
    )

  let sensitize heart =
    Tenacious.lift (fun () -> Deferred.return (Ok (), heart))

  let when_do_or_redo = Tenacious.when_do_or_redo

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
  let to_string t = Sexp.to_string (sexp_of_t t)
end

(*----------------------------------------------------------------------
  running things - generator, scanners, actions
----------------------------------------------------------------------*)

let run_generator : (RR.t -> Gen_key.t -> Rule_generator.t -> Rule_set.t Builder.t) =
  fun rr gen_key generator ->
    Message.reason "Running generator (%s) %s" (RR.to_string rr)
      (Gen_key.to_string gen_key);
    Builder.try_deferred ~err:(fun exn -> Reason.Generator_raised exn) (fun () ->
      Effort.track generation_calls (fun () ->
        Rule_generator.gen generator
      )
    ) *>>= fun rules ->
    match (Rule_set.create rules) with
    | `ok ruleset -> return ruleset
    | `dups paths -> error (Reason.Duplicate_rules_for_paths paths)


let run_scan_id : (RR.t -> Env1.t -> Scan_id.t -> Dep.t list Builder.t) =
  fun rr env1 scan_id ->
    Message.reason "Running scanner (%s) %s" (RR.to_string rr) (Scan_id.to_string scan_id);
    Builder.try_deferred ~err:(fun exn -> Reason.Scanner_raised exn) (fun () ->
      Effort.track scanner_calls (fun () ->
        Env1.run_scanner env1 scan_id
      )
    )

let run_action :
    (RR.t -> Env1.t -> Job_scheduler.t -> Action.t -> need:string -> unit Builder.t)=
  fun rr env1 js action ~need ->
    Message.reason "Running action (%s) %s" (RR.to_string rr) (Action.to_string action);
    match Action.case action with
    | `id action_id ->
      Builder.try_deferred ~err:(fun exn -> Reason.Internal_action_raised exn) (fun ()->
        Env1.run_internal_action env1 action_id
      )
    | `xaction x ->
      Builder.deferred (
        Effort.track external_action_counter (fun () ->
          let {Xaction.dir;prog;args} = x in
          Job_scheduler.shell js ~need ~dir ~prog ~args
        )
      ) *>>= function
      | Ok ()                      -> return ()
      | Error (`non_zero_status _) -> error Reason.Non_zero_status
      | Error (`other_error exn)   -> error (Reason.Running_external_action_raised exn)

(*----------------------------------------------------------------------
  File system interface
----------------------------------------------------------------------*)

let digest_path
    : (Fs.t -> Path.t -> [ `file of Digest.t | `directory | `missing ] Builder.t) =
  fun fs path ->
    Builder.tenacious (Fs.digest_file fs ~file:path) *>>= function
    | `digest_error _ -> error Reason.Digest_error
    | `stat_error _   -> return `missing
    | `not_a_file     -> return `directory
    | `digest digest  -> return (`file digest)

let look_for_source : (Fs.t -> Path.t -> Proxy.t option Builder.t) =
  fun fs path ->
    digest_path fs path *>>= function
    | `file digest    -> return (Some (Proxy.of_digest digest))
    | `directory      -> return (Some Proxy.is_directory)
    | `missing        -> return None

let need_glob : (Fs.t -> Glob.t -> Proxy_map.t Builder.t) =
  fun fs glob ->
    Builder.tenacious (Fs.list_glob fs glob) *>>= function
    | `stat_error _   -> error (Reason.Glob_error "no such directory")
    | `not_a_dir      -> error (Reason.Glob_error "not a directory")
    | `listing_error _-> error (Reason.Glob_error "unable to list")
    | `listing listing ->
      return (Proxy_map.single (Pm_key.of_glob glob) (Proxy.of_listing listing))

(*----------------------------------------------------------------------
  Persist - static cache - saved to file between runs
----------------------------------------------------------------------*)

module Persist = struct

  (* The values in these hashtable must be sexp convertable,
     so they can be saved to file.  *)

  type t = {
    scanned     : (Rooted_proxy.t * Dep.t list) Scan_id.Table.t;
    generated   : (Rooted_proxy.t * Rule_set.t) Gen_key.Table.t;
    actioned    : Rule_proxy.t Path.Table.t;
  } with sexp

  let create () = {
    scanned = Scan_id.Table.create();
    generated = Gen_key.Table.create();
    actioned = Path.Table.create();
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
    generating  : (Rule_set.t   Builder.t * DG.Node.t) Gen_key.Table.t;
    building    : (Proxy_map.t  Builder.t * DG.Node.t) Dep.Table.t;
    ruling      : (PPs.t        Builder.t * DG.Node.t) Target_rule.Table.t;
    root        : (Env1.t * Root_proxy.t) Builder.t option ref;
  }

  let create () = {
    generating = Gen_key.Table.create();
    building = Dep.Table.create();
    ruling = Target_rule.Table.create();
    root = ref None;
  }

end

(*----------------------------------------------------------------------
  t - The downwards bucket type
----------------------------------------------------------------------*)

type t = {

  fs : Fs.t; (* file system *)
  js : Job_scheduler.t; (* run external jobs through throttle *)
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
  Hashtbl.set (Progress.ht t.progress) ~key:(t.me) ~data:status

let push_dependency t item =
  {t with node = DG.create_dependency t.discovered_graph t.node item}


let scanned t = t.persist.Persist.scanned
let generated t = t.persist.Persist.generated
let actioned t = t.persist.Persist.actioned

let memo_generating t = t.memo.Memo.generating
let memo_building t = t.memo.Memo.building
let memo_ruling t = t.memo.Memo.ruling
let memo_root t = t.memo.Memo.root

let build_dep : (t ->  Dep.t -> Proxy_map.t Builder.t) =
  fun t dep -> t.recurse_build_dep {t with me = dep} dep


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
        match (DG.link_dependants t.discovered_graph node ~additional:t.node) with
        | `ok -> builder
        | `cycle items -> error (Reason.Cycle_report (t.me, items))
      end
    | None ->
      let t = push_dependency t item in
      let builder =
        Builder.when_do_or_redo (fun () ->
          f t
        ) ~f:(fun () -> DG.remove_all_dependencies t.node)
      in
      (* we use add_exn, because each key will be memoized exactly once *)
      Hashtbl.add_exn memo ~key ~data:(builder,t.node);
      builder


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
        | `directory -> error (Reason.Jenga_root_problem "is a directory")
        | `missing -> error (Reason.Jenga_root_problem "missing")
        | `file digest -> return (Some digest)
    )
    *>>= fun digest_opt ->
    Builder.deferred (Load_root.get_env t.jenga_root_path) *>>= function
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

let collect_child_errors_lift_cycles errors =
  (* Treat cycle errors specially.
     a cycle error in an error for every point in the cycle, but we would like to report
     it just once - at the point in the loop the cycle was detected.
  *)
  let cycles =
    List.filter errors ~f:(function
    | (_,Reason.Cycle_report _) -> true
    | _ -> false
    )
  in
  match cycles with
  | (_,cycle1)::_ -> cycle1
  | _ -> Reason.Error_in_deps errors


let build_deps : (t -> Dep.t list -> Proxy_map.t Builder.t) =
  (* Build a collection of [deps] in parallel.
     Error if any of [deps] errors
     - but for a new reason: [Error_in_deps], listing those deps in error.
  *)
  fun t deps ->
    set_status t Status.Waiting;
    Builder.for_all_collect_errors deps ~f:(build_dep t) *>>= fun (pms,errors) ->
    match errors with
    | _::_ -> error (collect_child_errors_lift_cycles errors)
    | [] ->
      match (Proxy_map.merge pms) with
      | `err inconsistency -> error (Reason.Inconsistent_proxies inconsistency)
      | `ok pm -> return pm


let generate_ruleset_if_necessary : (t -> Gen_key.t -> Rule_set.t Builder.t) =
  (* Run rule generation (for a goal) iff:
     - it has never been run before
     - or the proxy indicated it is out of date.
     Record a successful run in the persistent state.
  *)
  fun t gen_key ->
    jenga_root t *>>= fun (env1,root_proxy) ->
    let generator = Env1.lookup_generator env1 gen_key in
    let generator_deps = Rule_generator.deps generator in
    build_deps t generator_deps *>>= fun proxy_map ->
    let rooted_proxy = Rooted_proxy.create root_proxy proxy_map in
    let run_and_cache rr =
      set_status t (Status.Running "generator");
      run_generator rr gen_key generator *>>= fun ruleset ->
      Hashtbl.set (generated t) ~key:gen_key ~data:(rooted_proxy,ruleset);
      return ruleset
    in
    match (Hashtbl.find (generated t) gen_key) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some (prev,ruleset) ->
      match Rooted_proxy.diff prev rooted_proxy with
      | Some `root_changed -> run_and_cache RR.Jenga_root_changed
      | Some (`proxy_map_changed keys) -> run_and_cache (RR.Deps_have_changed keys)
      | None ->
        return ruleset (* Up to date; dont run anything *)


let generate_ruleset_if_necessary : (t -> Gen_key.t -> Rule_set.t Builder.t) =
  (* memo *)
  fun t gen_key ->
    share_and_check_for_cycles t
      ~key: gen_key
      ~memo: (memo_generating t)
      ~item: (DG.Item.Gen_key gen_key)
      ~f: (fun t -> generate_ruleset_if_necessary t gen_key)


let run_scanner_if_necessary : (t -> Dep.t list -> Scan_id.t -> Dep.t list Builder.t) =
  (* Run a scanner iff
     - it has never been run before
     - or the scanner_proxy indicated it is out of date.
     Record a successful run in the persistent state.
  *)
  fun t scanner_deps scan_id ->
    jenga_root t *>>= fun (env1,root_proxy) ->
    build_deps t scanner_deps *>>= fun proxy_map ->
    let rooted_proxy = Rooted_proxy.create root_proxy proxy_map in
    let run_and_cache rr =
      set_status t (Status.Running "scanner");
      run_scan_id rr env1 scan_id *>>= fun scanned_deps ->
      Hashtbl.set (scanned t) ~key:scan_id ~data:(rooted_proxy,scanned_deps);
      return scanned_deps
    in
    match (Hashtbl.find (scanned t) scan_id) with
    | None -> run_and_cache RR.No_record_of_being_run_before
    | Some (prev,scanned_deps) ->
      match Rooted_proxy.diff prev rooted_proxy with
      | Some `root_changed -> run_and_cache RR.Jenga_root_changed
      | Some (`proxy_map_changed keys) -> run_and_cache (RR.Deps_have_changed keys)
      | None ->
        return scanned_deps (* Up to date; dont run anything *)


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
      set_status t (Status.Running "action");
      run_action rr env1 t.js action ~need *>>= fun () ->
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
                Message.checked "NOT RUNNING: %s" (Action.to_string action);
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


let build_goal : (t -> Goal.t -> Proxy_map.t Builder.t) =
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
    | None -> return Rule_set.empty
    | Some gen_key -> generate_ruleset_if_necessary t gen_key
    end *>>= fun ruleset ->
    match Goal.case goal with
    | `alias alias_id ->
      begin
        match (Rule_set.lookup_alias ruleset alias_id) with
        | None -> error Reason.No_definition_for_alias
        | Some deps -> build_deps t deps
      end
    | `path demanded ->
      begin
        match (Rule_set.lookup_target ruleset demanded) with
        | Some tr -> build_using_target_rule t tr ~demanded
        | None ->
          look_for_source t.fs demanded *>>= function
          | None -> error Reason.No_rule_or_source
          | Some proxy ->
            return (Proxy_map.single (Pm_key.of_path demanded) proxy)
      end


let build_scanner : (t -> Dep.t list -> Scan_id.t -> Proxy_map.t Builder.t) =
  (* run the scanner; build the scanned dependencies
  *)
  fun t scan_deps scan_id ->
    run_scanner_if_necessary t scan_deps scan_id *>>= fun scanned_deps ->
    build_deps t scanned_deps


(* now follows a sequence of functions named [build_dep] which shadow each other &
   extend with different behaviour... *)


let build_dep : (t -> Dep.t -> Proxy_map.t Builder.t) =
  (* Build a dependency by considering cases *)
  fun t dep ->
    match (Dep.case dep) with
    | `scan (scan_deps,scan_id) -> build_scanner t scan_deps scan_id
    | `path path                -> build_goal t (Goal.path path)
    | `alias alias              -> build_goal t (Goal.alias alias)
    | `glob glob                -> need_glob t.fs glob
    | `null                     -> return Proxy_map.empty


let build_dep : (t -> Dep.t -> Proxy_map.t Builder.t) =
  (* Expose the builder's result/error for reporting *)
  fun t dep ->
    Builder.lift (
      Tenacious.bind (
        Tenacious.when_do_or_redo ~f:(fun () ->
          Message.considering "Considering: %s" (Dep.to_string dep);
          set_status t Status.Considering
        )
          (fun () -> Builder.expose (build_dep t dep))
      ) (fun ore ->
        begin
          match ore with
          | Ok _ -> set_status t Status.Built
          | Error reason -> set_status t (Status.Error reason)
        end;
        Tenacious.return ore
      )
    )

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
      Job_scheduler.t ->
      Fs.t ->
      Persist.t ->
      Memo.t ->
      DG.t ->
      Progress.t ->
      demanded:Dep.t ->
      unit Tenacious.t
    ) =
  (* Entry point to build a single root/
     Here the downwards "t" parameter is constructed from various components.
     And here we break out of the Builder monad, and revert to the plain
     tenacious monad - ignoring the proxy map or any errors.
  *)
  fun ~jenga_root_path js fs persist memo discovered_graph progress ~demanded ->
    let node = DG.create_root discovered_graph in
    let t = {
      fs;
      js;
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

let working_on_report_period = sec 1.0

let message_effort_and_progress progress =
  let counts = Progress.snap progress in
  (* more detailed message than just progress-fraction *)
  let num,den = Progress.Counts.fraction counts in
  Message.message "%s { %s } -- %d / %d"
    (Effort.Snapped.to_string (Effort.snap the_effort))
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
  build_once
----------------------------------------------------------------------*)

let build_once :
    (Config.t -> Progress.t -> unit Tenacious.t -> Heart.t Deferred.t) =
  (* Make a single build using the top level tenacious builder
     Where a single build means we've done all we can
     (maybe we are complete, or maybe some targets are in error)
     given the current state of the file-system.
     And now we are just polling of file-system changes.
  *)
  let genU = (let r = ref 1 in fun () -> let u = !r in r:=1+u; u) in
  fun config progress top_tenacious ->
    let u = genU() in (* count of the times we reach polling *)
    let start_time = Time.now() in
    Tenacious.exec top_tenacious >>= fun ((),heart) ->
    let counts = Progress.snap progress in
    let duration = Time.diff (Time.now()) start_time in
    if Progress.Counts.completed counts then (
      let total = Progress.Counts.total counts in
      Message.build_done ~duration ~u ~total
    ) else (
      let fraction = Progress.Counts.fraction counts in
      Message.build_failed ~duration ~u ~fraction
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

  fun config ~jenga_root_path ~top_level_demands fs persist ~when_polling ->

    let memo = Memo.create () in
    let discovered_graph = DG.create () in
    let progress = Progress.create () in

    let js =
      let delay_for_dev = Config.delay_for_dev config in
      let max_concurrent_jobs = Config.j_number config in
      Job_scheduler.create ~delay_for_dev ~max_concurrent_jobs
    in

    (* construct the top-level tenacious builder only once *)
    let top_tenacious =
      Tenacious.all_unit (
        List.map top_level_demands ~f:(fun demanded ->
          build_one_root_dep
            ~jenga_root_path js fs persist memo discovered_graph progress
            ~demanded
        )
      )
    in

    let `dump dump_graph = make_graph_dumper discovered_graph progress in

    let rec build_and_poll ()  = (* never finishes if polling *)

      (* start up various asyncronous writers/dumpers *)
      let fin = ref false in
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
      build_once config progress top_tenacious >>= fun heart ->

      fin := true;
      when_polling() >>= fun () ->
      begin
        if Config.continuous_graph_dump config
        then dump_graph ()
        else Deferred.return ()
      end >>= fun () ->
      if Config.show_working_on config then (
        message_effort_and_progress progress
      );

      match Config.poll_forever config with
      | false -> Deferred.return ()
      | true ->
        (* -P *)
        Message.polling ();
        if Config.show_sensitized config then (
          List.iter (Heart.to_sensitivity_list heart) ~f:(fun desc ->
            Message.sensitized_on desc
          )
        );
        (* wait here until something changes on the file-system *)
        let wait = Heart.when_broken heart in
        wait >>= fun () ->
        List.iter (Heart.to_broken_list heart) ~f:(fun desc ->
          Message.file_changed desc
        );
        Message.rebuilding ();
        Effort.reset_to_zero the_effort;
        build_and_poll ()

    in
    build_and_poll () >>= fun () ->
    Deferred.return ()
