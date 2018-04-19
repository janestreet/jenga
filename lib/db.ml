
open Core
open! Int.Replace_polymorphic_compare
module Unix = Async.Unix

module Kind = struct
  type t =
  [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  [@@deriving sexp, bin_io, hash, compare]
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Mtime = struct
  type t = float [@@deriving sexp, bin_io, hash, compare]
  let of_float f = f
  let equal = [%compare.equal: t]
end

module Stats = struct

  (* should contain enough info from the Unix.Stats.t to reliably indicate that
     the path must be re-digested because it might have changed.

     we should like this structure to be small, as we will store lots of them

     we're happy to accept that sometimes a change after the stat, we will have to
     redigest a file only to get the same digest

     what we dont want is for the chance that the stat info remains the same, BUT
     if we were to re-digest we would get different contents.

     This is possible in theory - i.e. to keep the same inode & mtime
     but have different content.
     But this is pretty unusual (how exactly can it happen?)

     And we are happy to not regard these as changes which will trigger a rebuild.

     Some of the info in this stat structure is not for the it-might-have-changed
     detection -- i.e. the kind -- but we need this to know how to treat the path
     just statted.
  *)

  type t = {
    (* can we do without dev & size ?? *)
    dev : int;
    ino : int;
    kind : Kind.t;
    size : int64;
    mtime : Mtime.t;
  } [@@deriving fields, sexp, bin_io, hash, compare]

  let equal = [%compare.equal: t]

  let of_unix_stats (u : Core.Unix.stats) =
    {
      dev = u.st_dev;
      ino = u.st_ino;
      kind = Async.Unix.File_kind.of_unix u.st_kind;
      size = u.st_size;
      mtime = u.st_mtime;
    }

end

module Digest = struct
  include Interning.String(struct let who = "<digest>" end)
  let t_of_sexp sexp = intern (Md5.to_binary (Md5.t_of_sexp sexp))
  let _ = t_of_sexp (* we silence the warning to keep shadowing the implementation
                       from the functor *)
  let sexp_of_t t = Md5.sexp_of_t (Md5.of_binary_exn (extern t))
  let intern md5 = intern (Md5.to_binary md5)
end

module Listing = struct

  module Elem = struct
    module T = struct
      type t = {
        base : string;
        kind : Kind.t;
      } [@@deriving sexp, bin_io, hash, compare]
    end
    include T

    module C = Comparable.Make(T)
    include (C : module type of struct include C end with module Set := C.Set)
    module Set = struct
      include C.Set
      include C.Set.Provide_hash(T)
    end

    let create ~base ~kind = { base; kind }
  end

  type listing = Elem.t list [@@deriving sexp, bin_io]

  let compare_listing xs1 xs2 =
    (* Allow differently ordering listings to be regarded as equal *)
    Elem.Set.compare (Elem.Set.of_list xs1) (Elem.Set.of_list xs2)

  let hash_fold_listing s xs =
    Elem.Set.hash_fold_t s (Elem.Set.of_list xs)

  type t = {
    dir : Path.t;
    listing : listing;
  } [@@deriving sexp, bin_io, hash, compare]

  let create ~dir ~elems = { dir; listing = elems }

  let equal = [%compare.equal: t]

  let of_file_paths_exn ~dir paths =
    let elems = List.map paths ~f:(fun path ->
      assert (Path.(dirname path = dir));
      {Elem. base = Path.basename path; kind = `File;}
    ) in
    {dir; listing = elems}

  let paths t =
    Path.Set.of_list (
      List.map t.listing ~f:(fun e ->
        Path.relative ~dir:t.dir e.Elem.base
      ))

  module Restriction = struct
    type t = {
      kinds : Kind.t list option; (* None means any kind *)
      pat : Pattern.t;
    } [@@deriving sexp, bin_io, hash, compare]

    let create ~kinds pat = { kinds; pat; }

    let pattern t = t.pat

    let kind_allows_file t =
      match t.kinds with
      | None -> true
      | Some kinds -> List.mem kinds `File ~equal:[%compare.equal: Kind.t]

    let to_string t =
      sprintf "%s %s"
        (match t.kinds with
        | None -> "(any-kind)"
        | Some kinds ->
          sprintf "(%s)" (String.concat ~sep:"," (List.map kinds ~f:Kind.to_string)))
        (Pattern.to_string t.pat)

  end

  let restrict t r =
    let match_kind =
      match r.Restriction.kinds with
      | None -> (fun _ -> true)
      | Some kinds -> List.mem kinds ~equal:[%compare.equal: Kind.t]
    in
    {
      dir = t.dir;
      listing =
        List.filter t.listing ~f:(fun e ->
          match_kind e.Elem.kind
          && Pattern.matches r.Restriction.pat e.Elem.base
        )
    }

end

module Glob = struct

  module T = struct
    type t = {
      dir : Path.t;
      restriction : Listing.Restriction.t;
    } [@@deriving sexp, bin_io, hash, compare, fields]
  end

  include T
  include Hashable.Make(T)

  let create ~dir ~restriction = { dir; restriction; }

  let to_string t =
    sprintf "glob: %s/ %s"
      (Path.to_string (dir t))
      (Listing.Restriction.to_string (restriction t))

end

module Pm_key = struct

  module T = struct
    type t = Path of Path.t | Glob of Glob.t
    [@@deriving sexp_of, bin_io, hash, compare]
  end
  include T

  module C = Comparable.Make_plain(T)
  include (C : module type of struct include C end with module Map := C.Map)
  module Map = struct
    include C.Map
    include C.Map.Provide_bin_io(T)
    include C.Map.Provide_hash(T)
  end

  let equal = [%compare.equal: t]

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

  let to_path_opt = function
    | Path path -> Some path
    | Glob _ -> None

end

module Proxy = struct

  type t =
    | Digest of Digest.t
    | Fs_proxy of Listing.t
  [@@deriving sexp_of, bin_io, hash, compare]

  let of_digest x = Digest x

  let of_listing ~dir paths =
    Fs_proxy (Listing.of_file_paths_exn ~dir (Path.Set.to_list paths))

  let equal = [%compare.equal: t]

end

module Proxy_map = struct

  module Id = Unique_id.Int63()

  module Cache = struct
    type t = Univ.t option
  end

  module rec T_with_shallow_ops : sig
    type t = {
      map : Proxy.t Pm_key.Map.t;
      groups : (group, Group_comparator.comparator_witness) Set.t;
    }
    and group = {
      id : Id.t;
      hash : int;
      t : t;
      mutable cache : Cache.t;
    }
    val compare_group : group -> group -> int
    val hash_fold_group : Hash.state -> group -> Hash.state
    val hash_group : group -> int
    val sexp_of_group : group -> Sexp.t
  end = struct
    include T_with_shallow_ops
    let compare_group group1 group2 = Id.compare group1.id group2.id
    let sexp_of_group group = Id.sexp_of_t group.id
    let hash_group group = group.hash
    let hash_fold_group state group = hash_fold_int state (hash_group group)
  end
  and Group_comparator : Comparator.S with type t := T_with_shallow_ops.group =
    Comparator.Make(struct
      type t = T_with_shallow_ops.group [@@deriving compare, sexp_of]
    end)

  module Group_set = struct
    module T = struct
      type t = T_with_shallow_ops.group [@@deriving hash]
      include Group_comparator
      let sexp_of_t = T_with_shallow_ops.sexp_of_group
    end
    include Set.Make_plain_using_comparator(T)
    include Provide_hash(T)
  end

  (* We allow some values to compare as not equal even though we would prefer to think of
     them as equal when they only differ in the structure of groups.
     However in practice, it sounds like an inefficiency in the rules if the same big
     dependencies are created with or without structure depending on the code path.
  *)
  type t = T_with_shallow_ops.t = {
    map : Proxy.t Pm_key.Map.t;
    groups : Group_set.t;
  }
  [@@deriving hash, compare]

  type group = T_with_shallow_ops.group = {
    id : Id.t;
    hash : int;
    t : t;
    mutable cache : Cache.t;
  }

  module Weak_group_set = Caml.Weak.Make(struct
    type t = group
    let hash t = t.hash
    (* Not comparing the id, otherwise the lookup below would always fail. *)
    let equal t1 t2 = t1.hash = t2.hash && compare t1.t t2.t = 0
  end)
  let weak_group_set = lazy (Weak_group_set.create 1000)
  let create_group t =
    let group = { hash = [%hash:t] t; t; id = Id.create (); cache = None } in
    Weak_group_set.merge (force weak_group_set) group

  module type Serialization_param = sig
    (* identifiers on disk can be unrelated to the ones in memory once loaded *)
    module Disk_id : sig include Binable.S include Sexpable.S with type t := t end
    (* only one of [Saving] and [Loading] will be defined at a time *)
    module Saving : sig
      val to_disk : Id.t -> Disk_id.t
    end
    module Loading : sig
      val group_of_id : Disk_id.t -> group
      val define_group : Disk_id.t -> group -> unit
    end
  end

  module T_with_serialization(X : Serialization_param) = struct
    module Group_with_shallow_ops = struct
      type t = group
      let to_id t = X.Saving.to_disk t.id
      let of_id id = X.Loading.group_of_id id
      module Binable = Binable.Of_binable(X.Disk_id)(struct
        type nonrec t = t
        let to_binable = to_id
        let of_binable = of_id
      end)
      module Sexpable = Sexpable.Of_sexpable(X.Disk_id)(struct
          type nonrec t = t
          let to_sexpable = to_id
          let of_sexpable = of_id
        end)
      include Binable
      include Sexpable
      module Set =
        Set.Make_binable_using_comparator(struct
          type t = T_with_shallow_ops.group
          include Group_comparator
          include Binable
          include Sexpable
        end)
    end
    type nonrec t = t = {
      map : Proxy.t Pm_key.Map.t;
      groups : Group_with_shallow_ops.Set.t;
    } [@@deriving bin_io, sexp_of]

    module Group_in_index = struct
      module Format = struct
        type nonrec t = X.Disk_id.t * t [@@deriving bin_io, sexp_of]
      end
      type t = group
      let to_format t = X.Saving.to_disk t.id, t.t
      let of_format (id, t) =
        let group = create_group t in
        X.Loading.define_group id group;
        group
      include Binable.Of_binable(Format)(struct
          type t = group
          let to_binable = to_format
          let of_binable = of_format
        end)
      let sexp_of_t t = Format.sexp_of_t (to_format t)
    end
  end

  (* Traverses subgroups in post-order, avoiding visiting the same group twice. *)
  let iter_groups' seen ~f =
    let rec go_group ({ id; hash = _; t; cache = _ } as group) =
      if not (Hash_set.mem seen id)
      then begin
        Hash_set.add seen id;
        go t;
        f group;
      end
    and go { map = _; groups } =
      Set.iter groups ~f:go_group
    in
    go
  ;;

  let iter_groups t ~f =
    let seen = Id.Hash_set.create ~size:(Set.length t.groups) () in
    iter_groups' seen t ~f;
  ;;

  let iter_bindings t ~f =
    let f { map; groups = _ } = Map.iteri map ~f in
    iter_groups t ~f:(fun { id = _; hash = _; t; cache = _ } -> f t);
    f t
  ;;

  (* We go recursively before enqueuing so the order in the index from group id to group
     respects the dependencies between groups. This way, when we read an id, we have seen
     its definition already, so we don't need an intermediate datastructure. *)
  let build_index (seen, index) = iter_groups' seen ~f:(Queue.enqueue index)

  let shallow_length { map; groups } =
    Map.length map + Set.length groups

  let no_groups = Group_set.empty
  let create map groups = { map; groups }

  let empty = create Pm_key.Map.empty no_groups

  let single key proxy = create (Pm_key.Map.of_alist_exn [(key,proxy)]) no_groups

  let group t =
    create Pm_key.Map.empty (Group_set.singleton (create_group t))

  type inconsistency = (Pm_key.t * Proxy.t list) list [@@deriving sexp_of]

  let map_of_alist =
    (* If [of_alist_reduce] gave us the key, we could use it in here. *)
    let rec loop acc_t = function
      | [] -> Ok acc_t
      | (key,proxy)::xs ->
        match (Map.find acc_t key) with
        | Some proxy' ->
          if Proxy.equal proxy proxy'
          then loop acc_t xs
          else
            (* just find/report one inconsistency *)
            Error [ (key,[proxy;proxy']) ]
        | None ->
          loop (Map.set acc_t ~key:key ~data:proxy) xs
    in
    fun xs -> loop Pm_key.Map.empty xs
  let of_alist xs =
    match map_of_alist xs with
    | Ok map -> Ok (create map no_groups)
    | Error _ as e -> e

  let create_by_path xs =
    of_alist (List.map xs ~f:(fun (path,v) -> (Pm_key.of_path path,v)))

  let equal = [%compare.equal: t]

  (* This does not avoid visiting the same group twice, so we should only traverse short
     prefixes. *)
  let rec to_sequence { map; groups } =
    Sequence.append
      (Map.to_sequence map)
      (Sequence.concat_map (Set.to_sequence groups) ~f:group_to_sequence)
  and group_to_sequence { id = _; hash = _; t; cache = _ } =
    to_sequence t
  ;;

  let difference { map = map1; groups = groups1 } { map = map2; groups = groups2 } =
    let diff_map =
      Map.symmetric_diff map1 map2 ~data_equal:Proxy.equal
      |> Sequence.map ~f:(fun (key, _) -> key)
    in
    let diff_groups =
      Set.symmetric_diff groups1 groups2
      |> Sequence.concat_map
           ~f:(fun (First group | Second group) ->
             Sequence.map (group_to_sequence group) ~f:(fun (key, _) -> key))
    in
    Sequence.append diff_map diff_groups
  ;;

  let diff ~before ~after =
    match compare before after with
    | 0 -> None
    | _ -> Some (Sequence.to_list (Sequence.take (difference before after) 5))
  ;;

  let filesystem_assumptions (t : t) =
    let dirs = Path.Rel.Hash_set.create () in
    let files = Path.Rel.Hash_set.create () in
    let arbitrary_files = Path.Rel.Hash_set.create () in
    let add_if_relative set path =
      match Path.case path with
      | `absolute _ -> ()
      | `relative path -> Hash_set.add set path
    in
    iter_bindings t ~f:(fun ~key ~data ->
      match key with
      | Path file ->
        add_if_relative dirs (Path.dirname file);
        add_if_relative files file;
      | Glob _ ->
        match data with
        | Digest _ -> assert false
        | Fs_proxy { dir; listing } ->
          add_if_relative dirs dir;
          List.iter listing ~f:(fun { base; kind } ->
            let path = Path.relative ~dir base in
            let set =
              match kind with
              | `File | `Char | `Block | `Link | `Fifo | `Socket ->
                (* Not sure about `Link, but it only matters if the link points to a
                   directory, which should be rare in practice. *)
                arbitrary_files
              | `Directory -> dirs
            in
            add_if_relative set path));
    `Dirs dirs, `Files files, `Arbitrary_files arbitrary_files

  let shallow_paths t =
    let map = t.map in
    let r = ref [] in
    Map.iteri map ~f:(fun ~key ~data:_ ->
      match key with
      | Path path -> r := path :: !r
      | Glob _ -> ());
    !r

  module Group = struct
    type t = group
    let find_or_add t ~unique_id_across_jenga:type_id ~unique_f_across_jenga:f =
      match t.cache with
      | Some univ -> Univ.match_exn univ type_id
      | None ->
        let r = f (shallow_paths t.t) in
        t.cache <- Some (Univ.create type_id r);
        r
  end

  let to_paths_for_mtimes_check t =
    let r = ref [] in
    iter_groups t ~f:(fun group -> r := group :: !r);
    shallow_paths t, !r

  let merge ts =
    let xs = List.concat_map ts ~f:(fun t -> Map.to_alist t.map) in
    match map_of_alist xs with
    | Ok map ->
      let group = Group_set.union_list (List.map ts ~f:(fun t -> t.groups)) in
      Ok (create map group)
    | Error _ as err -> err

end

module Sandbox_kind = struct

 type t =
   | No_sandbox
   | Hardlink
   | Copy
   | Hardlink_ignore_targets
   | Copy_ignore_targets
 [@@deriving hash, sexp, bin_io, compare]

end

module Process_proxy = struct

  type t = {
    dir : Path.t;
    prog : string;
    args : string list;
    ignore_stderr : bool;
    sandbox : Sandbox_kind.t;
  } [@@deriving hash, sexp, bin_io, compare, fields]

  let create ~dir ~prog ~args ~ignore_stderr ~sandbox =
    { dir ; prog; args; ignore_stderr; sandbox }

end

module Save_proxy = struct

  type t = {
    contents : string;
    target : Path.t;
    chmod_x : bool;
  } [@@deriving hash, sexp, bin_io, compare, fields]

  let create ~contents ~target ~chmod_x =
    { contents; target; chmod_x }

end

module Action_proxy = struct

  module T = struct
    type t =
      | Process of Process_proxy.t
      | Save of Save_proxy.t
    [@@deriving hash, sexp, bin_io, compare]
  end
  include T
  include Hashable.Make_binable(T)

end

module Rule_proxy = struct

  type t = {
    targets : Proxy_map.t;
    deps : Proxy_map.t;
    action : Action_proxy.t
  } [@@deriving hash, compare, fields]

  let build_index acc { targets; deps; action = _ } =
    Proxy_map.build_index acc targets;
    Proxy_map.build_index acc deps
  ;;

  module T_with_serialization(X : Proxy_map.Serialization_param) = struct
    module Proxy_map = Proxy_map.T_with_serialization(X)
    type nonrec t = t = {
      targets : Proxy_map.t;
      deps : Proxy_map.t;
      action : Action_proxy.t
    } [@@deriving bin_io, sexp_of]
  end

end

module Output_proxy = struct

  type t = {
    deps : Proxy_map.t;
    stdout : string;
  } [@@deriving hash, compare, fields]

  let build_index acc { deps; stdout = _ } = Proxy_map.build_index acc deps

  module T_with_serialization(X : Proxy_map.Serialization_param) = struct
    module Proxy_map = Proxy_map.T_with_serialization(X)
    type nonrec t = t = {
      deps : Proxy_map.t;
      stdout : string;
    } [@@deriving bin_io, sexp_of]
  end

end

module T = struct
  type t = {
    digest_cache : (Stats.t * Digest.t) Path.Table.t;
    generated : Path.Set.t Path.Rel.Table.t;
    ruled : Rule_proxy.t Path.Rel.Table.t;
    actioned : Output_proxy.t Action_proxy.Table.t;
  } [@@deriving fields]
end
include T

let build_index acc { digest_cache = _; generated = _; ruled; actioned } =
  Hashtbl.iter ruled ~f:(Rule_proxy.build_index acc);
  Hashtbl.iter actioned ~f:(Output_proxy.build_index acc);
;;

module T_with_serialization(X : Proxy_map.Serialization_param) = struct
  module Rule_proxy = Rule_proxy.T_with_serialization(X)
  module Output_proxy = Output_proxy.T_with_serialization(X)
  type nonrec t = t = {
    digest_cache : (Stats.t * Digest.t) Path.Table.t;
    generated : Path.Set.t Path.Rel.Table.t;
    ruled : Rule_proxy.t Path.Rel.Table.t;
    actioned : Output_proxy.t Action_proxy.Table.t;
  } [@@deriving sexp_of, bin_io]
end

let create () = {
  digest_cache = Path.Table.create ();
  generated = Path.Rel.Table.create();
  ruled = Path.Rel.Table.create();
  actioned = Action_proxy.Table.create();
}

module With_index = struct

  module Index = struct
    type t = Proxy_map.group Queue.t
    let iter t ~f = Queue.iter t ~f:(fun elt -> f elt.Proxy_map.t)
  end

  (* It's important for [index] to go first because its bin_io reading implementation is
     side-effectful and the side-effects are necessary to load [t]. *)
  type t = { index : Index.t; t : T.t }
  [@@deriving bin_shape ~basetype:"89920580-490a-11e6-a091-1398e5058293"]

  let index t = t.index

  module T_with_serialization(X : Proxy_map.Serialization_param) = struct
    module Proxy_map = Proxy_map.T_with_serialization(X)
    module T = T_with_serialization(X)
    type nonrec t = t = { index : Proxy_map.Group_in_index.t Queue.t; t : T.t }
    [@@deriving bin_io, sexp_of]
  end

  let snapshot t =
    let seen = Proxy_map.Id.Hash_set.create () in
    let index = Queue.create () in
    build_index (seen, index) t;
    { index; t }
  ;;

  let value t = t.t

  module Disk_id = Int

  let bin_write_t, bin_size_t, sexp_of_t =
    let module T =
      T_with_serialization(struct
        module Disk_id = Disk_id
        module Loading = struct
          let group_of_id _ = assert false
          let define_group _ _ = assert false
        end
        module Saving = struct
          let to_disk id = Proxy_map.Id.to_int_exn id
        end
      end) in
    T.bin_write_t, T.bin_size_t, T.sexp_of_t

  let bin_read_t buf ~pos_ref =
    let table = Disk_id.Table.create () in
    let module T =
      T_with_serialization(struct
        module Disk_id = Disk_id
        module Loading = struct
          let group_of_id id = Hashtbl.find_exn table id
          let define_group id group = Hashtbl.add_exn table ~key:id ~data:group
        end
        module Saving = struct
          let to_disk _ = assert false
        end
      end) in
    T.bin_read_t buf ~pos_ref

  let __bin_read_t__ _ ~pos_ref:_ = assert false

  (* Define bin_writer, bin_reader etc. *)
  include (struct type nonrec t = t [@@deriving bin_io] end
           : sig type t [@@deriving bin_io] end with type t := t)
end
