
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
  type t = Md5.t [@@deriving sexp, bin_io, hash, compare]
  let equal = Md5.equal
  let intern = Fn.id

  module Table = struct
    include Md5.Table
    include Provide_bin_io(Md5)
  end

  let buf = Bigbuffer.create 16384

  (* When using this function, be mindful that while digests for different values of the
     same type don't conflict, digests for different values of different types can very
     well conflict. So it may be necessary to add a version information to handle types
     changing over time. *)
  let digest_bin_prot w x =
    protectx buf ~finally:Bigbuffer.clear ~f:(fun buf ->
      Bigbuffer.add_bin_prot buf w x;
      Bigbuffer.md5 buf)
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
      include C.Set.Provide_bin_io(T)
    end

    let create ~base ~kind = { base; kind }
  end

  module Listing = struct
    type t = Elem.t list [@@deriving sexp, bin_io]

    let compare xs1 xs2 =
      (* Allow differently ordering listings to be regarded as equal *)
      Elem.Set.compare (Elem.Set.of_list xs1) (Elem.Set.of_list xs2)

    let hash_fold_t s xs =
      Elem.Set.hash_fold_t s (Elem.Set.of_list xs)

    include Bin_prot.Utils.Make_binable(struct
        module Binable = struct
          type t = Elem.Set.t [@@deriving bin_io]
        end
        type nonrec t = t
        let to_binable = Elem.Set.of_list
        let of_binable = Elem.Set.to_list
      end)
  end

  type t = {
    dir : Path.t;
    listing : Listing.t;
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

  module Digest = Digest

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
      digest : Digest.t;
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

  module Group_set_for_digest = struct
    type t = Group_set.t
    include Bin_prot.Utils.Make_binable(struct
        module Binable = struct
          type t = Digest.t list [@@deriving bin_io]
        end
        type nonrec t = t
        let to_binable t =
          Group_set.to_list t
          |> List.map ~f:(fun (group : T_with_shallow_ops.group) ->
            group.digest)
          |> List.sort ~compare:Digest.compare
        let of_binable _ = assert false
      end)
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
  [@@deriving hash, compare, sexp_of]

  type group = T_with_shallow_ops.group = {
    id : Id.t;
    hash : int;
    t : t;
    mutable cache : Cache.t;
    digest : Digest.t;
  }

  module For_digest = struct
    type nonrec t = t =
      { map    : Proxy.t Pm_key.Map.t
      ; groups : Group_set_for_digest.t
      }
    [@@deriving bin_write, bin_shape]
  end

  let digest t =
    Digest.digest_bin_prot For_digest.bin_writer_t t

  let [@inline never] debug_group { id; t; _ } =
    Caml.Printf.eprintf !"%{sexp#mach:Sexp.t}\n%!"
      [%sexp Group { id : Id.t ; t : t} ]

  module Weak_group_set = Caml.Weak.Make(struct
    type t = group
    let hash t = t.hash
    (* Not comparing the id, otherwise the lookup below would always fail. *)
    let equal t1 t2 = t1.hash = t2.hash && compare t1.t t2.t = 0
  end)
  let weak_group_set = lazy (Weak_group_set.create 1000)
  let create_group t =
    let id = Id.create () in
    let group =
      { hash = [%hash:t] t
      ; t
      ; id
      ; cache = None
      ; digest = digest t
      }
    in
    let group = Weak_group_set.merge (force weak_group_set) group in
    if Jenga_options.t.debug_group_dependencies && Id.equal id group.id then
      debug_group group;
    group

  (* Traverses subgroups in post-order, avoiding visiting the same group twice. *)
  let iter_groups' seen ~f =
    let rec go_group ({ id; hash = _; t; cache = _; digest = _ } as group) =
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
    iter_groups t ~f:(fun { id = _; hash = _; t; cache = _; digest = _ } -> f t);
    f t
  ;;

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

  let equal = [%compare.equal: t]

  (* This does not avoid visiting the same group twice, so we should only traverse short
     prefixes. *)
  let rec to_sequence { map; groups } =
    Sequence.append
      (Map.to_sequence map)
      (Sequence.concat_map (Set.to_sequence groups) ~f:group_to_sequence)
  and group_to_sequence { id = _; hash = _; t; cache = _; digest = _ } =
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

  module Digest = Digest

  let digest t =
    Digest.digest_bin_prot bin_writer_t t
end

module Versioned(M : sig type t [@@deriving bin_write] val version : int end) = struct
  type t = M.t

  let bin_write_t buf ~pos x =
    let pos = bin_write_int buf ~pos M.version in
    M.bin_write_t buf ~pos x

  let bin_size_t =
    let n = bin_size_int M.version in
    fun x -> n + M.bin_size_t x

  let bin_writer_t : t Bin_prot.Type_class.writer =
    { write = bin_write_t
    ; size  = bin_size_t
    }
end

module Rule_proxy = struct
  type t =
    { targets : Path.Rel.t list
    ; deps    : Proxy_map.t
    ; action  : Action_proxy.t
    } [@@deriving compare, fields, sexp_of]

  module Digest = Digest

  module For_digest = Versioned(struct
    type nonrec t = t =
      { targets : Path.Rel.t list
      ; deps    : Proxy_map.For_digest.t
      ; action  : Action_proxy.t
      }
    [@@deriving bin_write, bin_shape]

    let version = 0

    let%expect_test _ =
      printf "%s\n" (Bin_prot.Shape.eval_to_digest_string [%bin_shape: t]);
      (* Increase [version] when this changes *)
      [%expect {| 8151b26156c334060a92cd44171131aa |}]
  end)

  let [@inline never] debug t =
    Caml.Printf.eprintf !"%{sexp#mach:Sexp.t}\n%!"
      [%sexp Rule (t : t)]

  let digest t =
    if Jenga_options.t.debug_group_dependencies then debug t;
    Digest.digest_bin_prot For_digest.bin_writer_t t
end

module Targets_proxy = struct
  (* Each element of [t] is the digest for the element at the same index in the targets of
     the corresponding Rule_proxy. *)
  type t = Proxy.t list [@@deriving bin_write]

  module Digest = Digest

  let digest t =
    Digest.digest_bin_prot bin_writer_t t
end

module Output_proxy = struct
  type t =
    { deps   : Proxy_map.Digest.t
    ; stdout : string
    } [@@deriving hash, compare, bin_io, sexp_of, fields]
end

module Generated = struct
  type t = String.Set.t Path.Rel.Table.t [@@deriving sexp_of, bin_io]
end

type t =
  { digest_cache : (Stats.t * Digest.t) Path.Table.t
  ; generated    : Generated.t
  ; ruled        : (Rule_proxy.Digest.t * Targets_proxy.Digest.t) Path.Rel.Table.t
  ; actioned     : Output_proxy.t Action_proxy.Digest.Table.t
  } [@@deriving fields, bin_io, sexp_of]

let create () = {
  digest_cache = Path.Table.create ();
  generated = Path.Rel.Table.create();
  ruled = Path.Rel.Table.create();
  actioned = Action_proxy.Digest.Table.create();
}
