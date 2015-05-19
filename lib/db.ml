
open Core.Std
module Unix = Async.Std.Unix

let equal_using_compare compare = fun x1 x2 -> Int.(=) 0 (compare x1 x2)

module Job = struct

  module T = struct
    type t = {
      dir : Path.t;
      prog : string;
      args : string list;
    } with sexp, bin_io, compare, fields
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make_binable(T)

  let create ~dir ~prog ~args = { dir ; prog; args; }

end

module Kind = struct
  type t =
  [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
  with sexp, bin_io, compare
  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Mtime = struct
  type t = float with sexp, bin_io, compare
  let of_float f = f
  let equal = equal_using_compare compare
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
  } with fields, sexp, bin_io, compare

  let equal = equal_using_compare compare

  let of_unix_stats u =
    let module U = Unix.Stats in
    {
      dev = U.dev u;
      ino = U.ino u;
      kind = U.kind u;
      size = U.size u;
      mtime = Mtime.of_float (Time.to_float (U.mtime u));
    }

end

module Digest = struct
  module IS = Interning.String(struct let who = "<digest>" end)
  include IS
end

module Listing = struct

  module Elem = struct
    module T = struct
      type t = {
        base : string;
        kind : Kind.t;
      } with sexp, bin_io, compare
    end
    include T
    include Comparable.Make_binable(T)
    let create ~base ~kind = { base; kind }
  end

  type t = {
    dir : Path.t;
    listing : Elem.t list;
  } with sexp, bin_io, compare

  let create ~dir ~elems = { dir; listing = elems }

  let compare t1 t2 =
    let res = Path.compare t1.dir t2.dir in
    if Int.(res<>0) then res
    else
      (* Allow differently ordering listings to be regarded as equal *)
      Elem.Set.compare (Elem.Set.of_list t1.listing) (Elem.Set.of_list t2.listing)

  let equal = equal_using_compare compare

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
    } with sexp, bin_io, compare

    let create ~kinds pat = { kinds; pat; }

    let pattern t = t.pat

    let kind_allows_file t =
      match t.kinds with
      | None -> true
      | Some kinds -> List.mem kinds `File

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
      | Some kinds -> List.mem kinds
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
    } with sexp, bin_io, compare, fields
    let hash = Hashtbl.hash
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

  let to_path_opt = function
    | Path path -> Some path
    | Glob _ -> None

end

module Proxy = struct

  type t = Digest of Digest.t | Fs_proxy of Listing.t with sexp_of, bin_io, compare
  let of_digest x = Digest x

  let of_listing ~dir paths =
    Fs_proxy (Listing.of_file_paths_exn ~dir (Path.Set.to_list paths))

  let equal = equal_using_compare compare

end

module Proxy_map = struct
  type t = Proxy.t Pm_key.Map.t with sexp_of, bin_io, compare
  let empty = Pm_key.Map.empty
  let single key proxy = Pm_key.Map.of_alist_exn [(key,proxy)]
  let filesystem_assumptions (t : t) =
    let dirs = Path.Hash_set.create () in
    let files = Path.Hash_set.create () in
    let arbitrary_files = Path.Hash_set.create () in
    let add_if_relative set path =
      if Path.is_absolute path then ()
      else Hash_set.add set path
    in
    Map.iter t ~f:(fun ~key ~data ->
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
end

module Rule_proxy = struct

  type t = {
    targets : Proxy_map.t;
    deps : Proxy_map.t;
    action : Job.t
  } with sexp_of, bin_io, compare, fields

end

module Output_proxy = struct

  type t = {
    deps : Proxy_map.t;
    stdout : string;
  } with sexp_of, bin_io, compare, fields

end

type t = {
  digest_cache : (Stats.t * Digest.t) Path.Table.t;
  generated : Path.Set.t Gen_key.Table.t;
  ruled : Rule_proxy.t Path.Rel.Table.t;
  actioned : Output_proxy.t Job.Table.t;
} with sexp_of, bin_io, fields

let create () = {
  digest_cache = Path.Table.create ();
  generated = Gen_key.Table.create();
  ruled = Path.Rel.Table.create();
  actioned = Job.Table.create();
}
