(** Change-summary fusion (M1.6): combine independently-clustered changes that
    co-occur across the same files into conjunctive multi-section rules
    (design §4.2). Jaccard file-set overlap groups candidates; [materialise_group]
    emits the fused rule. Depends on {!Cs_types} and {!Cs_pattern}. *)

open Cs_types
open Cs_pattern

(* ── M1.6b: Jaccard fusion of one-sided clusters ─────────────────── *)

let os_files_of_cluster c =
  c.os_cluster_instances
  |> List.map (fun (i : one_sided_instance) -> i.os_file)
  |> List.sort_uniq String.compare

let jaccard (a : string list) (b : string list) : float =
  let inter = List.filter (fun x -> List.mem x b) a in
  let union = List.sort_uniq compare (a @ b) in
  match union with
  | [] -> 0.0
  | _ -> float_of_int (List.length inter) /. float_of_int (List.length union)

(** Attempt to fuse a Removed cluster with an Added cluster into a two-sided
    swap rule. Returns [None] if the after-side has holes (they would be orphan
    metavars on the `+` side, rejected by the spatch engine — cross-side
    alignment is M1.8's job). *)
let fuse_swap (removed : one_sided_cluster) (added : one_sided_cluster) :
    (edit_pat * one_sided_instance list) option =
  if count_holes added.os_cluster_pattern > 0 then None
  else
    let offset = max_hole_node removed.os_cluster_pattern + 1 in
    let ep =
      {
        before = removed.os_cluster_pattern;
        after = shift_holes_node offset added.os_cluster_pattern;
      }
    in
    let r_files = os_files_of_cluster removed in
    let a_files = os_files_of_cluster added in
    let common =
      List.filter (fun x -> List.mem x a_files) r_files
      |> List.sort_uniq String.compare
    in
    let inst_in_common (i : one_sided_instance) = List.mem i.os_file common in
    let insts =
      List.filter inst_in_common removed.os_cluster_instances
      @ List.filter inst_in_common added.os_cluster_instances
    in
    Some (ep, insts)

(** Greedy pair-up of Removed clusters with Added clusters by descending Jaccard
    over file sets. Pairs above threshold consume both clusters. *)
let pair_one_sided_clusters ?(threshold = Cs_config.default.jaccard_threshold)
    (clusters : one_sided_cluster list) :
    (one_sided_cluster * one_sided_cluster) list =
  let removeds =
    List.filter (fun c -> c.os_cluster_side = Before_side) clusters
    |> Array.of_list
  in
  let addeds =
    List.filter (fun c -> c.os_cluster_side = After_side) clusters
    |> Array.of_list
  in
  let used_r = Array.make (Array.length removeds) false in
  let used_a = Array.make (Array.length addeds) false in
  let scored = ref [] in
  for ri = 0 to Array.length removeds - 1 do
    let r_files = os_files_of_cluster removeds.(ri) in
    for ai = 0 to Array.length addeds - 1 do
      let a_files = os_files_of_cluster addeds.(ai) in
      let j = jaccard r_files a_files in
      if j >= threshold then scored := (j, ri, ai) :: !scored
    done
  done;
  let sorted = List.sort (fun (j1, _, _) (j2, _, _) -> compare j2 j1) !scored in
  let pairs = ref [] in
  List.iter
    (fun (_, ri, ai) ->
      if (not used_r.(ri)) && not used_a.(ai) then begin
        used_r.(ri) <- true;
        used_a.(ai) <- true;
        pairs := (removeds.(ri), addeds.(ai)) :: !pairs
      end)
    sorted;
  List.rev !pairs

(* ── M1.6 cases 2 & 3: conjunctive multi-section fusion ──────────── *)

type fusion_node = {
  fn_pattern : edit_pat;
  fn_files : string list;  (** sorted unique *)
  fn_support : int;
      (** support to report when this node is emitted standalone — total fire
          count for two-sided clusters, distinct-file count for swap pairs
          (matches the prior single-rule conventions). *)
  fn_language : string;
}
(** A node feeding the fusion graph: a two-sided edit pattern plus the file set
    it fires in. Two-sided clusters and one-sided swap pairs feed in identically
    — once a swap pair has been widened to two-sided by [fuse_swap], the
    downstream fusion treats it the same as any other two-sided cluster. Cases 2
    (one-sided + two-sided) and 3 (two-sided + two-sided) of §4.2 differ only in
    input shape; the output mechanics are uniform. *)

let intersect_sorted (a : string list) (b : string list) : string list =
  List.filter (fun x -> List.mem x b) a

let intersect_all (lists : string list list) : string list =
  match lists with
  | [] -> []
  | first :: rest -> List.fold_left intersect_sorted first rest

let fusion_node_of_two_sided (c : cluster) : fusion_node =
  let files =
    c.instances
    |> List.map (fun (i : instance) -> i.file)
    |> List.sort_uniq String.compare
  in
  let language = match c.instances with i :: _ -> i.language | [] -> "" in
  {
    fn_pattern = c.pattern;
    fn_files = files;
    fn_support = List.length c.instances;
    fn_language = language;
  }

let fusion_node_of_swap (ep : edit_pat) (insts : one_sided_instance list) :
    fusion_node =
  let files =
    insts
    |> List.map (fun (i : one_sided_instance) -> i.os_file)
    |> List.sort_uniq String.compare
  in
  let language = match insts with i :: _ -> i.os_language | [] -> "" in
  (* A swap fires once per removed-side instance (each removal pairs
     with an addition), so that — not the file count — is its support,
     mirroring how two-sided support counts instances. *)
  let fires =
    List.length (List.filter (fun i -> i.os_side = Before_side) insts)
  in
  {
    fn_pattern = ep;
    fn_files = files;
    fn_support = fires;
    fn_language = language;
  }

(** Group fusion nodes into connected components by Jaccard ≥ threshold over
    their file sets. Union-find: an edge [i—j] exists iff
    [J(files_i, files_j) ≥ threshold]; components are reachable sets. *)
let group_by_jaccard ?(threshold = Cs_config.default.jaccard_threshold) (nodes : fusion_node list) :
    fusion_node list list =
  let arr = Array.of_list nodes in
  let n = Array.length arr in
  let parent = Array.init n (fun i -> i) in
  let rec find i =
    if parent.(i) = i then i
    else
      let r = find parent.(i) in
      parent.(i) <- r;
      r
  in
  let union i j =
    let ri = find i and rj = find j in
    if ri <> rj then parent.(ri) <- rj
  in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      if jaccard arr.(i).fn_files arr.(j).fn_files >= threshold then union i j
    done
  done;
  let groups : (int, fusion_node list ref) Hashtbl.t = Hashtbl.create 8 in
  for i = 0 to n - 1 do
    let r = find i in
    match Hashtbl.find_opt groups r with
    | Some lst -> lst := arr.(i) :: !lst
    | None -> Hashtbl.add groups r (ref [ arr.(i) ])
  done;
  Hashtbl.fold (fun _ lst acc -> List.rev !lst :: acc) groups []

(** Materialise a fusion group into [(sections, sites, language, support)]
    tuples ready for rule emission. Singleton groups produce one tuple each.
    Multi-node groups fuse into one conjunctive rule whose sites are the
    intersection of all members' file sets — but only if the intersection has ≥
    [min_support] members; otherwise the fusion is abandoned and the members are
    emitted standalone (transitivity in the union-find can chain pairs whose
    all-way intersection is empty; standalone emission preserves coverage).
    Sections inside a fused rule are ordered by their rendered body for
    deterministic output. *)
let materialise_group ?(min_support = Cs_config.default.min_support) (group : fusion_node list) :
    (edit_pat list * string list * string * int) list =
  match group with
  | [] -> []
  | [ n ] -> [ ([ n.fn_pattern ], n.fn_files, n.fn_language, n.fn_support) ]
  | _ ->
      let inter = intersect_all (List.map (fun n -> n.fn_files) group) in
      if List.length inter < min_support then
        List.map
          (fun n -> ([ n.fn_pattern ], n.fn_files, n.fn_language, n.fn_support))
          group
      else
        let language = match group with n :: _ -> n.fn_language | [] -> "" in
        let sorted =
          List.sort
            (fun a b ->
              compare
                (render_pattern_body a.fn_pattern)
                (render_pattern_body b.fn_pattern))
            group
        in
        [
          ( List.map (fun n -> n.fn_pattern) sorted,
            inter,
            language,
            List.length inter );
        ]

(** Pre-cluster singletons whose patterns are structurally equal into
    multi-instance clusters. Clustering singletons at ~1000-site scale runs
    O(N³) through the dendrogram; deduplicating identical patterns up front cuts
    N dramatically — a refactor that renames one symbol at 200 sites collapses
    into a single cluster before the dendrogram is even constructed. *)
let pre_group_identical (clusters : cluster list) : cluster list =
  let tbl : (edit_pat, instance list ref) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun c ->
      match Hashtbl.find_opt tbl c.pattern with
      | Some insts -> insts := c.instances @ !insts
      | None -> Hashtbl.add tbl c.pattern (ref c.instances))
    clusters;
  Hashtbl.fold
    (fun p insts acc -> { pattern = p; instances = !insts } :: acc)
    tbl []
