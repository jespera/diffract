(** Change summary: cluster systematic edits across a changeset into
    spatch-style rules. M1 scope — rules only (no residuals, no tiered
    [after=] attribution). See docs/change-summary-design.md. *)

(* ── Public types ────────────────────────────────────────────────── *)

type file_change =
  | Modified of {
      path : string;
      language : string;
      before_source : string;
      after_source : string;
    }
  | Added of { path : string; language : string; after_source : string }
  | Deleted of { path : string; language : string; before_source : string }

type changeset = { files : file_change list }

type rule = {
  id : string;
  pattern_text : string;
  support : int;
  language : string;
  sites : string list;  (** distinct file paths where the rule fires, sorted *)
}

type summary = { rules : rule list }

(* ── Internal pattern representation ─────────────────────────────── *)

type pat_node =
  | Hole of int
  | Leaf of { node_type : string; value : string }
  | PNode of {
      node_type : string;
      is_named : bool;
      children : pat_child list;
    }

and pat_child = { field_name : string option; child : pat_node }

type edit_pat = { before : pat_node; after : pat_node }

type instance = {
  before_text : string;
  after_text : string;
  file : string;
  line : int;
  language : string;
}

type cluster = { pattern : edit_pat; instances : instance list }

type side = Before_side | After_side

type one_sided_instance = {
  os_file : string;
  os_line : int;
  os_language : string;
  os_text : string;
  os_side : side;
}

(** Internal candidate for M1.6 fusion: carries the structural shape alongside
    its site metadata. Not emitted as rules in M1. *)
type one_sided_candidate = { os_pat : pat_node; os_instance : one_sided_instance }

let one_sided_candidate_instance (c : one_sided_candidate) : one_sided_instance =
  c.os_instance

(* ── Conversion ──────────────────────────────────────────────────── *)

let rec of_src source (n : Tree.src Tree.t) : pat_node =
  if n.children = [] && n.is_named then
    Leaf { node_type = n.node_type; value = Tree.text source n }
  else
    PNode
      {
        node_type = n.node_type;
        is_named = n.is_named;
        children =
          List.map
            (fun (c : Tree.src Tree.child) ->
              { field_name = c.field_name; child = of_src source c.node })
            n.children;
      }

(* ── Rendering ───────────────────────────────────────────────────── *)

let rec render_pat_node = function
  | Hole h -> Printf.sprintf "$H%d" h
  | Leaf { value; _ } -> value
  | PNode { children = []; node_type; _ } -> node_type
  | PNode { children; _ } ->
      let parts = List.map (fun c -> render_pat_node c.child) children in
      let parts = List.filter (fun s -> s <> "") parts in
      String.concat " " parts

let rec collect_holes acc = function
  | Hole h -> if List.mem h acc then acc else h :: acc
  | Leaf _ -> acc
  | PNode { children; _ } ->
      List.fold_left (fun a c -> collect_holes a c.child) acc children

let edit_hole_list ep =
  let hs = collect_holes [] ep.before in
  let hs = collect_holes hs ep.after in
  List.sort compare hs

(** Render an edit_pat as a .pat-style spatch block body. *)
let render_pattern_body (ep : edit_pat) : string =
  let holes = edit_hole_list ep in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "@@\n";
  Buffer.add_string buf "match: strict\n";
  List.iter
    (fun h -> Buffer.add_string buf (Printf.sprintf "metavar $H%d: single\n" h))
    holes;
  Buffer.add_string buf "@@\n";
  let before_text = render_pat_node ep.before in
  List.iter
    (fun line -> Buffer.add_string buf (Printf.sprintf "- %s\n" line))
    (String.split_on_char '\n' before_text);
  let after_text = render_pat_node ep.after in
  List.iter
    (fun line -> Buffer.add_string buf (Printf.sprintf "+ %s\n" line))
    (String.split_on_char '\n' after_text);
  Buffer.contents buf

(* ── Metrics ─────────────────────────────────────────────────────── *)

let rec count_holes = function
  | Hole _ -> 1
  | Leaf _ -> 0
  | PNode { children; _ } ->
      List.fold_left (fun a c -> a + count_holes c.child) 0 children

let rec pat_size = function
  | Hole _ -> 1
  | Leaf _ -> 1
  | PNode { children; _ } ->
      1 + List.fold_left (fun a c -> a + pat_size c.child) 0 children

let edit_holes ep = count_holes ep.before + count_holes ep.after
let edit_size ep = pat_size ep.before + pat_size ep.after

(* ── Anti-unification ────────────────────────────────────────────── *)

(** Build a recursive anti-unifier parameterised on a [hole_for] function.
    When [hole_for] is shared across multiple invocations (as in
    [anti_unify_edits] across the before and after sides), identical
    concrete-pair differences reuse the same hole index. *)
let mk_anti_unify hole_for =
  let rec go p1 p2 =
    match (p1, p2) with
    | Hole h1, Hole h2 when h1 = h2 -> Hole h1
    | Leaf l1, Leaf l2
      when l1.node_type = l2.node_type && l1.value = l2.value ->
        Leaf l1
    | PNode n1, PNode n2
      when n1.node_type = n2.node_type
           && n1.is_named = n2.is_named
           && List.length n1.children = List.length n2.children ->
        let children =
          List.map2
            (fun c1 c2 ->
              if c1.field_name = c2.field_name then
                { field_name = c1.field_name; child = go c1.child c2.child }
              else
                {
                  field_name = None;
                  child = Hole (hole_for c1.child c2.child);
                })
            n1.children n2.children
        in
        PNode { n1 with children }
    | _ -> Hole (hole_for p1 p2)
  in
  go

let make_hole_for () =
  let next = ref 0 in
  let memo : (pat_node * pat_node, int) Hashtbl.t = Hashtbl.create 32 in
  fun p1 p2 ->
    match Hashtbl.find_opt memo (p1, p2) with
    | Some h -> h
    | None ->
        let h = !next in
        incr next;
        Hashtbl.add memo (p1, p2) h;
        h

let anti_unify_edits (e1 : edit_pat) (e2 : edit_pat) : edit_pat =
  let go = mk_anti_unify (make_hole_for ()) in
  let before = go e1.before e2.before in
  let after = go e1.after e2.after in
  { before; after }

(** Anti-unify two single [pat_node]s (used for one-sided candidate
    clustering in M1.6a). Uses its own hole counter — no cross-side sharing. *)
let anti_unify_pat (p1 : pat_node) (p2 : pat_node) : pat_node =
  let go = mk_anti_unify (make_hole_for ()) in
  go p1 p2

let rec max_hole_node = function
  | Hole h -> h
  | Leaf _ -> -1
  | PNode { children; _ } ->
      List.fold_left (fun m c -> max m (max_hole_node c.child)) (-1) children

let max_hole ep = max (max_hole_node ep.before) (max_hole_node ep.after)

let rec shift_holes_node offset = function
  | Hole h -> Hole (h + offset)
  | Leaf l -> Leaf l
  | PNode n ->
      PNode
        {
          n with
          children =
            List.map
              (fun c -> { c with child = shift_holes_node offset c.child })
              n.children;
        }

let shift_holes offset ep =
  {
    before = shift_holes_node offset ep.before;
    after = shift_holes_node offset ep.after;
  }

(* ── Dendrogram ──────────────────────────────────────────────────── *)

type dnode =
  | DLeaf of instance * edit_pat
  | DMerge of {
      pattern : edit_pat;
      instances : instance list;
      left : dnode;
      right : dnode;
    }

let dnode_instances = function
  | DLeaf (inst, _) -> [ inst ]
  | DMerge m -> m.instances

let dnode_pattern = function DLeaf (_, ep) -> ep | DMerge m -> m.pattern

let hole_frac ep =
  let s = edit_size ep in
  if s = 0 then 0.0 else float_of_int (edit_holes ep) /. float_of_int s

let build_dendrogram initial =
  let nodes =
    ref
      (List.map
         (fun c -> DLeaf (List.hd c.instances, c.pattern))
         initial)
  in
  while List.length !nodes > 1 do
    let arr = Array.of_list !nodes in
    let n = Array.length arr in
    let merge_patterns pi pj =
      let offset = max_hole pi + 1 in
      anti_unify_edits pi (shift_holes offset pj)
    in
    let bi = ref 0 and bj = ref 1 in
    let bp =
      ref (merge_patterns (dnode_pattern arr.(0)) (dnode_pattern arr.(1)))
    in
    let bfrac = ref (hole_frac !bp) in
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        if not (i = 0 && j = 1) then begin
          let p =
            merge_patterns (dnode_pattern arr.(i)) (dnode_pattern arr.(j))
          in
          let frac = hole_frac p in
          if frac < !bfrac then begin
            bi := i;
            bj := j;
            bp := p;
            bfrac := frac
          end
        end
      done
    done;
    let i = !bi and j = !bj in
    let merged =
      DMerge
        {
          pattern = !bp;
          instances = dnode_instances arr.(i) @ dnode_instances arr.(j);
          left = arr.(i);
          right = arr.(j);
        }
    in
    let rest = ref [ merged ] in
    Array.iteri (fun k nd -> if k <> i && k <> j then rest := nd :: !rest) arr;
    nodes := !rest
  done;
  List.hd !nodes

let rec has_concrete = function
  | Hole _ -> false
  | Leaf _ -> true
  | PNode { is_named = true; _ } -> true
  | PNode { children; _ } -> List.exists (fun c -> has_concrete c.child) children

let cut_dendrogram ?(threshold = 0.35) min_size root =
  let is_coherent ep =
    let s = edit_size ep in
    has_concrete ep.before
    && (s = 0
       || float_of_int (edit_holes ep) /. float_of_int s < threshold)
  in
  let clusters = ref [] in
  let singletons = ref [] in
  let rec go node =
    let insts = dnode_instances node in
    let n = List.length insts in
    if n < min_size then singletons := insts @ !singletons
    else
      match node with
      | DLeaf (inst, _) -> singletons := inst :: !singletons
      | DMerge m ->
          if is_coherent m.pattern then
            clusters :=
              { pattern = m.pattern; instances = insts } :: !clusters
          else begin
            go m.left;
            go m.right
          end
  in
  go root;
  (!clusters, !singletons)

(* ── One-sided dendrogram (M1.6a) ──────────────────────────────── *)

(** A cluster of Added-only or Removed-only candidates that share a common
    pat_node shape. Internal — consumed by M1.6b fusion. *)
type one_sided_cluster = {
  os_cluster_pattern : pat_node;
  os_cluster_side : side;
  os_cluster_instances : one_sided_instance list;
}

type os_dnode =
  | OsDLeaf of one_sided_instance * pat_node
  | OsDMerge of {
      om_pattern : pat_node;
      om_instances : one_sided_instance list;
      om_left : os_dnode;
      om_right : os_dnode;
    }

let os_dnode_instances = function
  | OsDLeaf (i, _) -> [ i ]
  | OsDMerge m -> m.om_instances

let os_dnode_pattern = function
  | OsDLeaf (_, p) -> p
  | OsDMerge m -> m.om_pattern

let hole_frac_pat p =
  let s = pat_size p in
  if s = 0 then 0.0 else float_of_int (count_holes p) /. float_of_int s

let build_os_dendrogram (initial : (one_sided_instance * pat_node) list) :
    os_dnode =
  let nodes = ref (List.map (fun (i, p) -> OsDLeaf (i, p)) initial) in
  while List.length !nodes > 1 do
    let arr = Array.of_list !nodes in
    let n = Array.length arr in
    let merge_patterns p1 p2 =
      let offset = max_hole_node p1 + 1 in
      anti_unify_pat p1 (shift_holes_node offset p2)
    in
    let bi = ref 0 and bj = ref 1 in
    let bp =
      ref (merge_patterns (os_dnode_pattern arr.(0)) (os_dnode_pattern arr.(1)))
    in
    let bfrac = ref (hole_frac_pat !bp) in
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        if not (i = 0 && j = 1) then begin
          let p =
            merge_patterns (os_dnode_pattern arr.(i)) (os_dnode_pattern arr.(j))
          in
          let frac = hole_frac_pat p in
          if frac < !bfrac then begin
            bi := i;
            bj := j;
            bp := p;
            bfrac := frac
          end
        end
      done
    done;
    let i = !bi and j = !bj in
    let merged =
      OsDMerge
        {
          om_pattern = !bp;
          om_instances =
            os_dnode_instances arr.(i) @ os_dnode_instances arr.(j);
          om_left = arr.(i);
          om_right = arr.(j);
        }
    in
    let rest = ref [ merged ] in
    Array.iteri (fun k nd -> if k <> i && k <> j then rest := nd :: !rest) arr;
    nodes := !rest
  done;
  List.hd !nodes

let cut_os_dendrogram ?(threshold = 0.35) min_size side root =
  let is_coherent p =
    let s = pat_size p in
    has_concrete p
    && (s = 0 || float_of_int (count_holes p) /. float_of_int s < threshold)
  in
  let clusters = ref [] in
  let singletons = ref [] in
  let rec go node =
    let insts = os_dnode_instances node in
    let n = List.length insts in
    if n < min_size then singletons := insts @ !singletons
    else
      match node with
      | OsDLeaf (inst, _) -> singletons := inst :: !singletons
      | OsDMerge m ->
          if is_coherent m.om_pattern then
            clusters :=
              {
                os_cluster_pattern = m.om_pattern;
                os_cluster_side = side;
                os_cluster_instances = insts;
              }
              :: !clusters
          else begin
            go m.om_left;
            go m.om_right
          end
  in
  go root;
  (!clusters, !singletons)

(** Cluster one-sided candidates into [one_sided_cluster]s, separately for
    each side (Removeds with Removeds, Addeds with Addeds). Candidates that
    don't cluster (singletons) are discarded at this stage. *)
let cluster_one_sided (candidates : one_sided_candidate list) :
    one_sided_cluster list =
  let by_side s =
    List.filter_map
      (fun c ->
        if c.os_instance.os_side = s then Some (c.os_instance, c.os_pat)
        else None)
      candidates
  in
  let cluster_side side items =
    if List.length items < 2 then []
    else
      let root = build_os_dendrogram items in
      let clusters, _ = cut_os_dendrogram 2 side root in
      clusters
  in
  cluster_side Before_side (by_side Before_side)
  @ cluster_side After_side (by_side After_side)

(* ── Change-pair extraction ──────────────────────────────────────── *)

let has_structural cc =
  List.exists
    (function
      | Tree_diff.Removed _ | Tree_diff.Added _ -> true | _ -> false)
    cc

let has_deep_structural cc =
  List.exists
    (function
      | Tree_diff.Changed { change = Modified { child_changes = cc2 }; _ } ->
          has_structural cc2
      | _ -> false)
    cc

let lookahead_pairs (d : Tree_diff.diff) =
  let out = ref [] in
  let emit b a =
    out :=
      {
        Tree_diff.before_node = b;
        after_node = a;
        before_source = d.before_source;
        after_source = d.after_source;
      }
      :: !out
  in
  let rec collect ~b ~a = function
    | Tree_diff.Unchanged -> ()
    | Tree_diff.Replaced -> emit b a
    | Tree_diff.Modified { child_changes } ->
        if has_structural child_changes || has_deep_structural child_changes
        then emit b a
        else
          List.iter
            (function
              | Tree_diff.Changed { before; after; change } ->
                  collect ~b:before ~a:after change
              | _ -> ())
            child_changes
  in
  (match d.root_change with
  | Tree_diff.Modified { child_changes } ->
      List.iter
        (function
          | Tree_diff.Changed { before; after; change } ->
              collect ~b:before ~a:after change
          | _ -> ())
        child_changes
  | Tree_diff.Replaced -> emit d.before_root d.after_root
  | Tree_diff.Unchanged -> ());
  List.rev !out

(** One-sided candidate extraction (M1.5).
    Walks the diff and emits every [Added]/[Removed] child subtree it
    encounters — including ones nested inside [Changed.Modified] chains.
    These are collected so M1.6 Jaccard fusion can pair them with two-sided
    clusters (e.g. a removed import anchoring a renamed call). They do not
    become standalone rules. *)
let lookahead_one_sided (d : Tree_diff.diff) :
    (side * Tree.src Tree.t) list =
  let out = ref [] in
  let emit s n = out := (s, n) :: !out in
  let rec visit_node_change = function
    | Tree_diff.Unchanged | Tree_diff.Replaced -> ()
    | Tree_diff.Modified { child_changes } ->
        List.iter visit_child child_changes
  and visit_child = function
    | Tree_diff.Same _ -> ()
    | Tree_diff.Changed { change; _ } -> visit_node_change change
    | Tree_diff.Removed { node } -> emit Before_side node
    | Tree_diff.Added { node } -> emit After_side node
  in
  (match d.root_change with
  | Tree_diff.Modified { child_changes } ->
      List.iter visit_child child_changes
  | Tree_diff.Replaced | Tree_diff.Unchanged -> ());
  List.rev !out

(* ── Pipeline ────────────────────────────────────────────────────── *)

let collect_initial_clusters ~ctx (cs : changeset) : cluster list =
  let initial = ref [] in
  List.iter
    (fun fc ->
      match fc with
      | Modified { path; language; before_source; after_source } -> (
          try
            let bt = Tree.parse ~ctx ~language before_source in
            let at = Tree.parse ~ctx ~language after_source in
            let d = Tree_diff.diff ~before:bt ~after:at in
            List.iter
              (fun (cp : Tree_diff.change_pair) ->
                let bt = Tree.text cp.before_source cp.before_node in
                let at = Tree.text cp.after_source cp.after_node in
                let inst =
                  {
                    before_text = bt;
                    after_text = at;
                    file = path;
                    line = cp.before_node.start_point.row + 1;
                    language;
                  }
                in
                let ep =
                  {
                    before = of_src cp.before_source cp.before_node;
                    after = of_src cp.after_source cp.after_node;
                  }
                in
                initial :=
                  { pattern = ep; instances = [ inst ] } :: !initial)
              (lookahead_pairs d)
          with _ -> ())
      | Added _ | Deleted _ -> ())
    cs.files;
  !initial

(** Collect one-sided candidates (M1.5) across a changeset's [Modified] files.
    Each candidate carries its pat_node shape and site metadata. Used
    internally by M1.6 fusion; not wired into M1 rule output. *)
let collect_one_sided_candidates ~ctx (cs : changeset) :
    one_sided_candidate list =
  let out = ref [] in
  List.iter
    (fun fc ->
      match fc with
      | Modified { path; language; before_source; after_source } -> (
          try
            let bt = Tree.parse ~ctx ~language before_source in
            let at = Tree.parse ~ctx ~language after_source in
            let d = Tree_diff.diff ~before:bt ~after:at in
            List.iter
              (fun (side, node) ->
                let source =
                  match side with
                  | Before_side -> d.before_source
                  | After_side -> d.after_source
                in
                let text = Tree.text source node in
                let inst =
                  {
                    os_file = path;
                    os_line = node.start_point.row + 1;
                    os_language = language;
                    os_text = text;
                    os_side = side;
                  }
                in
                out :=
                  { os_pat = of_src source node; os_instance = inst } :: !out)
              (lookahead_one_sided d)
          with _ -> ())
      | Added _ | Deleted _ -> ())
    cs.files;
  List.rev !out

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
  | _ ->
      float_of_int (List.length inter) /. float_of_int (List.length union)

(** Attempt to fuse a Removed cluster with an Added cluster into a
    two-sided swap rule. Returns [None] if the after-side has holes (they
    would be orphan metavars on the `+` side, rejected by the spatch
    engine — cross-side alignment is M1.8's job). *)
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

(** Greedy pair-up of Removed clusters with Added clusters by descending
    Jaccard over file sets. Pairs above threshold consume both clusters. *)
let pair_one_sided_clusters ?(threshold = 0.7)
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
  let sorted =
    List.sort (fun (j1, _, _) (j2, _, _) -> compare j2 j1) !scored
  in
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

let summarize ~ctx (cs : changeset) : summary =
  let initial = collect_initial_clusters ~ctx cs in
  let two_sided_clusters =
    if List.length initial < 2 then []
    else
      let root = build_dendrogram initial in
      let clusters, _singletons = cut_dendrogram 2 root in
      List.sort
        (fun a b ->
          compare (List.length b.instances) (List.length a.instances))
        clusters
  in
  let candidates = collect_one_sided_candidates ~ctx cs in
  let os_clusters = cluster_one_sided candidates in
  let pairs = pair_one_sided_clusters os_clusters in
  let swap_rules =
    List.filter_map (fun (r, a) -> fuse_swap r a) pairs
    |> List.sort
         (fun (_, i1) (_, i2) -> compare (List.length i2) (List.length i1))
  in
  let mk_ts_rule i (c : cluster) =
    let sites =
      c.instances
      |> List.map (fun inst -> inst.file)
      |> List.sort_uniq String.compare
    in
    let language =
      match c.instances with
      | inst :: _ -> inst.language
      | [] -> "unknown"
    in
    {
      id = Printf.sprintf "R%d" (i + 1);
      pattern_text = render_pattern_body c.pattern;
      support = List.length c.instances;
      language;
      sites;
    }
  in
  let ts_rules = List.mapi mk_ts_rule two_sided_clusters in
  let mk_swap_rule i (ep, insts) =
    let sites =
      insts
      |> List.map (fun (i : one_sided_instance) -> i.os_file)
      |> List.sort_uniq String.compare
    in
    let language =
      match insts with
      | i :: _ -> i.os_language
      | [] -> "unknown"
    in
    {
      id = Printf.sprintf "R%d" (List.length ts_rules + i + 1);
      pattern_text = render_pattern_body ep;
      support = List.length sites;
      language;
      sites;
    }
  in
  let swap_rules_emitted = List.mapi mk_swap_rule swap_rules in
  { rules = ts_rules @ swap_rules_emitted }

let format_summary (s : summary) : string =
  let buf = Buffer.create 256 in
  List.iteri
    (fun i r ->
      if i > 0 then Buffer.add_char buf '\n';
      Buffer.add_string buf
        (Printf.sprintf "# rule %s  support=%d  language=%s\n" r.id r.support
           r.language);
      Buffer.add_string buf r.pattern_text;
      if r.sites <> [] then begin
        Buffer.add_string buf (Printf.sprintf "# sites %s\n" r.id);
        List.iter
          (fun p -> Buffer.add_string buf (p ^ "\n"))
          r.sites
      end)
    s.rules;
  Buffer.contents buf

(* ── Filesystem loader ──────────────────────────────────────────── *)

let default_ext_language = [ (".tsx", "tsx"); (".ts", "typescript") ]

let language_of_file path ~default ~ext_language =
  match
    List.find_opt
      (fun (ext, _) -> Filename.check_suffix path ext)
      ext_language
  with
  | Some (_, lang) -> lang
  | None -> default

let glob_match pattern filename =
  let basename = Filename.basename filename in
  if String.contains pattern '*' then
    let parts = String.split_on_char '*' pattern in
    match parts with
    | [ prefix; suffix ] ->
        String.length basename >= String.length prefix + String.length suffix
        && String.starts_with ~prefix basename
        && String.ends_with ~suffix basename
    | [ prefix ] when String.ends_with ~suffix:"*" pattern ->
        String.starts_with ~prefix basename
    | _ -> basename = pattern
  else basename = pattern

let rec walk ~exclude_dirs ~pred root acc =
  let entries = try Sys.readdir root with Sys_error _ -> [||] in
  Array.fold_left
    (fun acc entry ->
      let path = Filename.concat root entry in
      if (try Sys.is_directory path with Sys_error _ -> false) then
        if List.mem entry exclude_dirs then acc
        else walk ~exclude_dirs ~pred path acc
      else if pred path then path :: acc
      else acc)
    acc entries

let load_from_dirs ~before_dir ~after_dir ?(include_glob = None)
    ?(exclude_dirs =
      [ "node_modules"; ".git"; "_build"; "target"; "__pycache__" ])
    ?(ext_language = default_ext_language) ~default_language () : changeset =
  let pred =
    match include_glob with
    | None -> fun _ -> true
    | Some g -> fun p -> glob_match g p
  in
  let before_files = walk ~exclude_dirs ~pred before_dir [] in
  let after_files = walk ~exclude_dirs ~pred after_dir [] in
  let rel_of root path =
    let rlen = String.length root in
    let rlen = if String.length path > rlen && path.[rlen] = '/' then rlen + 1
      else rlen
    in
    String.sub path rlen (String.length path - rlen)
  in
  let before_rel =
    List.map (fun p -> (rel_of before_dir p, p)) before_files
  in
  let after_rel = List.map (fun p -> (rel_of after_dir p, p)) after_files in
  let after_map = Hashtbl.create 32 in
  List.iter (fun (r, p) -> Hashtbl.replace after_map r p) after_rel;
  let files = ref [] in
  List.iter
    (fun (rel, bpath) ->
      let lang =
        language_of_file rel ~default:default_language ~ext_language
      in
      match Hashtbl.find_opt after_map rel with
      | Some apath ->
          let bsrc =
            In_channel.with_open_bin bpath In_channel.input_all
          in
          let asrc =
            In_channel.with_open_bin apath In_channel.input_all
          in
          Hashtbl.remove after_map rel;
          if bsrc <> asrc then
            files :=
              Modified
                {
                  path = rel;
                  language = lang;
                  before_source = bsrc;
                  after_source = asrc;
                }
              :: !files
      | None ->
          let bsrc =
            In_channel.with_open_bin bpath In_channel.input_all
          in
          files :=
            Deleted { path = rel; language = lang; before_source = bsrc }
            :: !files)
    before_rel;
  Hashtbl.iter
    (fun rel apath ->
      let lang =
        language_of_file rel ~default:default_language ~ext_language
      in
      let asrc = In_channel.with_open_bin apath In_channel.input_all in
      files :=
        Added { path = rel; language = lang; after_source = asrc } :: !files)
    after_map;
  { files = List.sort compare !files }
