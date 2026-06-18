(** Change-summary clustering (PROPOSE-side, design §4.1): build the
    anti-unification dendrogram over change pairs, cut it at the
    coarsest-still-coherent level, coarsen orphan holes, re-specialize survivors,
    and cluster one-sided (Added/Removed) candidates for fusion. Depends on
    {!Cs_types}, {!Cs_pattern}, and {!Cs_evaluate} (the removal-group safety
    check). *)

open Cs_types
open Cs_pattern
open Cs_evaluate

(* respecialize is defined after the orphan-coarsening block below: it
   re-anti-unifies from fully-concrete ipats, so it must re-apply
   coarsening or a coarsened cluster's orphan would resurface. *)

(* ── Dendrogram ──────────────────────────────────────────────────── *)

type dnode =
  | DLeaf of instance list * edit_pat
  | DMerge of {
      pattern : edit_pat;
      instances : instance list;
      left : dnode;
      right : dnode;
    }

let dnode_instances = function
  | DLeaf (insts, _) -> insts
  | DMerge m -> m.instances

let dnode_pattern = function DLeaf (_, ep) -> ep | DMerge m -> m.pattern

(* [hole_frac] and the coherence-gate predicates live in {!Cs_pattern}. *)

(** Score a candidate merge for the dendrogram. Pure hole-fraction can pick a
    merge whose anti-unification holes both sides at unrelated positions — e.g.
    merging [tokenCache.read → tokenCache.get] with
    [tokenCache.write → tokenCache.set] holes the property on each side
    independently, producing a [+]-side hole with no [-]-side binding source
    (orphan). Such patterns are rejected by the coherence gate, dropping the
    merge to singletons. Penalise these merges so the greedy step prefers a
    sibling pairing that keeps holes aligned (e.g. merging different receivers
    with the same property: [tokenCache.read] + [rateCache.read] → [$H0.read]).
*)
let merge_score ep =
  let base = hole_frac ep in
  let before_holes = collect_holes [] ep.before in
  let after_holes = collect_holes [] ep.after in
  let aligned = List.for_all (fun h -> List.mem h before_holes) after_holes in
  if aligned then base else base +. 10.0

(** Cheap signature for a pattern's root pair, used to short-circuit
    [build_dendrogram]'s inner loop. Anti-unifying two patterns whose [before]
    (or [after]) roots differ in kind or node_type produces a hole-rooted
    pattern that the downstream coherence and safety gates will reject.
    Comparing this signature is a string equality vs. a full tree
    anti-unification. *)
let root_sig (ep : edit_pat) =
  let tag = function
    | Hole _ -> ("H", "")
    | Leaf { node_type; _ } -> ("L", node_type)
    | PNode { node_type; _ } -> ("P", node_type)
  in
  (tag ep.before, tag ep.after)

let build_dendrogram initial =
  let trace = Cs_trace.on () in
  let initial_n = List.length initial in
  let merge_patterns pi pj =
    let offset = max_hole pi + 1 in
    anti_unify_edits pi (shift_holes offset pj)
  in
  (* Each node carries a stable id so a merge's anti-unification score is
     memoised on the unordered node pair. A merge removes its two nodes (whose
     cache entries are never queried again) and adds O(current) new pairs, so
     each distinct pair is anti-unified at most once over the whole build —
     O(m²) anti-unifications rather than the O(m³) of recomputing every
     surviving pair on every iteration. [merge_score] is symmetric in its
     arguments (identical hole count, size, and before/after hole alignment
     regardless of order — only hole *numbering*, which the score ignores,
     differs), so the unordered key is sound and each cached score is
     bit-identical to a fresh computation. Selection (the (0,1) default, the
     strict-< scan-order tie-break) and the chosen merge's pattern are
     therefore byte-identical to the unmemoised build. *)
  let next_id = ref 0 in
  let fresh () =
    let i = !next_id in
    incr next_id;
    i
  in
  let nodes =
    ref (List.map (fun c -> (fresh (), DLeaf (c.instances, c.pattern))) initial)
  in
  let score_cache : (int * int, float) Hashtbl.t = Hashtbl.create 256 in
  let total_antiunifies = ref 0 in
  let score (ida, nda) (idb, ndb) =
    let key = if ida <= idb then (ida, idb) else (idb, ida) in
    match Hashtbl.find_opt score_cache key with
    | Some f -> f
    | None ->
        incr total_antiunifies;
        let f =
          merge_score (merge_patterns (dnode_pattern nda) (dnode_pattern ndb))
        in
        Hashtbl.add score_cache key f;
        f
  in
  let t_start = Unix.gettimeofday () in
  let last_tick = ref t_start in
  let iter_no = ref 0 in
  let heartbeat () =
    if trace then begin
      let now = Unix.gettimeofday () in
      if now -. !last_tick >= 2.0 then begin
        Printf.eprintf
          "  dendrogram: iter %d/%d, %d anti-unifies so far, elapsed %.1fs\n%!"
          !iter_no (initial_n - 1) !total_antiunifies (now -. t_start);
        last_tick := now
      end
    end
  in
  if trace then
    Printf.eprintf
      "  dendrogram: starting hierarchical merge over %d clusters\n%!" initial_n;
  while List.length !nodes > 1 do
    let arr = Array.of_list !nodes in
    let n = Array.length arr in
    let sigs = Array.map (fun (_, nd) -> root_sig (dnode_pattern nd)) arr in
    incr iter_no;
    let bi = ref 0 and bj = ref 1 in
    let bfrac = ref (score arr.(0) arr.(1)) in
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        if (not (i = 0 && j = 1)) && sigs.(i) = sigs.(j) then begin
          let frac = score arr.(i) arr.(j) in
          if frac < !bfrac then begin
            bi := i;
            bj := j;
            bfrac := frac
          end
        end
      done;
      heartbeat ()
    done;
    let i = !bi and j = !bj in
    let _, ndi = arr.(i) and _, ndj = arr.(j) in
    let merged =
      ( fresh (),
        DMerge
          {
            pattern = merge_patterns (dnode_pattern ndi) (dnode_pattern ndj);
            instances = dnode_instances ndi @ dnode_instances ndj;
            left = ndi;
            right = ndj;
          } )
    in
    let rest = ref [ merged ] in
    Array.iteri (fun k nd -> if k <> i && k <> j then rest := nd :: !rest) arr;
    nodes := !rest
  done;
  if trace then
    Printf.eprintf
      "  dendrogram: done in %.2fs after %d iterations (%d anti-unifies)\n%!"
      (Unix.gettimeofday () -. t_start)
      !iter_no !total_antiunifies;
  match !nodes with (_, nd) :: _ -> nd | [] -> invalid_arg "build_dendrogram"

(* ── Proposal-side orphan coarsening (tree embedding, ordered) ─────────
   When anti-unification leaves a [+]-side hole with no [-]-side binding
   (an orphan), the after reuses content that *varies across instances*
   and has no before counterpart — e.g. a dependency array freshly
   introduced by a reshape (`useCallback($L, [d_i])`). The coherence gate
   would reject the whole candidate. Instead we coarsen the orphan to the
   instances' common embedded skeleton (ordered tree inclusion, §4.3
   discussion): the geodesic gate (§M1.9b) then claims each site
   *decomposably* and the per-site remainder becomes a `rule=`-attributed
   residual. This first cut handles the under-approximating direction
   (`C ⊑ A_i`) for *container* orphans of varying arity — drop to the
   empty container (`[]`/`()`/`{}`), which embeds in every instance. (The
   over-approximating / sub-part direction and richer common-subsequence
   cores are noted in the design as follow-ons; unordered embedding too.)
   Soundness is the gate's job — coarsening only proposes. *)

(* An unnamed (punctuation/keyword) AST node — a delimiter like [[], []],
   [(], [)]. of_src lowers these to a childless [PNode] with [is_named =
   false] (named leaves become [Leaf], which is always content). *)
let is_delim = function PNode { is_named = false; _ } -> true | _ -> false

(* Surface of [p] emptied of its content: a container whose first and last
   children are delimiters renders, with the middle dropped, as just those
   delimiters — [[d]] -> "[]", [(a, b)] -> "()", [{x: 1}] -> "{}". Tree
   delimiters are child nodes (not template text), so the empty form keeps
   the outermost two children and drops everything between (commas
   included). [None] for a leaf or a node not bracketed by delimiters
   (where emptying is not meaningful, e.g. [x + 1]). *)
let empty_container_surface (p : pat_node) : string option =
  match p with
  | PNode { children; _ } -> (
      match children with
      | first :: _ :: _ ->
          let last = List.nth children (List.length children - 1) in
          if is_delim first.child && is_delim last.child then
            Some (render_pat_node first.child ^ render_pat_node last.child)
          else None
      | _ -> None)
  | _ -> None

(* Substitute [repl] for every [Hole h] in a pat_node. *)
let rec subst_hole h repl = function
  | Hole h' when h' = h -> repl
  | (Hole _ | Leaf _) as p -> p
  | PNode n ->
      PNode
        {
          n with
          children =
            List.map
              (fun c -> { c with child = subst_hole h repl c.child })
              n.children;
        }

(* The concrete subtree an instance binds at [Hole h] (the pattern and the
   instance share shape everywhere the pattern is not a hole). *)
let rec subtree_at_hole (pat : pat_node) (con : pat_node) (h : int) :
    pat_node option =
  match pat with
  | Hole h' -> if h' = h then Some con else None
  | Leaf _ -> None
  | PNode pn -> (
      match con with
      | PNode cn when List.length pn.children = List.length cn.children ->
          List.fold_left2
            (fun acc pc cc ->
              match acc with
              | Some _ -> acc
              | None -> subtree_at_hole pc.child cc.child h)
            None pn.children cn.children
      | _ -> None)

(* Replace each orphan [+]-side hole with the instances' empty-container
   skeleton, when their bindings there are containers of one type sharing
   a delimiter surface. Leaves non-container orphans untouched (they stay
   orphans and the candidate is rejected — correctly a deviation). *)
let coarsen_orphans (ep : edit_pat) (ipats : edit_pat list) : edit_pat =
  let before_holes = collect_holes [] ep.before in
  let orphans =
    List.filter
      (fun h -> not (List.mem h before_holes))
      (collect_holes [] ep.after)
  in
  let after =
    List.fold_left
      (fun after h ->
        let subs =
          List.filter_map (fun ip -> subtree_at_hole ep.after ip.after h) ipats
        in
        let surfaces = List.filter_map empty_container_surface subs in
        match (subs, surfaces) with
        | _ :: _, v :: _
          when List.length surfaces = List.length subs
               && List.for_all (fun s -> s = v) surfaces -> (
            let node_type =
              List.fold_left
                (fun acc s ->
                  match (acc, s) with
                  | Some t, PNode n when t = n.node_type -> acc
                  | None, PNode n -> Some n.node_type
                  | _ -> Some "")
                None subs
            in
            match node_type with
            | Some nt when nt <> "" ->
                subst_hole h (Leaf { node_type = nt; value = v }) after
            | _ -> after)
        | _ -> after)
      ep.after orphans
  in
  { ep with after }

(** Re-specialize a cluster to its surviving instances: re-anti-unify their
    concrete patterns from scratch. Anti-unification only holes a position whose
    values differ among the inputs, so every hole in a freshly-formed cluster is
    witnessed by at least two distinct instantiations — but covering and safety
    shedding remove instances *after* formation, and a rule can otherwise be
    emitted with a hole its own remaining sites never vary on (more general than
    its evidence). Folding [anti_unify_edits] over the survivors' [ipat]s
    restores the witnessed-holes invariant: positions the survivors agree on
    collapse back to literals. The fold is safe without hole shifting because
    the inputs are fully concrete. Coarsening is re-applied at the end: the fold
    rebuilds the pattern from concrete ipats, so a coarsened orphan would
    otherwise resurface. *)
let respecialize (c : cluster) : cluster =
  match c.instances with
  | [] -> c
  | i :: rest ->
      let pattern =
        List.fold_left (fun acc j -> anti_unify_edits acc j.ipat) i.ipat rest
      in
      let pattern =
        coarsen_orphans pattern (List.map (fun j -> j.ipat) c.instances)
      in
      { c with pattern }

let cut_dendrogram ?(threshold = Cs_config.default.max_hole_fraction)
    ?(safe_instances = fun (_ : edit_pat) insts -> insts) min_size root =
  let is_coherent ep =
    let s = edit_size ep in
    (* The match side must always carry concrete content (a named
       leaf or a keyword token): a [-] side that is purely holes
       matches every node in the source and produces a degenerate
       "match anything, replace with this literal" rule. The [+]
       side may be hole-only — that is the asymmetric-reshape case,
       e.g. PHP's [array($X, $Y) -> [$X, $Y]] where the [array]
       keyword on the [-] side anchors the rule even though the
       [+] side is just brackets and holes. *)
    has_concrete ep.before && has_concrete_edit ep && no_orphan_after_holes ep
    && (s = 0 || float_of_int (edit_holes ep) /. float_of_int s < threshold)
  in
  let clusters = ref [] in
  let singletons = ref [] in
  (* Emit the node's pattern over its safe instances, shedding unsafe
     ones (design §3.1). Returns false when fewer than [min_size]
     instances survive the safety gate, so the caller can fall back to
     the node's children — typically a more concrete subtree whose
     pattern is safe at its sites. *)
  let try_emit ep insts =
    let safe = safe_instances ep insts in
    if List.length safe >= min_size then begin
      clusters := { pattern = ep; instances = safe } :: !clusters;
      List.iter
        (fun i -> if not (List.memq i safe) then singletons := i :: !singletons)
        insts;
      true
    end
    else false
  in
  (* M1.9c: before the coherence check, coarsen any [+]-side orphan to the
     instances' common container skeleton (tree-embedding, ordered). A
     candidate that would have been rejected for an orphan instead becomes
     a decomposable rule plus residuals. No-op when there is no orphan or
     none is a coarsenable container. *)
  let coarse ep insts =
    coarsen_orphans ep (List.map (fun (i : instance) -> i.ipat) insts)
  in
  let rec go node =
    let insts = dnode_instances node in
    let n = List.length insts in
    if n < min_size then singletons := insts @ !singletons
    else
      match node with
      | DLeaf (_, ep) ->
          let ep = coarse ep insts in
          if not (is_coherent ep && try_emit ep insts) then
            singletons := insts @ !singletons
      | DMerge m ->
          let pat = coarse m.pattern insts in
          if not (is_coherent pat && try_emit pat insts) then begin
            go m.left;
            go m.right
          end
  in
  go root;
  (!clusters, !singletons)

(* ── One-sided dendrogram (M1.6a) ──────────────────────────────── *)

(* [one_sided_cluster] now lives in {!Cs_types} (shared with {!Cs_fusion}). *)

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
          om_instances = os_dnode_instances arr.(i) @ os_dnode_instances arr.(j);
          om_left = arr.(i);
          om_right = arr.(j);
        }
    in
    let rest = ref [ merged ] in
    Array.iteri (fun k nd -> if k <> i && k <> j then rest := nd :: !rest) arr;
    nodes := !rest
  done;
  List.hd !nodes

let cut_os_dendrogram ?(threshold = Cs_config.default.max_hole_fraction) min_size side root =
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

(** Cluster one-sided candidates into [one_sided_cluster]s, separately for each
    side (Removeds with Removeds, Addeds with Addeds). Candidates that don't
    cluster (singletons) are discarded at this stage. *)
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

(** A removal-only [.pat] body for a concrete removed text (no holes): every
    line of [text] prefixed with [- ]. Used by the safety gate's
    concrete-regroup fallback, where a group of instances shares the removed
    text verbatim and no [pat_node] is at hand. *)
let removal_body_of_text (text : string) : string =
  let buf = Buffer.create (String.length text + 32) in
  Buffer.add_string buf "@@\nmatch: strict\n@@\n";
  List.iter
    (fun line -> Buffer.add_string buf (Printf.sprintf "- %s\n" line))
    (String.split_on_char '\n' text);
  Buffer.contents buf

(** Safe instances of a removal-only cluster, with a concrete-regroup fallback
    (design §3.1). First the cluster's own (possibly holed) pattern is
    safety-checked per site; if fewer than [min_support] sites survive — the
    over-merge case, e.g. [- import _H0] whose application would remove every
    import in the file — the instances are regrouped by their literal removed
    text and each group ≥ [min_support] is gated with its own concrete pattern.
    The fallback recovers the concrete-majority rule that the merged hole erased
    (intermediate generalisations between the hole and the concrete texts are
    not currently recovered). Returns [(pattern_text, instances)] groups to
    emit. *)
let safe_removal_groups ~ctx ~site_db ?(min_support = Cs_config.default.min_support) (c : one_sided_cluster)
    : (string * one_sided_instance list) list =
  let safe_with pattern_text (i : one_sided_instance) =
    i.os_language <> ""
    &&
    match Hashtbl.find_opt site_db i.os_file with
    | None -> false
    | Some si -> site_safe ~ctx ~language:i.os_language ~pattern_text si
  in
  let pattern_text = render_removal_only_body c.os_cluster_pattern in
  let safe = List.filter (safe_with pattern_text) c.os_cluster_instances in
  if List.length safe >= min_support then [ (pattern_text, safe) ]
  else begin
    (* Concrete-regroup fallback: group by removed text. *)
    let groups : (string, one_sided_instance list) Hashtbl.t =
      Hashtbl.create 8
    in
    List.iter
      (fun (i : one_sided_instance) ->
        let prev = try Hashtbl.find groups i.os_text with Not_found -> [] in
        Hashtbl.replace groups i.os_text (i :: prev))
      c.os_cluster_instances;
    Hashtbl.fold
      (fun text insts acc ->
        if List.length insts < min_support then acc
        else
          let body = removal_body_of_text text in
          let safe = List.filter (safe_with body) insts in
          if List.length safe >= min_support then (body, List.rev safe) :: acc
          else acc)
      groups []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  end

