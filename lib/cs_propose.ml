(** Change-summary proposal (design §3.1, §3.2): extract change pairs from a
    per-file {!Tree_diff} at every level of each change chain, and build the
    candidate channels — multi-level pairs, content-extraction pairs, delta-keyed
    pairs, and anchored lattice-descent variants. A weak proposer costs recall,
    never honesty (design §3.3); semantics are decided later by {!Cs_evaluate}.
    Depends on {!Cs_types} and {!Cs_pattern}. *)

open Cs_types
open Cs_pattern

(* ── Change-pair extraction ──────────────────────────────────────── *)

(** Fraction of a node's direct children whose change is non-[Same]. Higher
    ratio means the change converges at this level; lower means this node is
    mostly unchanged boilerplate around a deeper change. *)
let change_ratio (child_changes : Tree_diff.child_change list) : float =
  let total = List.length child_changes in
  if total = 0 then 0.0
  else
    let changed =
      List.fold_left
        (fun n c ->
          match c with
          | Tree_diff.Same _ -> n
          | Tree_diff.Changed _ | Tree_diff.Added _ | Tree_diff.Removed _ ->
              n + 1)
        0 child_changes
    in
    float_of_int changed /. float_of_int total

let has_direct_structural (cc : Tree_diff.child_change list) =
  List.exists
    (function Tree_diff.Added _ | Tree_diff.Removed _ -> true | _ -> false)
    cc

let rec subtree_has_structural (cc : Tree_diff.child_change list) =
  List.exists
    (function
      | Tree_diff.Added _ | Tree_diff.Removed _ -> true
      | Tree_diff.Changed { change = Modified { child_changes }; _ } ->
          subtree_has_structural child_changes
      | _ -> false)
    cc

(* Hashes of every subtree of [n] (incl. [n] itself). [Tree.hash] is a
   structural, position-independent digest, so it doubles as hdiff's
   "which common subtree" oracle (§4.3): a node is a common subtree of
   two trees iff its hash appears in both. Comparing hashes across the
   before/after parses is sound — the digest excludes position and source
   buffer. *)

(** Emit change pairs at emission points selected by the structure of the diff,
    not by depth:

    - Every [Replaced] leaf — the natural unit for a pure rename.
    - Every [Modified] ancestor that either (a) has an [Added]/[Removed] child
      directly (the locus of a structural change), or (b) has [Added]/[Removed]
      somewhere in its subtree {e and} enough of its own direct children are
      non-[Same] to make it a plausible lift target (ratio ≥
      [emission_threshold]).

    Case (b) lifts through arbitrarily long wrapper chains — grammar- agnostic,
    no depth bound — because at each wrapper level both conditions (structural
    descendant, ratio ≥ threshold) still hold. The lift stops at the first
    ancestor where most children are [Same], which is typically the enclosing
    function/block. The covering pass then ranks the candidate levels and picks
    the tightest informative one.

    Cases without any [Added]/[Removed] anywhere emit {e only} at [Replaced]
    leaves — no ancestor candidates, so no churn from cluster-level decisions in
    what should already be leaf-level renames. Byte-range deduplication prevents
    duplicate ancestor emissions when multiple descendant chains meet at the
    same ancestor. *)
let rec subtree_hashes (n : Tree.src Tree.t) (acc : int list) : int list =
  List.fold_left
    (fun a (c : Tree.src Tree.child) -> subtree_hashes c.node a)
    (n.Tree.hash :: acc) n.Tree.children

(* §4.3 cross-side alignment, extraction case. When a [Modified] node has
   a [Removed] child [r] and an [Added] child [a] where one is a subtree
   of the other (a wrapper was added or removed around a preserved
   value), the after reuses a sub-part of the before — GumTree reports it
   as an unrelated Removed+Added rather than a rewrite. Emit it as a
   two-sided change pair [(r, a)] so it forms an extraction rule
   ([box($H).get() ⤳ $H]); the existing cross-file anti-unification then
   binds the shared hole by content (the preserved value coincides on
   both sides). The original one-sided Removed/Added candidates are left
   intact — this augments, and selection (§3.3) picks whichever covers
   more. *)
let extraction_pairs (child_changes : Tree_diff.child_change list) :
    (Tree.src Tree.t * Tree.src Tree.t) list =
  let removeds =
    List.filter_map
      (function Tree_diff.Removed { node } -> Some node | _ -> None)
      child_changes
  in
  let addeds =
    List.filter_map
      (function Tree_diff.Added { node } -> Some node | _ -> None)
      child_changes
  in
  let subtree_of x y =
    (* is [x] a subtree of [y] (by structural hash)? *)
    List.mem x.Tree.hash (subtree_hashes y [])
  in
  List.concat_map
    (fun (r : Tree.src Tree.t) ->
      List.filter_map
        (fun (a : Tree.src Tree.t) ->
          if r.Tree.hash <> a.Tree.hash && (subtree_of a r || subtree_of r a)
          then Some (r, a)
          else None)
        addeds)
    removeds

let collect_change_pairs_multi ?(emission_threshold = Cs_config.default.emission_threshold) (d : Tree_diff.diff)
    : Tree_diff.change_pair list =
  let out = ref [] in
  let emitted : (int * int, unit) Hashtbl.t = Hashtbl.create 16 in
  let emit (b : Tree.src Tree.t) (a : Tree.src Tree.t) =
    let key = (b.start_byte, b.end_byte) in
    if not (Hashtbl.mem emitted key) then begin
      Hashtbl.add emitted key ();
      out :=
        {
          Tree_diff.before_node = b;
          after_node = a;
          before_source = d.before_source;
          after_source = d.after_source;
        }
        :: !out
    end
  in
  let rec collect ~b ~a = function
    | Tree_diff.Unchanged -> ()
    | Tree_diff.Replaced -> emit b a
    | Tree_diff.Modified { child_changes } ->
        (* Emit at every Modified ancestor along the change chain.
           A given level may produce a pattern whose rendered text
           cannot fire as a [.pat] rule (e.g. a [property_identifier]
           in isolation re-parses as [identifier]; a [jsx_attribute]
           in isolation re-parses as an [assignment_expression]).
           Generating candidates at all levels lets the applicability
           filter reject unworkable ones while a coherent ancestor
           level (typically [member_expression] or
           [jsx_self_closing_element]) survives. The covering pass
           then picks the smallest among applicable candidates,
           resolving overlap by byte range. *)
        emit b a;
        List.iter (fun (r, a) -> emit r a) (extraction_pairs child_changes);
        List.iter
          (function
            | Tree_diff.Changed { before; after; change } ->
                collect ~b:before ~a:after change
            | _ -> ())
          child_changes
  in
  (match d.root_change with
  | Tree_diff.Modified { child_changes } ->
      List.iter (fun (r, a) -> emit r a) (extraction_pairs child_changes);
      List.iter
        (function
          | Tree_diff.Changed { before; after; change } ->
              collect ~b:before ~a:after change
          | _ -> ())
        child_changes
  | Tree_diff.Replaced -> emit d.before_root d.after_root
  | Tree_diff.Unchanged -> ());
  List.rev !out

(** One-sided candidate extraction (M1.5). Walks the diff and emits every
    [Added]/[Removed] child subtree it encounters — including ones nested inside
    [Changed.Modified] chains. These are collected so M1.6 Jaccard fusion can
    pair them with two-sided clusters (e.g. a removed import anchoring a renamed
    call). They do not become standalone rules. *)
let lookahead_one_sided (d : Tree_diff.diff) : (side * Tree.src Tree.t) list =
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
  | Tree_diff.Modified { child_changes } -> List.iter visit_child child_changes
  | Tree_diff.Replaced | Tree_diff.Unchanged -> ());
  List.rev !out

(* ── Delta-keyed pair variant (§3.2) ─────────────────────────────── *)

(** Scope-holed variant of a change pair: the pair's preserved children (equal
    structural hash on both sides) become shared holes — the same metavar bound
    on before and after — while the changed children stay concrete. This keys
    clustering on the delta itself instead of on whatever surrounding shape the
    dendrogram's merge order happens to anti-unify first, pooling one delta's
    support across heterogeneous anchors. It also evades the rendered-pattern
    re-parse mismatch: a scope name kept concrete re-parses with a
    neutral-context node type ([simple_identifier] where the source position has
    [type_identifier]) and the gate then finds zero fires; a hole is
    node-type-agnostic, and the delta's own leaves keep their grammatical role.
    See design §3.2 "Diagnosis". Load-bearing on the real soak corpora — it
    shapes the type-parameter rename family. The [ts_typearg_rename_delta]
    golden fixture guards it: disabling this channel makes that case fall back
    to a coarse whole-block rewrite and the test fails.

    Returns [None] when the variant would be useless: a leaf-shaped node
    (mirrors [of_src]'s leaf rules), no preserved child (the variant equals the
    concrete pair), no changed child, or an incoherent result the dendrogram cut
    would reject anyway. *)
let delta_keyed_pair (cp : Tree_diff.change_pair) : edit_pat option =
  let b = cp.before_node and a = cp.after_node in
  let pnode_shaped source (n : Tree.src Tree.t) =
    n.children <> []
    && (not (has_silent_concrete_delimiters ~source ~node:n))
    && not (has_quote_delim_children ~source ~node:n)
  in
  if
    not
      (pnode_shaped cp.before_source b
      && pnode_shaped cp.after_source a
      && b.node_type = a.node_type)
  then None
  else begin
    let kept (n : Tree.src Tree.t) =
      List.filter
        (fun (c : Tree.src Tree.child) -> not c.node.is_extra)
        n.children
    in
    let bks = kept b and aks = kept a in
    let aks_arr = Array.of_list aks in
    let used = Array.make (Array.length aks_arr) false in
    (* Greedy in-order hash matching: a before-child is preserved iff an
       unconsumed after-child has the same structural hash. Only NAMED
       preserved children become holes — an anonymous token (operator,
       punctuation) is structure, not content, and holing it produces
       nonsense patterns like [holder _H0 null]; matched anonymous
       children stay concrete (their text is identical anyway). *)
    let next_hole = ref 0 in
    let b_assign =
      List.map
        (fun (c : Tree.src Tree.child) ->
          let m = ref None in
          Array.iteri
            (fun i (ac : Tree.src Tree.child) ->
              if !m = None && (not used.(i)) && ac.node.hash = c.node.hash then begin
                used.(i) <- true;
                m := Some i
              end)
            aks_arr;
          match !m with
          | Some i when c.node.is_named ->
              let h = !next_hole in
              incr next_hole;
              (c, `Holed (i, h))
          | Some _ -> (c, `Matched)
          | None -> (c, `Delta))
        bks
    in
    let n_holes =
      List.length
        (List.filter
           (fun (_, m) -> match m with `Holed _ -> true | _ -> false)
           b_assign)
    in
    let n_matched_b =
      List.length
        (List.filter
           (fun (_, m) -> match m with `Delta -> false | _ -> true)
           b_assign)
    in
    let n_delta_b = List.length bks - n_matched_b in
    let n_delta_a =
      List.length aks
      - Array.fold_left (fun n u -> if u then n + 1 else n) 0 used
    in
    if n_holes = 0 || (n_delta_b = 0 && n_delta_a = 0) then None
    else begin
      let hole_of_a = Array.make (Array.length aks_arr) None in
      List.iter
        (fun ((_ : Tree.src Tree.child), m) ->
          match m with `Holed (i, h) -> hole_of_a.(i) <- Some h | _ -> ())
        b_assign;
      let keep (n : Tree.src Tree.t) = not n.is_extra in
      let before =
        PNode
          {
            node_type = b.node_type;
            is_named = b.is_named;
            children =
              List.map
                (fun ((c : Tree.src Tree.child), m) ->
                  {
                    field_name = c.field_name;
                    child =
                      (match m with
                      | `Holed (_, h) -> Hole h
                      | `Matched | `Delta -> of_src cp.before_source c.node);
                  })
                b_assign;
            template = build_template ~source:cp.before_source ~node:b ~keep ();
          }
      in
      let after =
        PNode
          {
            node_type = a.node_type;
            is_named = a.is_named;
            children =
              List.mapi
                (fun i (c : Tree.src Tree.child) ->
                  {
                    field_name = c.field_name;
                    child =
                      (match hole_of_a.(i) with
                      | Some h -> Hole h
                      | None -> of_src cp.after_source c.node);
                  })
                aks;
            template = build_template ~source:cp.after_source ~node:a ~keep ();
          }
      in
      let ep = { before; after } in
      if
        has_concrete ep.before && has_concrete_edit ep
        && no_orphan_after_holes ep
        && hole_frac ep < Cs_config.default.max_hole_fraction
      then Some ep
      else None
    end
  end

(* ── Anchored variant (§3.2 lattice descent) ─────────────────────── *)

(** Anchored lattice-descent variants (§3.2): the pair's own preserved children
    stay CONCRETE — they are the anchor that discriminates a context-dependent change
    — while preserved content *inside* the changed-child chain becomes shared
    holes, recursively along the single-changed-child path. For a rename applied
    at [state = X(args)] this yields [state = X(_H0) ⤳ state = Y(_H0)]: the
    [state =] anchor literal, the site-specific args holed.

    Each variant carries a *delta key* — the changed leaves' source text on both
    sides — so that support can be pooled on the delta across sites whose
    anchors differ (design §3.2: support and min_support are counted on the
    delta cluster; the anchored variants are its site-local realisations).
    Variants with no holes are kept only when the change is leaf-level (the
    [::X ⤳ ::Y] case, where the anchor is pure structure); deeper hole-free
    variants are just the concrete base pair again. *)
let anchored_variants (cp : Tree_diff.change_pair) :
    (edit_pat * string * (int * int)) list =
  let pnode_shaped source (n : Tree.src Tree.t) =
    n.children <> []
    && (not (has_silent_concrete_delimiters ~source ~node:n))
    && not (has_quote_delim_children ~source ~node:n)
  in
  let next_hole = ref 0 in
  (* Hole a preserved subtree. A node whose surface carries its own
     delimiters as unnamed children (the parens of an argument list,
     the angle brackets of type arguments) cannot be replaced by a bare
     hole — the render would glue [_H0] to the preceding token
     ([WorkflowState_H0]). Keep such a shell concrete and hole its
     named children. Quote-delimited and silently-delimited tokens
     (string literals) hole whole: a metavar inside the quotes would be
     read as string content. *)
  let rec hole_subtree source (n : Tree.src Tree.t) : pat_node =
    let fresh () =
      let h = !next_hole in
      incr next_hole;
      Hole h
    in
    if n.children = [] then fresh ()
    else if
      has_silent_concrete_delimiters ~source ~node:n
      || has_quote_delim_children ~source ~node:n
    then fresh ()
    else if
      List.exists
        (fun (c : Tree.src Tree.child) -> not c.node.is_named)
        n.children
    then
      let keep (c : Tree.src Tree.t) = not c.is_extra in
      PNode
        {
          node_type = n.node_type;
          is_named = n.is_named;
          children =
            List.filter_map
              (fun (c : Tree.src Tree.child) ->
                if not (keep c.node) then None
                else if c.node.is_named then
                  Some { field_name = c.field_name; child = fresh () }
                else
                  Some
                    { field_name = c.field_name; child = of_src source c.node })
              n.children;
          template = build_template ~source ~node:n ~keep ();
        }
    else if List.length n.children = 1 then
      (* All-named single-child wrapper (e.g. [call_suffix] around
         [value_arguments]): descend — the delimiter shell, if any,
         lives below. *)
      let keep (c : Tree.src Tree.t) = not c.is_extra in
      PNode
        {
          node_type = n.node_type;
          is_named = n.is_named;
          children =
            List.map
              (fun (c : Tree.src Tree.child) ->
                {
                  field_name = c.field_name;
                  child = hole_subtree source c.node;
                })
              n.children;
          template = build_template ~source ~node:n ~keep ();
        }
    else fresh ()
  in
  (* Match the kept children of [bn]/[an] by structural hash (greedy,
     in order). *)
  let match_children (bn : Tree.src Tree.t) (an : Tree.src Tree.t) =
    let kept (n : Tree.src Tree.t) =
      List.filter
        (fun (c : Tree.src Tree.child) -> not c.node.is_extra)
        n.children
    in
    let bks = kept bn and aks = kept an in
    let aks_arr = Array.of_list aks in
    let used = Array.make (Array.length aks_arr) false in
    let b_assign =
      List.map
        (fun (c : Tree.src Tree.child) ->
          let m = ref None in
          Array.iteri
            (fun i (ac : Tree.src Tree.child) ->
              if !m = None && (not used.(i)) && ac.node.hash = c.node.hash then begin
                used.(i) <- true;
                m := Some i
              end)
            aks_arr;
          (c, !m))
        bks
    in
    (b_assign, aks, used)
  in
  let recursable (cb : Tree.src Tree.child) (ca : Tree.src Tree.child) =
    cb.node.node_type = ca.node.node_type
    && pnode_shaped cp.before_source cb.node
    && pnode_shaped cp.after_source ca.node
  in
  (* The changed-children pairing at a level: when before- and
     after-side unmatched counts are equal, zip them in order. *)
  let zipped_unmatched b_assign aks used =
    let unmatched_b =
      List.filter_map (fun (c, m) -> if m = None then Some c else None) b_assign
    in
    let unmatched_a_idx = ref [] in
    List.iteri
      (fun i (c : Tree.src Tree.child) ->
        if not used.(i) then unmatched_a_idx := (i, c) :: !unmatched_a_idx)
      aks;
    let unmatched_a_idx = List.rev !unmatched_a_idx in
    if
      List.length unmatched_b = List.length unmatched_a_idx && unmatched_b <> []
    then Some (List.combine unmatched_b unmatched_a_idx)
    else None
  in
  (* Enumerate path selectors: at each MULTI-pair branch point the
     selector names the zipped pair to descend ([] = stop there with a
     compound delta); single recursable pairs descend automatically.
     Depth- and width-capped — variants beyond the cap are simply not
     proposed (coverage falls to residuals, never to wrong output). *)
  let rec enum_selectors (bn : Tree.src Tree.t) (an : Tree.src Tree.t) depth :
      int list list =
    if depth > Cs_config.default.selector_depth_limit then [ [] ]
    else
      let b_assign, aks, used = match_children bn an in
      match zipped_unmatched b_assign aks used with
      | None -> [ [] ]
      | Some [ (cb, (_, ca)) ] ->
          if recursable cb ca then enum_selectors cb.node ca.node (depth + 1)
          else [ [] ]
      | Some zipped ->
          (* At a branch point each zipped pair is a possible path:
             recursable pairs descend (deeper selectors), the others
             terminate as the chosen delta with siblings holed. [] =
             stop here with the compound delta. *)
          []
          :: List.concat
               (List.mapi
                  (fun j (cb, ((_ : int), ca)) ->
                    if recursable cb ca then
                      List.map
                        (fun s -> j :: s)
                        (enum_selectors cb.node ca.node (depth + 1))
                    else [ [ j ] ])
                  zipped)
  in
  (* Build one variant for a given selector. Mutable per-run state. *)
  let build selector0 =
    next_hole := 0;
    let sel = ref selector0 in
    let b_delta = ref [] and a_delta = ref [] in
    let all_leaves = ref true in
    (* Byte span of the delta — the identity of the CHANGE itself,
       shared by this change's anchored variants at every ancestor
       level (the pair spans differ per level and would overcount one
       change as several pool sites). *)
    let d_start = ref max_int and d_end = ref 0 in
    let record_delta side (source : string) (n : Tree.src Tree.t) =
      let t = Tree.text source n in
      (match side with
      | `B ->
          b_delta := t :: !b_delta;
          d_start := min !d_start n.start_byte;
          d_end := max !d_end n.end_byte
      | `A -> a_delta := t :: !a_delta);
      if n.children <> [] then all_leaves := false
    in
    (* Build both sides at one level. [holed_preserved] says whether
       preserved named children become holes (Inner mode) or stay
       concrete (Anchor mode, top level only). *)
    let rec level ~holed_preserved (bn : Tree.src Tree.t) (an : Tree.src Tree.t)
        : pat_node * pat_node =
      let b_assign, aks, used = match_children bn an in
      let zipped = zipped_unmatched b_assign aks used in
      (* Which zipped pair continues the chain? Single recursable pairs
         descend automatically; multi-pair branch points consult the
         selector; everything else is a compound delta. *)
      (* [`Descend] continues the chain into pair j; [`DeltaPair]
         terminates at pair j (a leaf or non-recursable pair chosen as
         THE delta), holing the sibling pairs. *)
      let chosen =
        match zipped with
        | Some [ (cb, (ai, ca)) ] when recursable cb ca ->
            Some (`Descend (cb.node, ai, ca.node))
        | Some zs when List.length zs > 1 -> (
            match !sel with
            | j :: rest when j >= 0 && j < List.length zs ->
                let cb, (ai, ca) = List.nth zs j in
                sel := rest;
                if recursable cb ca then Some (`Descend (cb.node, ai, ca.node))
                else Some (`DeltaPair (cb.node, ai, ca.node))
            | _ -> None)
        | _ -> None
      in
      let descend =
        match chosen with Some (`Descend d) -> Some d | _ -> None
      in
      (* Sibling changed pairs at a descended branch point become
         SHARED holes: identical on both sides, they render as context
         lines, so the rule does not claim those changes — they stay
         with other rules or residuals. *)
      let sibling_hole : (int, pat_node) Hashtbl.t = Hashtbl.create 4 in
      let b_sibling : (Tree.src Tree.t, pat_node) Hashtbl.t =
        Hashtbl.create 4
      in
      (match (chosen, zipped) with
      | Some (`Descend (ub, _, _) | `DeltaPair (ub, _, _)), Some zs -> (
          List.iter
            (fun ((cb : Tree.src Tree.child), ((ai : int), _)) ->
              if cb.node != ub then begin
                (* Built from the before side and shared verbatim with
                   the after side: identical on both sides, the holed
                   sibling renders as context lines — the rule does not
                   claim that change. [hole_subtree] keeps delimiter
                   shells concrete so the render cannot glue. *)
                let hp = hole_subtree cp.before_source cb.node in
                Hashtbl.replace sibling_hole ai hp;
                Hashtbl.replace b_sibling cb.node hp
              end)
            zs;
          match chosen with
          | Some (`DeltaPair (ub, _, ua)) ->
              record_delta `B cp.before_source ub;
              record_delta `A cp.after_source ua
          | _ -> ())
      | None, _ -> (
          (* Compound delta: every unmatched child is delta content. *)
          match zipped with
          | Some zs ->
              List.iter
                (fun ( (cb : Tree.src Tree.child),
                       (_, (ca : Tree.src Tree.child)) ) ->
                  record_delta `B cp.before_source cb.node;
                  record_delta `A cp.after_source ca.node)
                zs
          | None ->
              List.iter
                (fun ((c : Tree.src Tree.child), m) ->
                  if m = None then record_delta `B cp.before_source c.node)
                b_assign;
              List.iteri
                (fun i (c : Tree.src Tree.child) ->
                  if not used.(i) then record_delta `A cp.after_source c.node)
                aks)
      | Some _, None -> ());
      let hole_of_a = Array.make (List.length aks) None in
      let inner_ap = ref None in
      let b_children =
        List.map
          (fun ((c : Tree.src Tree.child), m) ->
            let child =
              match m with
              | Some i when c.node.is_named && holed_preserved ->
                  (* Shared structure: the after side reuses the same
                     pat_node, so its holes carry the same indices. *)
                  let hp = hole_subtree cp.before_source c.node in
                  hole_of_a.(i) <- Some hp;
                  hp
              | Some i ->
                  let ac = List.nth aks i in
                  hole_of_a.(i) <- Some (of_src cp.after_source ac.node);
                  of_src cp.before_source c.node
              | None -> (
                  match descend with
                  | Some (ub, ai, ua) when ub == c.node ->
                      let bp, ap = level ~holed_preserved:true ub ua in
                      inner_ap := Some ap;
                      hole_of_a.(ai) <- None;
                      bp
                  | _ -> (
                      match Hashtbl.find_opt b_sibling c.node with
                      | Some hp -> hp
                      | None -> of_src cp.before_source c.node))
            in
            { field_name = c.field_name; child })
          b_assign
      in
      let a_children =
        List.mapi
          (fun i (c : Tree.src Tree.child) ->
            let child =
              match descend with
              | Some (_, ai, ua) when ai = i && ua == c.node -> (
                  match !inner_ap with
                  | Some ap -> ap
                  | None -> of_src cp.after_source c.node)
              | _ -> (
                  match hole_of_a.(i) with
                  | Some p -> p
                  | None -> (
                      match Hashtbl.find_opt sibling_hole i with
                      | Some hp -> hp
                      | None -> of_src cp.after_source c.node))
            in
            { field_name = c.field_name; child })
          aks
      in
      let keep (n : Tree.src Tree.t) = not n.is_extra in
      ( PNode
          {
            node_type = bn.node_type;
            is_named = bn.is_named;
            children = b_children;
            template = build_template ~source:cp.before_source ~node:bn ~keep ();
          },
        PNode
          {
            node_type = an.node_type;
            is_named = an.is_named;
            children = a_children;
            template = build_template ~source:cp.after_source ~node:an ~keep ();
          } )
    in
    let before, after =
      level ~holed_preserved:false cp.before_node cp.after_node
    in
    let ep = { before; after } in
    let holes = edit_holes ep in
    (* A pure-insertion delta (empty before side) is un-anchorable: an
       anchored realisation would claim a one-site insertion with a
       support-1 rule, which states the change worse than its residual
       (the §5.5 pure-additions philosophy). *)
    if !b_delta = [] then None
    else if holes = 0 && not !all_leaves then None
    else if
      has_concrete ep.before && has_concrete_edit ep && no_orphan_after_holes ep
      && hole_frac ep < Cs_config.default.max_hole_fraction
    then
      let key =
        String.concat "\x00" (List.rev !b_delta)
        ^ "\x01"
        ^ String.concat "\x00" (List.rev !a_delta)
      in
      Some (ep, key, (!d_start, !d_end))
    else None
  in
  let b = cp.before_node and a = cp.after_node in
  if
    not
      (pnode_shaped cp.before_source b
      && pnode_shaped cp.after_source a
      && b.node_type = a.node_type
      (* Anchored realisations are site-local statements of a change;
         a pair spanning more than ~a statement produces huge patterns
         whose gate evaluations dominate runtime and which the
         least-concrete tie-break would never pick anyway. *)
      && b.end_byte - b.start_byte <= Cs_config.default.cost_byte_limit
      && a.end_byte - a.start_byte <= Cs_config.default.cost_byte_limit)
  then []
  else begin
    let sels = enum_selectors b a 0 in
    let sels = List.filteri (fun i _ -> i < Cs_config.default.max_selectors_per_pair) sels in
    let seen = Hashtbl.create 8 in
    List.filter_map
      (fun s ->
        match build s with
        | Some ((ep, _, _) as v) ->
            if Hashtbl.mem seen ep then None
            else begin
              Hashtbl.add seen ep ();
              Some v
            end
        | None -> None)
      sels
  end

(* Returns (base clusters, delta-keyed clusters, anchored variants).
   The base clusters feed the dendrogram as before; the delta-keyed and
   anchored variants deliberately stay OUT of it — adding them as
   dendrogram inputs changes its merge geometry for everyone (observed:
   displaced extraction and call-level rules on the golden cases). The
   delta-keyed variants pool by exact pattern identity; the anchored
   variants pool by DELTA key, so a context-dependent change whose
   anchors differ per site still accumulates delta-level support. *)
let collect_initial_clusters ?on_file ~ctx (cs : changeset) :
    cluster list * cluster list * (string * (int * int) * cluster) list =
  let modified =
    List.filter (function Modified _ -> true | _ -> false) cs.files
  in
  let total = List.length modified in
  let initial = ref [] in
  let delta = ref [] in
  let anchored = ref [] in
  List.iteri
    (fun i fc ->
      match fc with
      | Modified { path; language; before_source; after_source } -> (
          (match on_file with
          | Some f -> f ~idx:(i + 1) ~total ~path
          | None -> ());
          try
            let bt = Tree.parse ~ctx ~language before_source in
            let at = Tree.parse ~ctx ~language after_source in
            let d = Tree_diff.diff ~before:bt ~after:at in
            List.iter
              (fun (cp : Tree_diff.change_pair) ->
                let bt = Tree.text cp.before_source cp.before_node in
                let at = Tree.text cp.after_source cp.after_node in
                let ep =
                  {
                    before = of_src cp.before_source cp.before_node;
                    after = of_src cp.after_source cp.after_node;
                  }
                in
                let inst =
                  {
                    before_text = bt;
                    after_text = at;
                    before_full_source = before_source;
                    file = path;
                    line = cp.before_node.start_point.row + 1;
                    language;
                    site_start = cp.before_node.start_byte;
                    site_end = cp.before_node.end_byte;
                    ipat = ep;
                  }
                in
                initial := { pattern = ep; instances = [ inst ] } :: !initial;
                (* §3.2 delta-keyed variant: same site, scope-holed
                   pattern, collected on its own channel. *)
                (match delta_keyed_pair cp with
                | Some dep ->
                    delta :=
                      {
                        pattern = dep;
                        instances = [ { inst with ipat = dep } ];
                      }
                      :: !delta
                | None -> ());
                (* §3.2 anchored variants: preserved siblings literal,
                   changed-chain interior holed, keyed by the delta —
                   one per path choice at branching levels. *)
                match anchored_variants cp with
                | vs ->
                    List.iter
                      (fun (aep, key, span) ->
                        anchored :=
                          ( key,
                            span,
                            {
                              pattern = aep;
                              instances = [ { inst with ipat = aep } ];
                            } )
                          :: !anchored)
                      vs
                | exception ((Stack_overflow | Out_of_memory | Sys.Break) as e)
                  ->
                    raise e
                | exception e ->
                    Cs_trace.trace "anchored_variants exn at %s:%d: %s\n%!" path
                      inst.line (Printexc.to_string e))
              (collect_change_pairs_multi d)
          with
          | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
          | e ->
              Cs_trace.trace "collect_initial_clusters: skipping %s: %s\n%!" path
                (Printexc.to_string e))
      | Added _ | Deleted _ -> ())
    modified;
  (!initial, !delta, !anchored)

(** Collect one-sided candidates (M1.5) across a changeset's [Modified] files.
    Each candidate carries its pat_node shape and site metadata. Used internally
    by M1.6 fusion; not wired into M1 rule output. *)
let collect_one_sided_candidates ?on_file ~ctx (cs : changeset) :
    one_sided_candidate list =
  let modified =
    List.filter (function Modified _ -> true | _ -> false) cs.files
  in
  let total = List.length modified in
  let out = ref [] in
  List.iteri
    (fun i fc ->
      match fc with
      | Modified { path; language; before_source; after_source } -> (
          (match on_file with
          | Some f -> f ~idx:(i + 1) ~total ~path
          | None -> ());
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
                    os_start_byte = node.start_byte;
                    os_end_byte = node.end_byte;
                  }
                in
                out :=
                  { os_pat = of_src source node; os_instance = inst } :: !out)
              (lookahead_one_sided d)
          with
          | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
          | e ->
              Cs_trace.trace "collect_one_sided_candidates: skipping %s: %s\n%!"
                path (Printexc.to_string e))
      | Added _ | Deleted _ -> ())
    modified;
  List.rev !out
