(** Change-summary proposal (design §3.1, §3.2): extract change pairs from a
    per-file {!Tree_diff} at every level of each change chain, and build the
    candidate channels — multi-level pairs, content-extraction pairs,
    delta-keyed pairs, and anchored lattice-descent variants. A weak proposer
    costs recall, never honesty (design §3.3); semantics are decided later by
    {!Cs_evaluate}. Depends on {!Cs_types} and {!Cs_pattern}. *)

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

(** Emit change pairs at every [Replaced] leaf and at {e every} [Modified]
    ancestor along each change chain — deliberately unfiltered (see the
    rejected-gates note inside [collect_change_pairs_multi]): downstream
    applicability, coherence, and the safety gate discard unworkable levels, and
    {!Cs_config.dendrogram_bucket_cap} bounds clustering cost at corpus scale.
    Byte-range deduplication prevents duplicate ancestor emissions when multiple
    descendant chains meet at the same ancestor. *)
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

let collect_change_pairs_multi (d : Tree_diff.diff) : Tree_diff.change_pair list
    =
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
           resolving overlap by byte range.

           Two emission gates were tried at corpus scale and REJECTED
           (2026-07): a structural gate (locus + change-density lift, the
           [emission_threshold] design) starves the pure-rename lift —
           renames have no Added/Removed anywhere, and member_expression
           sits at ratio 1/3 — failing 14 golden fixtures; a per-side
           node-count cap kills whole-declaration rules whose emissions
           are big but whose patterns ellipsize small (webxforge's
           removeExtends family). Scale is handled downstream by
           {!Cs_config.dendrogram_bucket_cap}: emission stays complete,
           clustering samples. *)
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
        && no_orphan_after_holes ep && no_junk_passthrough ep
        && hole_frac ep < Cs_config.default.max_hole_fraction
      then Some ep
      else None
    end
  end

(* ── Anchored variant (§3.2 lattice descent) ─────────────────────── *)

(** Ellipsis-context form of one level (§3.2, list bloat): when a level of the
    changed-child chain is a bracket-delimited list — its first and last kept
    children are a matching anonymous bracket pair — and exactly one child
    between the brackets changed, the list's other children collapse into
    [Ellipsis] runs around the changed child instead of staying concrete (anchor
    mode) or becoming per-child holes (inner mode). Both alternatives bake the
    list's ARITY into the pattern, so a one-parameter type rename in
    constructors of different arities fragments into per-arity rules; the
    ellipsis form renders as

    {v
    (
    ...
    private readonly _H0: DestroyRef
    ...
    )
    v}

    which is arity- and position-independent (a leading/trailing [...] matches
    zero siblings, and absorbs the separator next to the changed child), so the
    realisation is textually identical across sites and pools by identity. The
    synthetic template puts every part on its own line: the surgical renderer
    aligns lines, and the [...] lines must land in the common prefix/suffix to
    render as context — a [+ ...] line would be a fresh unbound sequence
    binding.

    Keeping the brackets concrete is load-bearing beyond readability: a bare
    fragment without them re-parses with neutral-context leaf types and the gate
    finds zero fires (the §3.2 re-parse mismatch); [<] is worst — bare angle
    brackets re-parse as JSX/comparison. The bracket level typically sits under
    a concrete head kept by anchor mode ([constructor], the generic type name),
    so the emitted rule reads [head( ... delta ... )].

    A one-sided before-run — a contiguous stretch of unmatched children with
    nothing unmatched on the after side — is a DELETION from the list; the run
    (the element plus the adjacent separator tree-diff leaves unmatched) renders
    as [-] lines between the context ellipses, so the separator is deleted
    explicitly rather than by cleanup magic.

    The mirror one-sided case — a contiguous after-side run with nothing
    unmatched on the before side — is an INSERTION into the list. Two
    asymmetries with deletion: the run renders as [+] lines whose text IS the
    output (a deletion's [-] spans delete source bytes, so its line split never
    shows), so the whole run — element plus separator — is glued onto one [+]
    line; and a [+] line is only anchorable next to a concrete token (the
    matcher rejects one flanked by two [...] runs), so both sides get a SINGLE
    ellipsis and only edge positions qualify: a run adjacent to the closer
    renders [{ ... + X, }], one adjacent to the opener/head [{ + X, ... }].
    Mid-list runs return [None] — their position inside the captured run would
    be arbitrary — and the change stays residual.

    Returns [None] (caller falls back to the concrete form) unless the bracket
    shape holds and the unmatched children form exactly one changed child on
    each side (rewrite), one contiguous before-side run with a named member
    (deletion), or one contiguous edge-positioned after-side run with a named
    member (insertion — [Lv_insertion] carries the before-side anchor byte, the
    level-independent site identity of a change with no before span). *)
type level_form = Lv_rewrite | Lv_deletion | Lv_insertion of int

let ellipsize_level (bn : Tree.src Tree.t) (an : Tree.src Tree.t)
    (b_assign : (Tree.src Tree.child * int option) list)
    (aks : Tree.src Tree.child list) (used : bool array)
    (b_children : pat_child list) (a_children : pat_child list) :
    (pat_node * pat_node * level_form) option =
  let closer_of = function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> '\x00'
  in
  let bracket_leaf (c : Tree.src Tree.child) =
    (not c.node.is_named)
    && (String.length c.node.node_type = 1 || c.node.node_type = "/>")
  in
  let ba = Array.of_list b_assign in
  let bc = Array.of_list b_children in
  let aa = Array.of_list aks in
  let ac = Array.of_list a_children in
  let nb = Array.length ba and na = Array.length aa in
  if nb < 3 || na < 3 then None
  else
    let b0, _ = ba.(0)
    and blast, _ = ba.(nb - 1)
    and a0 = aa.(0)
    and alast = aa.(na - 1) in
    let closes opener (closer : string) =
      closer = String.make 1 (closer_of opener)
      || (opener = '<' && closer = "/>")
    in
    let shape_ok =
      bracket_leaf b0 && bracket_leaf blast && bracket_leaf a0
      && bracket_leaf alast
      && String.length b0.node.node_type = 1
      && String.contains "([{<" b0.node.node_type.[0]
      && closes b0.node.node_type.[0] blast.node.node_type
      && a0.node.node_type = b0.node.node_type
      && alast.node.node_type = blast.node.node_type
    in
    (* [<]-opened levels (JSX elements): a bare [<] re-parses as
       comparison/JSX-fragment junk, so the variant must also keep the
       element head concrete, glued to the bracket ([<Button ... >]) — the
       same re-parse argument that keeps the brackets themselves. Without a
       kept head aligned on both sides there is no viable render: emit
       nothing rather than a candidate the gate can never fire. *)
    let angle = shape_ok && b0.node.node_type = "<" in
    let head_kept =
      angle
      && Array.length ba >= 4
      && Array.length aa >= 4
      && match ba.(1) with _, Some 1 -> true | _ -> false
    in
    if (not shape_ok) || (angle && not head_kept) then None
    else begin
      let unmatched_b = ref [] in
      Array.iteri
        (fun i (_, m) ->
          if m = None && i > 0 && i < nb - 1 then
            unmatched_b := i :: !unmatched_b)
        ba;
      let unmatched_a = ref [] in
      Array.iteri
        (fun i u ->
          if (not u) && i > 0 && i < na - 1 then
            unmatched_a := i :: !unmatched_a)
        used;
      let unmatched_b = List.rev !unmatched_b in
      let unmatched_a = List.rev !unmatched_a in
      let contiguous = function
        | [] -> false
        | x :: rest ->
            let ok = ref true and prev = ref x in
            List.iter
              (fun i ->
                if i <> !prev + 1 then ok := false;
                prev := i)
              rest;
            !ok
      in
      (* One child per line: the surgical renderer aligns lines, so the
         ellipses must sit on lines of their own to become context. *)
      let mk (node : Tree.src Tree.t) open_c heads mids close_c =
        let nheads = List.length heads in
        let children =
          (open_c :: heads)
          @ ({ field_name = None; child = Ellipsis } :: mids)
          @ [ { field_name = None; child = Ellipsis }; close_c ]
        in
        let template =
          List.concat
            (List.mapi
               (fun i _ ->
                 if i = 0 then [ Slot 0 ]
                 else if i <= nheads then [ Slot i ] (* glued: [<Button] *)
                 else [ Lit "\n"; Slot i ])
               children)
        in
        PNode
          {
            node_type = node.node_type;
            is_named = node.is_named;
            children;
            template;
          }
      in
      (* Insertion variant: ONE ellipsis on each side (a [+] line between two
         [...] runs is unanchorable and the matcher rejects it), the run's
         children glued onto one line ([+] text is output text — the
         separator belongs on the element's line, [+ standalone: false,]). *)
      let mk_ins (node : Tree.src Tree.t) open_c heads ~prepend mids close_c =
        let nheads = List.length heads in
        let ell = { field_name = None; child = Ellipsis } in
        let children =
          if prepend then ((open_c :: heads) @ mids) @ [ ell; close_c ]
          else ((open_c :: heads) @ (ell :: mids)) @ [ close_c ]
        in
        let nmids = List.length mids in
        let mid_first = if prepend then 1 + nheads else 2 + nheads in
        let template =
          List.concat
            (List.mapi
               (fun i _ ->
                 if i = 0 then [ Slot 0 ]
                 else if i <= nheads then [ Slot i ] (* glued: [<Button] *)
                 else if i > mid_first && i < mid_first + nmids then [ Slot i ]
                   (* the run glues onto its first child's line *)
                 else [ Lit "\n"; Slot i ])
               children)
        in
        PNode
          {
            node_type = node.node_type;
            is_named = node.is_named;
            children;
            template;
          }
      in
      let b_heads = if head_kept then [ bc.(1) ] else [] in
      let a_heads = if head_kept then [ ac.(1) ] else [] in
      match (unmatched_b, unmatched_a) with
      | [ bi ], [ ai ] when bi > List.length b_heads && ai > List.length a_heads
        ->
          Some
            ( mk bn bc.(0) b_heads [ bc.(bi) ] bc.(nb - 1),
              mk an ac.(0) a_heads [ ac.(ai) ] ac.(na - 1),
              Lv_rewrite )
      | (_ :: _ as bis), []
        when contiguous bis
             && List.exists (fun i -> (fst ba.(i)).node.is_named) bis ->
          (* Deletion of a contiguous run — an element plus the adjacent
             separator tree-diff leaves unmatched. The run stays concrete on
             [-] lines (so the separator is deleted explicitly, no cleanup
             magic), the after side is the same list with only the ellipses:
             [{ ... - DestroyRef - , ... }]. Pure-separator runs are junk,
             not a deletion — require a named child. *)
          Some
            ( mk bn bc.(0) b_heads (List.map (fun i -> bc.(i)) bis) bc.(nb - 1),
              mk an ac.(0) a_heads [] ac.(na - 1),
              Lv_deletion )
      | [], (_ :: _ as ais) when List.exists (fun i -> aa.(i).node.is_named) ais
        -> (
          (* Insertion of a run, edge positions only. The child matcher
             pairs equal separators greedily, so an inserted [x ,] run can
             surface as {content at the edge, freed separator at the far
             end}. What the list actually gained is the contiguous edge
             span whose children — as a multiset of structural hashes —
             equal the unmatched set; rebuild the run as that span
             (separators are textually interchangeable, and the safety
             gate still requires byte-exact reproduction). The before-side
             anchor byte — end of the opener for a prepend, end of the
             last child before the closer for an append — is the change's
             level-independent identity: the same insertion seen from
             every Modified ancestor pools on it. *)
          let n = List.length ais in
          let hashes idxs =
            List.sort compare (List.map (fun i -> aa.(i).node.Tree.hash) idxs)
          in
          let u = hashes ais in
          let span_from lo = List.init n (fun k -> lo + k) in
          let pre_lo = 1 + List.length a_heads in
          let app_lo = na - 1 - n in
          let pick =
            if pre_lo + n - 1 <= na - 2 && hashes (span_from pre_lo) = u then
              Some (span_from pre_lo, true)
            else if app_lo >= pre_lo && hashes (span_from app_lo) = u then
              Some (span_from app_lo, false)
            else None
          in
          match pick with
          | None -> None
          | Some (ais, prepend) ->
              let anchor =
                if prepend then (fst ba.(0)).node.end_byte
                else (fst ba.(nb - 2)).node.end_byte
              in
              let mids = List.map (fun i -> ac.(i)) ais in
              Some
                ( mk_ins bn bc.(0) b_heads ~prepend [] bc.(nb - 1),
                  mk_ins an ac.(0) a_heads ~prepend mids ac.(na - 1),
                  Lv_insertion anchor ))
      | _ -> None
    end

(** Deep (chain-recursive) delta-keyed variant. [delta_keyed_pair] holes a
    pair's preserved children but keeps every changed child fully concrete —
    at the delta's own level that IS the delta, but at an ancestor level the
    changed child is a whole subtree containing per-site variation, so the
    variant is site-specific and its identity pool stays at 1. This variant
    recurses instead: when a level has exactly one changed child on each side,
    that pair is generalized by the same rules, all the way down to the level
    where the change stops being a single-child chain (the delta, kept
    concrete). The result is identical across sites that share the delta and
    the chain's structure, so pools form at {e every} context level — the
    gate then decides which level's render actually fires (§3.2's re-parse
    mismatch kills fragment levels; an anchored statement-ish level
    survives). Emitted for every change pair the multi-level extraction
    visits, so each suffix of the context chain is a candidate.

    Per-level generalization of the preserved siblings, by container shape:
    - {b bracket-delimited levels} take the {!ellipsize_level} form —
      [{ ... <delta> ... }] — so the list's arity is NOT baked into the
      pattern. Holing each preserved sibling instead pools only same-arity
      sites, and such an arity-baked variant can then hijack a family from
      its arity-general rule on application-order specificity, dropping the
      off-arity sites to residuals (observed: a decorator-property insertion
      whose one-prior-property variant stole the spec-file sites). When the
      ellipsis form declines (mid-list insertion, degenerate shapes), the
      preserved siblings stay {e concrete} — the level still carries the
      recursed generalization below, but pools only among sites that agree
      on the siblings, rather than generalizing along the wrong axis.
    - {b fixed-shape levels} (declarations, annotations — arity set by the
      grammar) hole each preserved named child, delta-keyed's own rule;
      anonymous tokens stay concrete.

    No hole-fraction cut: recursion accumulates holes by design (each
    context level trades anchors for pooling), and the pool is gated like
    any cluster — the delta-channel precedent that meaning is decided by
    evaluation. The other structural checks (concrete match side, closed
    after-holes, no junk pass-through) still apply.

    Returns [None] when nothing was generalized anywhere (the variant would
    equal the concrete pair). *)
(** Returns [(holed, filled)]: [holed] is the pooling key and the emitted
    candidate; [filled] is the same skeleton with each hole replaced by the
    instance's own concrete subpattern — carried as the instance [ipat] so a
    pool's re-specialized twin ([with_twins]'s anti-unification fold over
    ipats) shares the skeleton and collapses exactly the holes the pool's
    instances never vary on (preference-matrix row 6: unwitnessed holes lose
    to the witnessed literal). [Ellipsis] positions stay [Ellipsis] in both —
    they absorb arity variation the fold must not re-expand. *)
let delta_keyed_deep (cp : Tree_diff.change_pair) :
    (edit_pat * edit_pat) option =
  let pnode_shaped source (n : Tree.src Tree.t) =
    n.children <> []
    && (not (has_silent_concrete_delimiters ~source ~node:n))
    && not (has_quote_delim_children ~source ~node:n)
  in
  let next_hole = ref 0 in
  let fills : (int, pat_node * pat_node) Hashtbl.t = Hashtbl.create 8 in
  let generalized = ref false in
  (* Insertion chains never generalize their context: a pure insertion has
     no match-side delta — its before side is ALL context, so firing is
     shape-triggered, and holing the shape's anchors (a decorator name, a
     head) makes the rule fire on every same-shaped construct. Observed:
     the deep [@_H0( ... + standalone: false ... )] rule was gate-clean on
     a corpus whose decorators all take the flag, then regressed the
     spartacus holdout 61→43 exact by inserting into @NgModule/@Injectable.
     Rewrites and deletions carry their delta as an anchor and may hole
     freely; insertions keep preserved named children concrete (matching
     the insert-anchoring channel's deliberate concrete heads). *)
  let saw_insertion = ref false in
  let keep (n : Tree.src Tree.t) = not n.is_extra in
  let rec go depth (b : Tree.src Tree.t) (a : Tree.src Tree.t) :
      pat_node * pat_node =
    let concrete () = (of_src cp.before_source b, of_src cp.after_source a) in
    if
      depth > 8
      || (not (pnode_shaped cp.before_source b))
      || (not (pnode_shaped cp.after_source a))
      || b.node_type <> a.node_type
    then concrete ()
    else begin
      let kept (n : Tree.src Tree.t) =
        List.filter
          (fun (c : Tree.src Tree.child) -> not c.node.is_extra)
          n.children
      in
      let bks = kept b and aks = kept a in
      let aks_arr = Array.of_list aks in
      let used = Array.make (Array.length aks_arr) false in
      (* Greedy in-order hash matching, as in [delta_keyed_pair]. *)
      let b_assign =
        List.map
          (fun (c : Tree.src Tree.child) ->
            let m = ref None in
            Array.iteri
              (fun i (ac : Tree.src Tree.child) ->
                if !m = None && (not used.(i)) && ac.node.hash = c.node.hash
                then begin
                  used.(i) <- true;
                  m := Some i
                end)
              aks_arr;
            match !m with
            | Some i when c.node.is_named -> (c, `Pres i)
            | Some i -> (c, `Anon i)
            | None -> (c, `Delta))
          bks
      in
      let delta_b =
        List.filter (fun (_, m) -> m = `Delta) b_assign |> List.map fst
      in
      let delta_a = List.filteri (fun i _ -> not used.(i)) aks in
      if delta_b = [] && delta_a <> [] then saw_insertion := true;
      (* Recurse only through a one-child-per-side chain: with several changed
         children the level is where deltas fuse, and each stays concrete
         (delta_keyed_pair parity). *)
      let recursed =
        match (delta_b, delta_a) with
        | [ db ], [ da ] -> Some (db, go (depth + 1) db.node da.node)
        | _ -> None
      in
      let before_child ((c : Tree.src Tree.child), m) hole_of =
        match (m, recursed) with
        | `Pres _, _ -> hole_of c m
        | `Delta, Some (db, (rb, _)) when db == c -> rb
        | _ -> of_src cp.before_source c.node
      in
      let after_child i (c : Tree.src Tree.child) hole_of =
        if used.(i) then hole_of i
        else
          match recursed with
          | Some (_, (_, ra)) -> ra
          | None -> of_src cp.after_source c.node
      in
      (* Bracket-shaped level? Try the arity-free ellipsis form first. Only
         the opener/closer, a potential kept head, and the delta children are
         ever read from the pattern-child arrays, so preserved siblings get a
         placeholder rather than paying [of_src] on (possibly large) subtrees
         they'd never render. *)
      let bracketish =
        match (bks, aks) with
        | b0 :: _ :: _, a0 :: _ :: _ ->
            (not b0.node.is_named)
            && (not a0.node.is_named)
            && String.length b0.node.node_type = 1
            && String.contains "([{<" b0.node.node_type.[0]
        | _ -> false
      in
      let ellipsized =
        if not bracketish then None
        else begin
          let placeholder = Hole 0 in
          let pat_children source recursed_pat delta_test children real_idx =
            List.mapi
              (fun i (c : Tree.src Tree.child) ->
                let child =
                  if delta_test i c then
                    match recursed_pat with
                    | Some p -> p
                    | None -> of_src source c.node
                  else if i = 0 || i = 1 || i = List.length children - 1 then
                    of_src source c.node
                  else if real_idx i c then of_src source c.node
                  else placeholder
                in
                { field_name = c.field_name; child })
              children
          in
          let b_delta_idx =
            List.mapi
              (fun i (_, m) -> if m = `Delta then Some i else None)
              b_assign
            |> List.filter_map Fun.id
          in
          let bc =
            pat_children cp.before_source
              (match recursed with Some (_, (rb, _)) -> Some rb | None -> None)
              (fun i _ -> List.mem i b_delta_idx)
              bks
              (fun i _ -> List.mem i b_delta_idx)
          in
          let ac =
            pat_children cp.after_source
              (match recursed with Some (_, (_, ra)) -> Some ra | None -> None)
              (fun i _ -> not used.(i))
              aks
              (fun i _ -> not used.(i))
          in
          let b_pairs =
            List.map
              (fun (c, m) ->
                ( c,
                  match m with
                  | `Pres i | `Anon i -> Some i
                  | `Delta -> None ))
              b_assign
          in
          match ellipsize_level b a b_pairs aks used bc ac with
          | Some (pb, pa, _form) ->
              generalized := true;
              Some (pb, pa)
          | None -> None
        end
      in
      match ellipsized with
      | Some pair -> pair
      | None ->
          (* Fixed-shape level: hole preserved named children. At a bracket
             level whose ellipsis form declined, keep them concrete instead —
             an arity-baked hole pattern generalizes along the wrong axis. *)
          let hole_of_a = Array.make (Array.length aks_arr) None in
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
                          before_child (c, m) (fun c' m' ->
                              match m' with
                              | `Pres i
                                when (not bracketish) && not !saw_insertion ->
                                  let h = !next_hole in
                                  incr next_hole;
                                  generalized := true;
                                  hole_of_a.(i) <- Some h;
                                  Hashtbl.replace fills h
                                    ( of_src cp.before_source c'.node,
                                      of_src cp.after_source
                                        aks_arr.(i).node );
                                  Hole h
                              | _ -> of_src cp.before_source c'.node);
                      })
                    b_assign;
                template =
                  build_template ~source:cp.before_source ~node:b ~keep ();
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
                          after_child i c (fun i' ->
                              match hole_of_a.(i') with
                              | Some h -> Hole h
                              | None -> of_src cp.after_source c.node);
                      })
                    aks;
                template =
                  build_template ~source:cp.after_source ~node:a ~keep ();
              }
          in
          (before, after)
    end
  in
  let before, after = go 0 cp.before_node cp.after_node in
  let ep = { before; after } in
  if
    !generalized && has_concrete ep.before && has_concrete_edit ep
    && no_orphan_after_holes ep && no_junk_passthrough ep
  then begin
    let rec fill side = function
      | Hole h -> (
          match Hashtbl.find_opt fills h with
          | Some (fb, fa) -> ( match side with `B -> fb | `A -> fa)
          | None -> Hole h)
      | Ellipsis -> Ellipsis
      | Leaf _ as l -> l
      | PNode n ->
          PNode
            {
              n with
              children =
                List.map
                  (fun c -> { c with child = fill side c.child })
                  n.children;
            }
    in
    let filled = { before = fill `B ep.before; after = fill `A ep.after } in
    Some (ep, filled)
  end
  else None

(* The 4th component marks an insertion-form variant (the chain ended in an
   ellipsis-context insertion): those are GENERAL candidates — one arity-free
   text shared by every site, the opposite of a site-local anchored
   realisation — and the caller routes them to the delta-keyed round-1
   channel instead of the anchored round-2 stream. *)

(** Anchored lattice-descent variants (§3.2): the pair's own preserved children
    stay CONCRETE — they are the anchor that discriminates a context-dependent
    change — while preserved content *inside* the changed-child chain becomes
    shared holes, recursively along the single-changed-child path. For a rename
    applied at [state = X(args)] this yields [state = X(_H0) ⤳ state = Y(_H0)]:
    the [state =] anchor literal, the site-specific args holed.

    Each variant carries a *delta key* — the changed leaves' source text on both
    sides — so that support can be pooled on the delta across sites whose
    anchors differ (design §3.2: support and min_support are counted on the
    delta cluster; the anchored variants are its site-local realisations).
    Variants with no holes are kept only when the change is leaf-level (the
    [::X ⤳ ::Y] case, where the anchor is pure structure); deeper hole-free
    variants are just the concrete base pair again. *)
let anchored_variants (cp : Tree_diff.change_pair) :
    (edit_pat * string * (int * int) * bool) list =
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
  (* Build one variant for a given selector. Mutable per-run state.
     [ellipsis_lists] additionally collapses bracket-delimited levels of the
     chain into ellipsis-context form (see {!ellipsize_level}); levels where
     that shape doesn't hold fall back to the concrete form, so the flag is a
     best-effort variant, deduped against the concrete one when it changes
     nothing. *)
  let build ~ellipsis_lists selector0 =
    next_hole := 0;
    let sel = ref selector0 in
    let b_delta = ref [] and a_delta = ref [] in
    let ins_form = ref false in
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
      (* Descend FIRST: whether the chain below ends in an insertion form
         ([ins_form]) decides how this level treats its preserved named
         children. An insertion has no before-side delta — its entire match
         power is context — so holing the single head-shaped sibling of the
         descended child (a call's callee, a decorator's name) would erase
         exactly the anchor the rule needs (and [has_concrete] then rightly
         kills the anchorless [_H0(...)] form). A level with one preserved
         named child keeps it concrete on insertion chains; levels with more
         (statement lists, declaration headers) hole as usual — keeping many
         siblings concrete would bake the site's surroundings into the
         pattern. *)
      let descended =
        match descend with
        | Some (ub, ai, ua) ->
            let bp, ap = level ~holed_preserved:true ub ua in
            inner_ap := Some ap;
            hole_of_a.(ai) <- None;
            Some (ub, bp)
        | None -> None
      in
      let preserved_named =
        List.length
          (List.filter
             (fun ((c : Tree.src Tree.child), m) ->
               m <> None && c.node.is_named)
             b_assign)
      in
      let keep_head_anchor = !ins_form && preserved_named = 1 in
      let b_children =
        List.map
          (fun ((c : Tree.src Tree.child), m) ->
            let child =
              match m with
              | Some i
                when c.node.is_named && holed_preserved && not keep_head_anchor
                ->
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
                  match descended with
                  | Some (ub, bp) when ub == c.node -> bp
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
      let concrete_level () =
        ( PNode
            {
              node_type = bn.node_type;
              is_named = bn.is_named;
              children = b_children;
              template =
                build_template ~source:cp.before_source ~node:bn ~keep ();
            },
          PNode
            {
              node_type = an.node_type;
              is_named = an.is_named;
              children = a_children;
              template =
                build_template ~source:cp.after_source ~node:an ~keep ();
            } )
      in
      if not ellipsis_lists then concrete_level ()
      else
        match ellipsize_level bn an b_assign aks used b_children a_children with
        | Some (bp, ap, form) ->
            (match form with
            | Lv_insertion anchor ->
                (* The insertion's before-side anchor byte doubles as the
                   delta span: a pure insertion has no before-side delta
                   bytes, and the anchor point is the identity shared by
                   this change's variants at every ancestor level. *)
                ins_form := true;
                d_start := min !d_start anchor;
                d_end := max !d_end anchor
            | Lv_rewrite | Lv_deletion -> ());
            (bp, ap)
        | None -> concrete_level ()
    in
    let before, after =
      level ~holed_preserved:false cp.before_node cp.after_node
    in
    let ep = { before; after } in
    let holes = edit_holes ep in
    (* A pure-insertion delta (empty before side) is un-anchorable — an
       anchored realisation would claim a one-site insertion with a
       support-1 rule, which states the change worse than its residual (the
       §5.5 pure-additions philosophy) — UNLESS the chain ended in an
       ellipsis-context insertion form, which anchors the [+] run to the
       container's delimiters/head instead of to before-side content and
       pools on the insertion point like any other delta. *)
    if !b_delta = [] && not !ins_form then None
    else if holes = 0 && (not !all_leaves) && not (contains_ellipsis ep.before)
    then None
    else if
      has_concrete ep.before && has_concrete_edit ep && no_orphan_after_holes ep
      && no_junk_passthrough ep
      && hole_frac ep < Cs_config.default.max_hole_fraction
    then
      let key =
        String.concat "\x00" (List.rev !b_delta)
        ^ "\x01"
        ^ String.concat "\x00" (List.rev !a_delta)
      in
      Some (ep, key, (!d_start, !d_end), !ins_form)
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
    let sels =
      List.filteri
        (fun i _ -> i < Cs_config.default.max_selectors_per_pair)
        sels
    in
    let seen = Hashtbl.create 8 in
    List.concat_map
      (fun s ->
        List.filter_map
          (fun ellipsis_lists ->
            match build ~ellipsis_lists s with
            | Some ((ep, _, _, _) as v) ->
                if Hashtbl.mem seen ep then None
                else begin
                  Hashtbl.add seen ep ();
                  Some v
                end
            | None -> None)
          [ false; true ])
      sels
  end

(* Returns (base clusters, delta-keyed clusters, deep chain variants,
   anchored variants). The base clusters feed the dendrogram as before; the
   other three streams deliberately stay OUT of it — adding them as
   dendrogram inputs changes its merge geometry for everyone (observed:
   displaced extraction and call-level rules on the golden cases). The
   delta-keyed and deep variants pool by exact pattern identity; the
   anchored variants pool by DELTA key, so a context-dependent change whose
   anchors differ per site still accumulates delta-level support. The deep
   stream is kept separate from the plain delta stream because it is
   round-2 material: an anchored context-chain form competes with the
   nice ellipsis-context rules only for regions round 1 leaves uncovered
   (see the selection notes in {!Cs_select}). *)
let collect_initial_clusters ?on_file ~ctx (cs : changeset) :
    cluster list
    * cluster list
    * cluster list
    * (string * (int * int) * cluster) list =
  let modified =
    List.filter (function Modified _ -> true | _ -> false) cs.files
  in
  let total = List.length modified in
  let initial = ref [] in
  let delta = ref [] in
  let deep = ref [] in
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
                let dep = delta_keyed_pair cp in
                (match dep with
                | Some dep ->
                    delta :=
                      {
                        pattern = dep;
                        instances = [ { inst with ipat = dep } ];
                      }
                      :: !delta
                | None -> ());
                (* Deep (chain-recursive) variant: pools at ancestor
                   context levels too. Instances carry the HOLE-FILLED
                   skeleton as [ipat] (same shape as the pooling key,
                   holes replaced by this site's concrete subpatterns),
                   so the pool's re-specialized twin collapses exactly
                   the holes the pool never varies on while keeping the
                   skeleton aligned. The holed pattern is the pooling
                   key and the fallback where sites do vary. Skipped
                   when equal to the plain variant (same site would
                   pool twice). *)
                (match delta_keyed_deep cp with
                | Some (ddep, dfill) when Some ddep <> dep ->
                    deep :=
                      { pattern = ddep; instances = [ { inst with ipat = dfill } ] }
                      :: !deep
                | _ -> ());
                (* §3.2 anchored variants: preserved siblings literal,
                   changed-chain interior holed, keyed by the delta —
                   one per path choice at branching levels. Insertion-form
                   variants are one arity-free text shared by every site,
                   so they join the delta-keyed round-1 channel (pooled by
                   exact identity) rather than the anchored round-2
                   stream of site-local realisations. *)
                match anchored_variants cp with
                | vs ->
                    List.iter
                      (fun (aep, key, span, insertion_general) ->
                        if insertion_general then
                          delta :=
                            {
                              pattern = aep;
                              instances = [ { inst with ipat = aep } ];
                            }
                            :: !delta
                        else
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
              Cs_trace.trace "collect_initial_clusters: skipping %s: %s\n%!"
                path (Printexc.to_string e))
      | Added _ | Deleted _ -> ())
    modified;
  (!initial, !delta, !deep, !anchored)

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
