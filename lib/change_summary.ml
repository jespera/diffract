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
      template : template_part list;
          (** Inter-child source text and child placeholders, used by the
              renderer to reconstruct the node's surface syntax. Captures
              source bytes (e.g. the quote delimiters of a string literal)
              that the grammar consumes silently — i.e. that fall inside
              the node's byte range but aren't exposed as child nodes. *)
    }

and pat_child = { field_name : string option; child : pat_node }
and template_part = Lit of string | Slot of int

type edit_pat = { before : pat_node; after : pat_node }

type instance = {
  before_text : string;
  after_text : string;
  before_full_source : string;
      (** Full pre-change source of the file containing this site, used
          by the applicability check to verify the rendered pattern
          actually matches in real code. *)
  file : string;
  line : int;
  language : string;
  site_start : int;
  site_end : int;
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

(** Build the inter-child template for a tree-sitter node. Walks the
    node's byte range from [start_byte] to [end_byte], emitting [Lit]
    parts for source bytes outside any kept child and [Slot j] for the
    [j]-th kept child (0-based among kept children). Children for which
    [keep c = false] have their byte range absorbed into the surrounding
    [Lit] so the rendered output preserves the original surface text.
    Captures source bytes that the grammar consumes silently (e.g.
    string-literal quote delimiters in Kotlin/TS, or leading/trailing
    whitespace inside a parenthesised list). *)
let build_template ~source ~(node : Tree.src Tree.t)
    ?(keep = fun (_ : Tree.src Tree.t) -> true) () : template_part list =
  let parts = ref [] in
  let push p = parts := p :: !parts in
  let cursor = ref node.start_byte in
  let emit_lit_upto upto =
    if upto > !cursor then
      push (Lit (String.sub source !cursor (upto - !cursor)));
    cursor := upto
  in
  let kept_idx = ref 0 in
  List.iter
    (fun (c : _ Tree.child) ->
      if keep c.node then begin
        emit_lit_upto c.node.start_byte;
        push (Slot !kept_idx);
        incr kept_idx;
        cursor := c.node.end_byte
      end
      (* else: dropped child — leave cursor alone so its bytes are
         absorbed into the next Lit emit. *))
    node.children;
  emit_lit_upto node.end_byte;
  List.rev !parts

let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(** True if any byte in the node's range that is not covered by a child
    is non-whitespace. These are "silently-consumed delimiters" — bytes
    the grammar embeds in the parent node's text but does not expose as
    children (e.g. the surrounding quotes of a string literal in
    Kotlin/TS, the slashes of a regex literal). Treat such nodes as
    leaves during [of_src]: anti-unification then holes the whole node
    when its content varies, rather than holing inside the delimiters
    and rendering a placeholder embedded inside a string literal — which
    the pattern parser would misread as the literal characters of the
    delimited token rather than as a metavariable. *)
let has_silent_concrete_delimiters ~source ~(node : Tree.src Tree.t) : bool =
  let found = ref false in
  let cursor = ref node.start_byte in
  let scan_gap upto =
    for i = !cursor to upto - 1 do
      if not (is_ws source.[i]) then found := true
    done;
    cursor := upto
  in
  List.iter
    (fun (c : _ Tree.child) ->
      scan_gap c.node.start_byte;
      cursor := c.node.end_byte)
    node.children;
  scan_gap node.end_byte;
  !found

(** Some grammars (TypeScript, JavaScript) expose string-literal quotes as
    unnamed child nodes rather than consuming them silently. These nodes
    have full byte-coverage by children (so [has_silent_concrete_delimiters]
    misses them) but still wrap their content in tokens that would
    sandwich any rendered hole into a string-literal token. Treat as a
    leaf when an unnamed child has text equal to a string-quote character.
    Limited to double-quote, single-quote, and backtick; the slash is
    excluded because it is also the division operator and would
    misclassify binary expressions. *)
let has_quote_delim_children ~source ~(node : Tree.src Tree.t) : bool =
  List.exists
    (fun (c : _ Tree.child) ->
      if c.node.is_named then false
      else
        match Tree.text source c.node with
        | "\"" | "'" | "`" -> true
        | _ -> false)
    node.children

let rec of_src source (n : Tree.src Tree.t) : pat_node =
  if n.children = [] && n.is_named then
    Leaf { node_type = n.node_type; value = Tree.text source n }
  else if
    n.is_named
    && (has_silent_concrete_delimiters ~source ~node:n
       || has_quote_delim_children ~source ~node:n)
  then Leaf { node_type = n.node_type; value = Tree.text source n }
  else
    (* Drop tree-sitter "extras" (typically comments) from the AST so
       presence/absence of comments doesn't fragment clusters. The
       filtered child's source bytes are still preserved in the
       template's [Lit] segments, so rendering reproduces the original
       surface text. *)
    let keep (c : Tree.src Tree.t) = not c.is_extra in
    PNode
      {
        node_type = n.node_type;
        is_named = n.is_named;
        children =
          List.filter_map
            (fun (c : Tree.src Tree.child) ->
              if keep c.node then
                Some { field_name = c.field_name; child = of_src source c.node }
              else None)
            n.children;
        template = build_template ~source ~node:n ~keep ();
      }

(* ── Rendering ───────────────────────────────────────────────────── *)

let rec render_pat_node = function
  | Hole h -> Printf.sprintf "$H%d" h
  | Leaf { value; _ } -> value
  | PNode { children = []; node_type; _ } -> node_type
  | PNode { children; template; _ } ->
      let arr = Array.of_list children in
      let buf = Buffer.create 32 in
      List.iter
        (function
          | Lit s -> Buffer.add_string buf s
          | Slot i -> Buffer.add_string buf (render_pat_node arr.(i).child))
        template;
      Buffer.contents buf

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

let hole_frac ep =
  let s = edit_size ep in
  if s = 0 then 0.0 else float_of_int (edit_holes ep) /. float_of_int s

(** Score a candidate merge for the dendrogram. Pure hole-fraction can
    pick a merge whose anti-unification holes both sides at unrelated
    positions — e.g. merging [tokenCache.read → tokenCache.get] with
    [tokenCache.write → tokenCache.set] holes the property on each
    side independently, producing a [+]-side hole with no [-]-side
    binding source (orphan). Such patterns are rejected by the
    coherence gate, dropping the merge to singletons. Penalise these
    merges so the greedy step prefers a sibling pairing that keeps
    holes aligned (e.g. merging different receivers with the same
    property: [tokenCache.read] + [rateCache.read] → [$H0.read]). *)
let merge_score ep =
  let base = hole_frac ep in
  let before_holes = collect_holes [] ep.before in
  let after_holes = collect_holes [] ep.after in
  let aligned =
    List.for_all (fun h -> List.mem h before_holes) after_holes
  in
  if aligned then base else base +. 10.0

(** Cheap signature for a pattern's root pair, used to short-circuit
    [build_dendrogram]'s inner loop. Anti-unifying two patterns whose
    [before] (or [after]) roots differ in kind or node_type produces a
    hole-rooted pattern that the downstream coherence filter
    ([cluster_applies]) will reject. Comparing this signature is a
    string equality vs. a full tree anti-unification. *)
let root_sig (ep : edit_pat) =
  let tag = function
    | Hole _ -> ("H", "")
    | Leaf { node_type; _ } -> ("L", node_type)
    | PNode { node_type; _ } -> ("P", node_type)
  in
  (tag ep.before, tag ep.after)

let build_dendrogram initial =
  let trace = Sys.getenv_opt "CS_TRACE" <> None in
  let initial_n = List.length initial in
  let nodes =
    ref (List.map (fun c -> DLeaf (c.instances, c.pattern)) initial)
  in
  let t_start = Unix.gettimeofday () in
  let last_tick = ref t_start in
  let iter_no = ref 0 in
  let cur_n = ref initial_n in
  let cur_antiunifies = ref 0 in
  let heartbeat_interval = 2.0 in
  let heartbeat () =
    if trace then begin
      let now = Unix.gettimeofday () in
      if now -. !last_tick >= heartbeat_interval then begin
        Printf.eprintf
          "  dendrogram: iter %d/%d, n=%d, %d anti-unifies in iter, elapsed %.1fs\n%!"
          !iter_no (initial_n - 1) !cur_n !cur_antiunifies
          (now -. t_start);
        last_tick := now
      end
    end
  in
  if trace then
    Printf.eprintf
      "  dendrogram: starting hierarchical merge over %d clusters\n%!"
      initial_n;
  while List.length !nodes > 1 do
    let arr = Array.of_list !nodes in
    let n = Array.length arr in
    let sigs = Array.map (fun nd -> root_sig (dnode_pattern nd)) arr in
    incr iter_no;
    cur_n := n;
    cur_antiunifies := 0;
    let merge_patterns pi pj =
      let offset = max_hole pi + 1 in
      anti_unify_edits pi (shift_holes offset pj)
    in
    let bi = ref 0 and bj = ref 1 in
    let bp =
      ref (merge_patterns (dnode_pattern arr.(0)) (dnode_pattern arr.(1)))
    in
    let bfrac = ref (merge_score !bp) in
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        if not (i = 0 && j = 1) && sigs.(i) = sigs.(j) then begin
          incr cur_antiunifies;
          if !cur_antiunifies mod 1000 = 0 then heartbeat ();
          let p =
            merge_patterns (dnode_pattern arr.(i)) (dnode_pattern arr.(j))
          in
          let frac = merge_score p in
          if frac < !bfrac then begin
            bi := i;
            bj := j;
            bp := p;
            bfrac := frac
          end
        end
      done
    done;
    heartbeat ();
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
  if trace then
    Printf.eprintf "  dendrogram: done in %.2fs after %d iterations\n%!"
      (Unix.gettimeofday () -. t_start) !iter_no;
  List.hd !nodes

(** A pattern is "concrete" iff it contains at least one named [Leaf]
    or a keyword-shaped unnamed token (one whose text contains an
    alphabetic character — [array], [function], [class] etc.). Empty
    [PNode]s whose node_type is pure punctuation ([,], [(], [;])
    don't count: the pattern is structural scaffolding without a
    keyword anchor and matches arbitrary content of the right shape.
    The keyword carve-out is what lets a PHP rule like [array($H0)
    -> [$H0]] survive coherence: the [array] keyword is the
    distinguishing concrete signal even though every named-leaf
    descendant becomes a hole. *)
let has_keyword_text s =
  let n = String.length s in
  let rec loop i =
    if i >= n then false
    else
      let c = s.[i] in
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then true
      else loop (i + 1)
  in
  loop 0

let rec has_concrete = function
  | Hole _ -> false
  | Leaf _ -> true
  | PNode { children = []; node_type; _ } -> has_keyword_text node_type
  | PNode { children; _ } -> List.exists (fun c -> has_concrete c.child) children

let rec collect_leaf_values acc = function
  | Hole _ -> acc
  | Leaf { value; _ } -> value :: acc
  | PNode { children; _ } ->
      List.fold_left (fun a c -> collect_leaf_values a c.child) acc children

(** [same_shape_mod_holes p1 p2] holds iff [p1] and [p2] have identical
    structural shape — same node types at every position and the same
    number of children at every [PNode]. Leaf {e values} are not compared:
    a pure rename [foo -> bar] is same-shape, but a call with different
    argument counts ([foo(a, b)] vs [bar(a)]) is not.

    Used to distinguish symmetric patterns (pure renames, one-for-one
    substitutions) from asymmetric ones (structural reshapes like dropped
    arguments). Asymmetric patterns carry structural-edit information that
    leaf-level patterns alone can't express. *)
let rec same_shape_mod_holes p1 p2 =
  match (p1, p2) with
  | Hole _, Hole _ -> true
  | Leaf l1, Leaf l2 -> l1.node_type = l2.node_type
  | PNode n1, PNode n2 ->
      n1.node_type = n2.node_type
      && n1.is_named = n2.is_named
      && List.length n1.children = List.length n2.children
      && List.for_all2
           (fun c1 c2 -> same_shape_mod_holes c1.child c2.child)
           n1.children n2.children
  | _ -> false

let is_symmetric (ep : edit_pat) : bool =
  same_shape_mod_holes ep.before ep.after

(** True if the edit pattern carries at least one concrete edit signal —
    either (a) the multiset of named-leaf values differs between [-] and
    [+] sides (a concrete rename), or (b) the shape differs modulo holes
    (a structural edit such as a dropped argument). A pattern whose only
    differences are in which hole-variable index appears where is
    substance-free: it's a generic "something in this context changed"
    matcher that fires at every Modified ancestor and drowns more
    specific rules. Rejected at coherence time. *)
let has_concrete_edit (ep : edit_pat) : bool =
  let b = List.sort compare (collect_leaf_values [] ep.before) in
  let a = List.sort compare (collect_leaf_values [] ep.after) in
  b <> a || not (same_shape_mod_holes ep.before ep.after)

(** No replace-side hole appears that lacks a binding source on the
    match side. A pattern with a [+]-side metavariable not present on
    the [-] side is rejected by the spatch engine at apply time
    ("Metavars in replacement not bound in match"), so emitting it
    would produce an unapplicable rule. This usually means the
    cluster's anti-unification dropped some context that carried the
    binding source — fall back to the coherent dendrogram parent
    instead. See design doc §4.3. *)
let no_orphan_after_holes (ep : edit_pat) : bool =
  let before_holes = collect_holes [] ep.before in
  let after_holes = collect_holes [] ep.after in
  List.for_all (fun h -> List.mem h before_holes) after_holes

(** Behavioural applicability check: the rule, when its match side is
    parsed as a [.pat] pattern and applied to the source bytes the
    cluster was extracted from, must find at least one match. The
    renderer can drop syntactic context that the grammar uses to
    decide a node's type — most commonly, a [property_identifier]
    (the property name in a member access) renders as just its text
    and re-parses as a bare [identifier] at expression position. Such
    a pattern can never fire: the matcher only unifies nodes whose
    types match. Verifying directly via [Match.find_matches] catches
    these mismatches without having to model the matcher's
    type-strict behaviour with a static heuristic.

    Reject the cluster so the covering pass falls back to a
    sibling-level cluster (typically the surrounding member or call
    expression) that retains the disambiguating context. *)
let cluster_applies ~ctx (c : cluster) : bool =
  match c.instances with
  | [] -> false
  | inst :: _ ->
      let lang = inst.language in
      if lang = "" then true
      else
        let pattern_text = render_pattern_body c.pattern in
        try
          let matches =
            Match.find_matches ~ctx ~language:lang ~pattern_text
              ~source_text:inst.before_full_source
          in
          List.length matches > 0
        with _ -> false

let cut_dendrogram ?(threshold = 0.35) min_size root =
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
    has_concrete ep.before
    && has_concrete_edit ep
    && no_orphan_after_holes ep
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
      | DLeaf (_, ep) when is_coherent ep ->
          clusters := { pattern = ep; instances = insts } :: !clusters
      | DLeaf _ -> singletons := insts @ !singletons
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

(* ── Site covering ──────────────────────────────────────────────── *)

(** Count of non-[Hole] nodes in an edit pattern. Used as a tiebreak when
    multiple clusters cover the same sites: prefer the pattern that carries
    more concrete context. *)
let rec concrete_count = function
  | Hole _ -> 0
  | Leaf _ -> 1
  | PNode { children; _ } ->
      1 + List.fold_left (fun a c -> a + concrete_count c.child) 0 children

let edit_concrete_count ep = concrete_count ep.before + concrete_count ep.after

(** Count the multiset symmetric difference between the leaf values on
    each side of the edit pattern. Each leaf-value rename contributes
    2 (one removal, one addition); a structural-only edit (arg drop)
    contributes 0 from this metric. Used to prefer clusters that
    capture more concrete renames at a site — e.g. a member-expression
    rule [legacyStore.fetch -> store.get] (count 4) is preferred over
    a leaf rule [legacyStore -> store] (count 2) at the same site,
    since the larger rule reproduces every concrete change there
    while the leaf silently leaves the sibling [.fetch] rename
    uncovered. *)
let leaf_value_diff_count (ep : edit_pat) : int =
  let b = List.sort compare (collect_leaf_values [] ep.before) in
  let a = List.sort compare (collect_leaf_values [] ep.after) in
  let rec diff l1 l2 acc =
    match (l1, l2) with
    | [], rest | rest, [] -> acc + List.length rest
    | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then diff t1 t2 acc
        else if h1 < h2 then diff t1 l2 (acc + 1)
        else diff l1 t2 (acc + 1)
  in
  diff b a 0

(** Greedy site-covering pass. Rank clusters by support (desc), then
    asymmetric-shape-first (a pattern whose [-] and [+] sides differ
    structurally — e.g. dropped argument — beats a pure rename; this
    carries strictly more information), then concrete node count (asc,
    smaller is better), then hole fraction (asc), then pattern text (asc)
    as a total order. Walk in that order; for each cluster, drop instances
    whose byte range {e overlaps} a previously-claimed range within the
    same file. Emit clusters whose surviving support is still at least
    [min_support].

    The asymmetric-first preference captures the intuition from the design
    doc §4.1: a rule that describes a structural reshaping (arg drop,
    import swap with different argument counts) is strictly more
    informative than the same-shape rename it encloses, so it wins even
    though it's larger. Among patterns with matching symmetry, smaller is
    better — a leaf rename is preferred over its enclosing call-level
    cluster when both have the same support and no structural difference
    at the call level. This reflects the fact that the leaf pattern is
    equally applicable but more reusable.

    {e Overlap}, not containment. A higher-level ancestor's byte range
    contains its descendant's range, not the other way around. If leaves
    claim first, subsequent ancestor instances overlap the leaf claims
    and get dropped. Conversely if the call-level rule wins first, it
    claims a larger range that swallows any leaf instances. Two
    disjoint-range leaves at the same call site (both the receiver and
    method of a method call rename independently) do not overlap each
    other, so both leaves survive — this preserves emitting two
    independent rules for [api_swap]-style changes. *)
let cover_sites ?(min_support = 2) (clusters : cluster list) : cluster list =
  let rank (c : cluster) =
    let support = List.length c.instances in
    let sym = if is_symmetric c.pattern then 1 else 0 in
    (* Negate so higher count wins when sorted ascending. *)
    let edits = -(leaf_value_diff_count c.pattern) in
    let concrete = edit_concrete_count c.pattern in
    let hf = hole_frac c.pattern in
    let ptext = render_pattern_body c.pattern in
    (-support, sym, edits, concrete, hf, ptext)
  in
  let sorted = List.sort (fun a b -> compare (rank a) (rank b)) clusters in
  let claimed : (string, (int * int) list) Hashtbl.t = Hashtbl.create 16 in
  let is_claimed file s e =
    match Hashtbl.find_opt claimed file with
    | None -> false
    | Some ranges -> List.exists (fun (cs, ce) -> cs < e && s < ce) ranges
  in
  let claim file s e =
    let cur = try Hashtbl.find claimed file with Not_found -> [] in
    Hashtbl.replace claimed file ((s, e) :: cur)
  in
  let out = ref [] in
  List.iter
    (fun c ->
      let surviving =
        List.filter
          (fun inst ->
            not (is_claimed inst.file inst.site_start inst.site_end))
          c.instances
      in
      if List.length surviving >= min_support then begin
        List.iter
          (fun inst -> claim inst.file inst.site_start inst.site_end)
          surviving;
        out := { c with instances = surviving } :: !out
      end)
    sorted;
  List.rev !out

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

(** Fraction of a node's direct children whose change is non-[Same].
    Higher ratio means the change converges at this level; lower means
    this node is mostly unchanged boilerplate around a deeper change. *)
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
    (function
      | Tree_diff.Added _ | Tree_diff.Removed _ -> true | _ -> false)
    cc

let rec subtree_has_structural (cc : Tree_diff.child_change list) =
  List.exists
    (function
      | Tree_diff.Added _ | Tree_diff.Removed _ -> true
      | Tree_diff.Changed { change = Modified { child_changes }; _ } ->
          subtree_has_structural child_changes
      | _ -> false)
    cc

(** Emit change pairs at emission points selected by the structure of the
    diff, not by depth:

    - Every [Replaced] leaf — the natural unit for a pure rename.
    - Every [Modified] ancestor that either (a) has an [Added]/[Removed]
      child directly (the locus of a structural change), or (b) has
      [Added]/[Removed] somewhere in its subtree {e and} enough of its
      own direct children are non-[Same] to make it a plausible lift
      target (ratio ≥ [emission_threshold]).

    Case (b) lifts through arbitrarily long wrapper chains — grammar-
    agnostic, no depth bound — because at each wrapper level both
    conditions (structural descendant, ratio ≥ threshold) still hold.
    The lift stops at the first ancestor where most children are [Same],
    which is typically the enclosing function/block. The covering pass
    then ranks the candidate levels and picks the tightest informative
    one.

    Cases without any [Added]/[Removed] anywhere emit {e only} at
    [Replaced] leaves — no ancestor candidates, so no churn from
    cluster-level decisions in what should already be leaf-level
    renames. Byte-range deduplication prevents duplicate ancestor
    emissions when multiple descendant chains meet at the same ancestor. *)
let collect_change_pairs_multi ?(emission_threshold = 0.5)
    (d : Tree_diff.diff) : Tree_diff.change_pair list =
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

let collect_initial_clusters ?on_file ~ctx (cs : changeset) : cluster list =
  let modified =
    List.filter (function Modified _ -> true | _ -> false) cs.files
  in
  let total = List.length modified in
  let initial = ref [] in
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
              (collect_change_pairs_multi d)
          with _ -> ())
      | Added _ | Deleted _ -> ())
    modified;
  !initial

(** Collect one-sided candidates (M1.5) across a changeset's [Modified] files.
    Each candidate carries its pat_node shape and site metadata. Used
    internally by M1.6 fusion; not wired into M1 rule output. *)
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
                  }
                in
                out :=
                  { os_pat = of_src source node; os_instance = inst } :: !out)
              (lookahead_one_sided d)
          with _ -> ())
      | Added _ | Deleted _ -> ())
    modified;
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

(* ── M1.6 cases 2 & 3: conjunctive multi-section fusion ──────────── *)

(** A node feeding the fusion graph: a two-sided edit pattern plus the
    file set it fires in. Two-sided clusters and one-sided swap pairs
    feed in identically — once a swap pair has been widened to two-sided
    by [fuse_swap], the downstream fusion treats it the same as any
    other two-sided cluster. Cases 2 (one-sided + two-sided) and 3
    (two-sided + two-sided) of §4.2 differ only in input shape; the
    output mechanics are uniform. *)
type fusion_node = {
  fn_pattern : edit_pat;
  fn_files : string list;  (** sorted unique *)
  fn_support : int;
      (** support to report when this node is emitted standalone — total
          fire count for two-sided clusters, distinct-file count for
          swap pairs (matches the prior single-rule conventions). *)
  fn_language : string;
}

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
  let language =
    match c.instances with i :: _ -> i.language | [] -> ""
  in
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
  {
    fn_pattern = ep;
    fn_files = files;
    fn_support = List.length files;
    fn_language = language;
  }

(** Group fusion nodes into connected components by Jaccard ≥ threshold
    over their file sets. Union-find: an edge [i—j] exists iff
    [J(files_i, files_j) ≥ threshold]; components are reachable sets. *)
let group_by_jaccard ?(threshold = 0.7) (nodes : fusion_node list) :
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
      if jaccard arr.(i).fn_files arr.(j).fn_files >= threshold then
        union i j
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
    tuples ready for rule emission. Singleton groups produce one tuple
    each. Multi-node groups fuse into one conjunctive rule whose sites
    are the intersection of all members' file sets — but only if the
    intersection has ≥ [min_support] members; otherwise the fusion is
    abandoned and the members are emitted standalone (transitivity in
    the union-find can chain pairs whose all-way intersection is empty;
    standalone emission preserves coverage). Sections inside a fused
    rule are ordered by their rendered body for deterministic output. *)
let materialise_group ?(min_support = 2) (group : fusion_node list) :
    (edit_pat list * string list * string * int) list =
  match group with
  | [] -> []
  | [ n ] ->
      [ ([ n.fn_pattern ], n.fn_files, n.fn_language, n.fn_support) ]
  | _ ->
      let inter = intersect_all (List.map (fun n -> n.fn_files) group) in
      if List.length inter < min_support then
        List.map
          (fun n ->
            ([ n.fn_pattern ], n.fn_files, n.fn_language, n.fn_support))
          group
      else
        let language =
          match group with n :: _ -> n.fn_language | [] -> ""
        in
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
    multi-instance clusters. Clustering singletons at ~1000-site scale
    runs O(N³) through the dendrogram; deduplicating identical patterns
    up front cuts N dramatically — a refactor that renames one symbol at
    200 sites collapses into a single cluster before the dendrogram is
    even constructed. *)
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

let summarize ?progress ~ctx (cs : changeset) : summary =
  let on_file_for stage =
    match progress with
    | None -> None
    | Some p -> Some (fun ~idx ~total ~path -> p ~stage ~idx ~total ~path)
  in
  let raw =
    collect_initial_clusters ?on_file:(on_file_for "two-sided") ~ctx cs
  in
  let initial = pre_group_identical raw in
  if Sys.getenv_opt "CS_TRACE" <> None then begin
    Printf.eprintf "initial emissions: %d, clusters after pre-group: %d\n%!"
      (List.length raw) (List.length initial);
    let buckets = [| 0; 0; 0; 0; 0; 0; 0 |] in
    let bucket_of n =
      if n <= 5 then 0
      else if n <= 10 then 1
      else if n <= 20 then 2
      else if n <= 40 then 3
      else if n <= 80 then 4
      else if n <= 160 then 5
      else 6
    in
    List.iter (fun c ->
      let s = edit_size c.pattern in
      buckets.(bucket_of s) <- buckets.(bucket_of s) + 1)
      initial;
    Printf.eprintf
      "size hist (edit_size before+after): <=5:%d <=10:%d <=20:%d <=40:%d <=80:%d <=160:%d >160:%d\n%!"
      buckets.(0) buckets.(1) buckets.(2) buckets.(3) buckets.(4)
      buckets.(5) buckets.(6)
  end;
  let two_sided_clusters =
    match initial with
    | [] -> []
    | [ c ] ->
        (* Single pre-grouped cluster — no dendrogram needed. Check
           coherence and min_size directly. *)
        if
          List.length c.instances >= 2
          && has_concrete c.pattern.before
          && has_concrete c.pattern.after
          && has_concrete_edit c.pattern
          && hole_frac c.pattern < 0.35
          && cluster_applies ~ctx c
        then cover_sites [ c ]
        else []
    | _ ->
      let root = build_dendrogram initial in
      if Sys.getenv_opt "CS_TRACE" <> None then
        Printf.eprintf "dendrogram built\n%!";
      let clusters, _singletons = cut_dendrogram 2 root in
      if Sys.getenv_opt "CS_TRACE" <> None then
        Printf.eprintf "clusters: %d\n%!" (List.length clusters);
      let clusters =
        List.filter
          (fun c ->
            let ok = cluster_applies ~ctx c in
            if Sys.getenv_opt "CS_TRACE" <> None then begin
              let intended =
                match c.pattern.before with
                | Hole _ -> "<hole>"
                | Leaf { node_type; _ } -> node_type
                | PNode { node_type; _ } -> node_type
              in
              let text = render_pat_node c.pattern.before in
              Printf.eprintf "  applicability: intended=%s text=%S -> %b\n%!"
                intended text ok
            end;
            ok)
          clusters
      in
      if Sys.getenv_opt "CS_TRACE" <> None then
        Printf.eprintf "clusters after applicability: %d\n%!"
          (List.length clusters);
      let covered = cover_sites clusters in
      List.sort
        (fun a b ->
          compare (List.length b.instances) (List.length a.instances))
        covered
  in
  let candidates =
    collect_one_sided_candidates ?on_file:(on_file_for "one-sided") ~ctx cs
  in
  let os_clusters = cluster_one_sided candidates in
  let pairs = pair_one_sided_clusters os_clusters in
  let swap_pairs = List.filter_map (fun (r, a) -> fuse_swap r a) pairs in
  let nodes =
    List.map fusion_node_of_two_sided two_sided_clusters
    @ List.map (fun (ep, insts) -> fusion_node_of_swap ep insts) swap_pairs
  in
  let groups = group_by_jaccard nodes in
  let group_outputs = List.concat_map materialise_group groups in
  let sorted_outputs =
    List.sort
      (fun (_, _, _, s1) (_, _, _, s2) -> compare s2 s1)
      group_outputs
  in
  let mk_rule i (sections, sites, language, support) =
    let pattern_text =
      String.concat "\n" (List.map render_pattern_body sections)
    in
    {
      id = Printf.sprintf "R%d" (i + 1);
      pattern_text;
      support;
      language = (if language = "" then "unknown" else language);
      sites;
    }
  in
  { rules = List.mapi mk_rule sorted_outputs }

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
