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
      (** number of edits the rule makes in the applied chain, summed
          over [sites] — chain-effective, not evaluation-time (see
          [sites]) *)
  language : string;
  sites : string list;
      (** distinct file paths where the rule actually edits something
          when the summary's rules are applied in id order — not merely
          where its pattern matches the original source. An earlier rule
          can consume a later rule's matches at some files; those files
          are not listed (M2 chain-effect accounting). *)
  after : (string * string list) list;
      (** M2 per-site tier attribution: [(site, earlier rule ids)] — the
          rule's pattern at [site] matches the intermediate produced by
          applying those earlier rules, so it must be applied after them
          (rule-id order is application order). Empty for tier-1 rules;
          a site absent from the list has no predecessors. Global residual
          clustering makes this per-site: one tier-2 rule may follow
          different primaries at different sites (design §3.3 "common
          factors", §9.3). *)
}

type residual = {
  res_file : string;
  res_rules : string list;
  res_diff : string;
}

type summary = { rules : rule list; residuals : residual list }

(** Collapse whitespace runs to single spaces and trim. Layout-only
    differences are presentational, not a statement about the change —
    the same tolerance the safety gate's tree-level re-diff gives. *)
let ws_collapse s =
  let b = Buffer.create (String.length s) in
  let pend = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then pend := true
      else begin
        if !pend && Buffer.length b > 0 then Buffer.add_char b ' ';
        pend := false;
        Buffer.add_char b c
      end)
    s;
  Buffer.contents b

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
  ipat : edit_pat;
      (** The instance's own fully-concrete pattern (no holes) — what the
          site's change pair anti-unifies *from*. Kept so a cluster can be
          re-specialized over its surviving instances after covering and
          safety shedding: a hole the survivors no longer vary on
          collapses back to the literal. *)
}

type cluster = { pattern : edit_pat; instances : instance list }

type side = Before_side | After_side

type one_sided_instance = {
  os_file : string;
  os_line : int;
  os_language : string;
  os_text : string;
  os_side : side;
  os_start_byte : int;
  os_end_byte : int;
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

(* Metavar name for hole [h]. Sigil-free: the universal-tokenizer matcher
   treats a leaf as a metavar iff its text equals a declared name (no [$]
   sigil). A [$] prefix only tokenizes as part of an identifier in
   TypeScript/TSX; in Kotlin [$] is string-template syntax and in PHP it is
   the variable delimiter, so a [$]-prefixed name fails to lex as one leaf
   there. The leading [_] keeps the generated name from colliding with an
   ordinary source identifier while staying a valid identifier in every
   supported grammar. Any literal [$] in a rendered pattern (e.g. PHP's
   [$_H0]) comes from the source variable's own delimiter, preserved as a
   concrete leaf, not from this name. *)
let hole_name h = Printf.sprintf "_H%d" h

let rec render_pat_node = function
  | Hole h -> hole_name h
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
    (fun h ->
      Buffer.add_string buf (Printf.sprintf "metavar %s: single\n" (hole_name h)))
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

(** Render a removal-only pattern as a .pat-style block body containing
    only [-] lines. Used for unpaired Before_side one-sided clusters that
    survive M1.6 fusion as bare removals. *)
let render_removal_only_body (p : pat_node) : string =
  let holes = collect_holes [] p |> List.sort compare in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "@@\n";
  Buffer.add_string buf "match: strict\n";
  List.iter
    (fun h ->
      Buffer.add_string buf (Printf.sprintf "metavar %s: single\n" (hole_name h)))
    holes;
  Buffer.add_string buf "@@\n";
  let text = render_pat_node p in
  List.iter
    (fun line -> Buffer.add_string buf (Printf.sprintf "- %s\n" line))
    (String.split_on_char '\n' text);
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

(* respecialize is defined after the orphan-coarsening block below: it
   re-anti-unifies from fully-concrete ipats, so it must re-apply
   coarsening or a coarsened cluster's orphan would resurface. *)

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
    hole-rooted pattern that the downstream coherence and safety gates
    will reject. Comparing this signature is a string equality vs. a
    full tree anti-unification. *)
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

(* ── Per-site safety gate (design §2.3, §3.1) ────────────────────── *)

type site_info = {
  si_before : string;  (** full pre-change source of the file *)
  si_after : string;  (** full post-change source of the file *)
  si_language : string;  (** grammar the file parses with *)
  si_regions : (int * int * string) list;
      (** the file's changed regions in before-coordinates, sorted by
          start, disjoint: [(start, end, after_content)]. A zero-width
          region [(p, p, txt)] is an insertion at byte [p]. *)
  si_before_errors : string list;
      (** texts of the before-parse's ERROR nodes (usually empty). The
          well-formedness guard tolerates these in a rule's output — they
          predate the rule — while rejecting any error the rule invents. *)
}

(** Finest-grain changed regions of a diff, each carrying the
    after-side content that replaced it. The walk recurses through
    [Modified] chains so a region is the smallest changed node, not its
    enclosing scaffold; [Added] children become zero-width insertions at
    the before-position between their siblings. *)
let changed_regions (d : Tree_diff.diff) : (int * int * string) list =
  let acc = ref [] in
  let add s e txt = acc := (s, e, txt) :: !acc in
  let after_text (n : Tree.src Tree.t) = Tree.text d.after_source n in
  let rec go (b : Tree.src Tree.t) (a : Tree.src Tree.t)
      (ch : Tree_diff.node_change) =
    match ch with
    | Tree_diff.Unchanged -> ()
    | Tree_diff.Replaced -> add b.start_byte b.end_byte (after_text a)
    | Tree_diff.Modified { child_changes } ->
        let cursor = ref b.start_byte in
        List.iter
          (fun (cc : Tree_diff.child_change) ->
            match cc with
            | Tree_diff.Same { node } -> cursor := node.end_byte
            | Tree_diff.Changed { before; after; change } ->
                go before after change;
                cursor := before.end_byte
            | Tree_diff.Removed { node } ->
                add node.start_byte node.end_byte "";
                cursor := node.end_byte
            | Tree_diff.Added { node } -> add !cursor !cursor (after_text node))
          child_changes
  in
  go d.before_root d.after_root d.root_change;
  List.sort compare !acc

(* Source texts of the outermost ERROR nodes in a parse tree —
   tree-sitter's markers for unparseable stretches. Position-independent
   identities for the gate's well-formedness guard: edits elsewhere in
   the file shift an untouched error's offsets but not its text. *)
let error_texts source (root : Tree.src Tree.t) : string list =
  let rec go acc (n : Tree.src Tree.t) =
    if n.Tree.node_type = "ERROR" then Tree.text source n :: acc
    else
      List.fold_left
        (fun a (c : Tree.src Tree.child) -> go a c.node)
        acc n.Tree.children
  in
  go [] root

(* Multiset inclusion: every element of [xs] consumed from [allowance]. *)
let multiset_covered xs allowance =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun a ->
      Hashtbl.replace tbl a (1 + Option.value ~default:0 (Hashtbl.find_opt tbl a)))
    allowance;
  List.for_all
    (fun x ->
      match Hashtbl.find_opt tbl x with
      | Some n when n > 0 ->
          Hashtbl.replace tbl x (n - 1);
          true
      | _ -> false)
    xs

(* Plain substring membership: does [sub] occur in [s]? Used by the
   deletion-direction guard of the decomposable check. *)
let string_mem ~sub s =
  let n = String.length sub and m = String.length s in
  n = 0
  ||
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

(** Residual diff from a post-rule intermediate to the real after-source,
    with layout-only hunks dropped (§9.1): a hunk is kept only when it
    touches a tree-level changed region of the (intermediate, after)
    diff. Layout — re-indentation, [{ }] vs [{}], line splits — is
    invisible to the parse tree, so a hunk over lines no changed region
    touches states nothing about the change; the summary's reconstruction
    guarantee is already modulo layout (the whole-file gap check, the
    gate's tree-level re-diff). Returns [""] when the gap is entirely
    layout. Conservative in the keep direction: hunk and region line
    spans are each widened by one line before intersecting, and an
    unparseable side falls back to the unfiltered diff. *)
let residual_diff ~ctx ~language ~file_path ~original ~transformed () =
  if original = transformed then ""
  else
    let keep_hunk =
      try
        let bt = Tree.parse ~ctx ~language original in
        let at = Tree.parse ~ctx ~language transformed in
        let d = Tree_diff.diff ~before:bt ~after:at in
        let regions = changed_regions d in
        let line_starts =
          let acc = ref [ 0 ] in
          String.iteri
            (fun i c -> if c = '\n' then acc := (i + 1) :: !acc)
            original;
          Array.of_list (List.rev !acc)
        in
        let line_of byte =
          let lo = ref 0 and hi = ref (Array.length line_starts - 1) in
          while !lo < !hi do
            let mid = (!lo + !hi + 1) / 2 in
            if line_starts.(mid) <= byte then lo := mid else hi := mid - 1
          done;
          !lo
        in
        let region_spans =
          (* Exact line spans; only zero-width insertion regions get a
             one-line widening (the insertion point sits between lines,
             and the textual diff may render the added lines on either
             side of it). Symmetric widening of every span would bridge
             one-line gaps between a real change and an adjacent
             layout-only hunk. *)
          List.map
            (fun (s, e, _) ->
              if e <= s then (line_of s - 1, line_of s)
              else (line_of s, line_of (e - 1)))
            regions
        in
        Some
          (fun ~orig_start ~orig_len ->
            let h_lo = if orig_len = 0 then orig_start - 1 else orig_start in
            let h_hi =
              if orig_len = 0 then orig_start else orig_start + orig_len - 1
            in
            List.exists
              (fun (lo, hi) -> max lo h_lo <= min hi h_hi)
              region_spans)
      with _ -> None
    in
    match keep_hunk with
    | None ->
        Text_diff.generate_diff ~context:0 ~file_path ~original ~transformed
          ()
    | Some keep_hunk ->
        Text_diff.generate_diff ~context:0 ~keep_hunk ~file_path ~original
          ~transformed ()

(** [path → site_info] for every [Modified] file in the changeset. The
    safety gate evaluates rules against these. *)
let build_site_db ~ctx (cs : changeset) : (string, site_info) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun fc ->
      match fc with
      | Modified { path; language; before_source; after_source } -> (
          try
            let bt = Tree.parse ~ctx ~language before_source in
            let at = Tree.parse ~ctx ~language after_source in
            let d = Tree_diff.diff ~before:bt ~after:at in
            Hashtbl.replace tbl path
              {
                si_before = before_source;
                si_after = after_source;
                si_language = language;
                si_regions = changed_regions d;
                si_before_errors = error_texts before_source bt.Tree.root;
              }
          with _ -> ())
      | Added _ | Deleted _ -> ())
    cs.files;
  tbl

(* [s, e) and [rs, re) overlap. Zero-width intervals (a pure-removal
   landing zone, an insertion region) overlap when they touch the other
   interval, including at its boundary — a removal at the exact point
   where content must be re-added is a real conflict. Two non-zero-width
   intervals that merely share a boundary do not overlap. *)
let spans_overlap s e rs re =
  max s rs < min e re
  || (s = e && rs <= s && s <= re)
  || (rs = re && s <= rs && rs <= e)

(** Per-site safety gate: the operational form of the safety property
    (design §2.3) — with [t'' = apply(rule, t)],
    [d(t,t'') + d(t'',t') = d(t,t')]. Two legs (§3.1):

    {b Placement}: every edit the rule would make must intersect a
    changed region of the site's diff. An edit confined to unchanged
    territory is, by construction, a change that must be undone to reach
    the after-source — the over-merged [- import _H0] case, whose
    application would remove every import in the file.

    {b Content}: apply the rule ([t''] = the transformed source) and
    re-diff against the real after-source. No remaining change may
    overlap the rule's landing zones: a zone that still differs from
    [t'] means the rule wrote something other than what the changeset
    wrote there (claiming [f → h] where the change was [f → g]).
    Comparing via a re-diff rather than reconstructing expected text
    from the region list keeps separator tokens and layout — which the
    node-level regions do not cover — out of the comparison.

    A site where the rule produces no edits fails too — the rendered
    rule cannot fire there at all (the old zero-match applicability
    failure, e.g. a [property_identifier] rendered standalone re-parsing
    as a bare [identifier]).

    M1 emission policy: this is the [exact] classification only — the
    rule fully explains every region it touches; regions it does not
    touch are other rules' or residuals' business. A site where the rule
    makes safe-but-partial progress {e within} a region (the residual
    case, §4.4) is shed until M1.9b can attach residuals to state the
    gap honestly. *)
type site_evaluation = {
  ev_exact : bool;
      (** the gate verdict: the candidate fires and fully explains every
          region it touches (no remaining change in its landing zones). *)
  ev_decomposable : bool;
      (** M1.9b: the candidate fires and makes safe-but-*partial* progress
          within a region — [t''] differs from the after inside a landing
          zone, but the rule stays on the geodesic (§2.3): [t''] and the
          after are tree-inclusion comparable ([Tree_inclusion]), so the
          gap is a pure insertion or pure deletion — an honest residual
          rather than a detour (relabel) that must be undone. A
          decomposable site counts toward support and coverage;
          its in-zone gap is emitted as a [rule=]-attributed residual
          (§4.4) by the re-diff in [summarize]. Mutually exclusive with
          [ev_exact]. *)
  ev_fires : int;  (** number of edits the candidate makes at the site *)
  ev_resolved : int list;
      (** indices into [si_regions] of the changed regions the candidate
          fully resolves: regions an edit touches whose t''-image carries
          no remaining change after application. The selector's coverage
          unit (§3.3). A decomposable site lists only the regions it fully
          resolves; partial ones fall to the residual. Empty unless
          [ev_exact] or [ev_decomposable]. *)
  ev_clean : bool;
      (** the candidate, applied alone, reproduces the site's after-source
          (modulo whitespace) — i.e. it leaves no residual here. Used by
          selection to prefer a rule that reconstructs over one that only
          partially resolves the same regions (e.g. an extraction
          [box($H).get() ⤳ $H] over a bare removal [box($H).get()] that
          deletes and defers the rest to a residual). Always false for a
          decomposable-only site. *)
}

let no_fire =
  {
    ev_exact = false;
    ev_decomposable = false;
    ev_fires = 0;
    ev_resolved = [];
    ev_clean = false;
  }

(** Evaluate one candidate pattern at one site — the §3.1 gate, keeping
    the information it computes instead of reducing to a boolean. *)
let site_eval ~ctx ~language ~pattern_text (si : site_info) : site_evaluation
    =
  try
    let edits =
      Matcher.transform_edits ~ctx ~language ~pattern_text
        ~source_text:si.si_before
    in
    if edits = [] then no_fire
    else
      let placement_ok =
        List.for_all
          (fun (ed : Matcher.edit) ->
            List.exists
              (fun (rs, re, _) ->
                spans_overlap ed.start_byte ed.end_byte rs re)
              si.si_regions)
          edits
      in
      if not placement_ok then no_fire
      else begin
        (* Landing zones in t''-coordinates: each edit's span shifted by
           the cumulative length delta of the edits before it
           ([transform_edits] returns them sorted by start). *)
        let zones =
          let delta = ref 0 in
          List.map
            (fun (ed : Matcher.edit) ->
              let zs = ed.start_byte + !delta in
              let ze = zs + String.length ed.replacement in
              delta :=
                !delta
                + String.length ed.replacement
                - (ed.end_byte - ed.start_byte);
              (zs, ze))
            edits
        in
        (* Map a before-coordinate point to t''-coordinates: add the
           length deltas of the edits entirely before it; a point inside
           an edit span clamps into the edit's zone. Approximate inside
           edits — used only for coverage marking, where inaccuracy can
           cost a region its "resolved" mark (it falls to the residual,
           which stays honest) but cannot mis-state anything. *)
        let shift_pt p =
          let delta = ref 0 in
          let result = ref None in
          List.iter
            (fun (ed : Matcher.edit) ->
              match !result with
              | Some _ -> ()
              | None ->
                  if p >= ed.end_byte then
                    delta :=
                      !delta
                      + String.length ed.replacement
                      - (ed.end_byte - ed.start_byte)
                  else if p > ed.start_byte then
                    (* inside the edit span: clamp into its zone *)
                    result :=
                      Some
                        (ed.start_byte + !delta
                        + min (p - ed.start_byte)
                            (String.length ed.replacement)))
            edits;
          match !result with Some q -> q | None -> p + !delta
        in
        let t'' =
          Matcher.transform ~ctx ~language ~pattern_text
            ~source_text:si.si_before
        in
        let bt = Tree.parse ~ctx ~language t'' in
        let at = Tree.parse ~ctx ~language si.si_after in
        (* Well-formedness: a transform must produce parseable code. A
           removal-only rule that deletes a grammar-required
           sub-expression yields a broken intermediate ([const r = ;]);
           the re-diff over that ERROR-laden tree is unreliable, and it
           once judged such sites "fully explained" — letting a deletion
           rule out-cover the extraction rule and mis-state preserved
           values as deleted-then-readded. Every ERROR in [t''] must
           already exist in the before (pre-dates the rule; real corpora
           do contain the odd unparseable stretch) or in the after (the
           target state itself carries it) — by error text, position
           shifts aside. An error in neither endpoint is one the rule
           invented, and repairing rule-inflicted damage is not a
           residual's job. *)
        if
          not
            (multiset_covered
               (error_texts t'' bt.Tree.root)
               (si.si_before_errors @ error_texts si.si_after at.Tree.root))
        then no_fire
        else begin
        let d = Tree_diff.diff ~before:bt ~after:at in
        let remaining = changed_regions d in
        let exact =
          List.for_all
            (fun (rs, re, _) ->
              List.for_all
                (fun (zs, ze) -> not (spans_overlap zs ze rs re))
                zones)
            remaining
        in
        (* M1.9b decomposable: the rule's edits left a gap inside a landing
           zone, but [t''] is on the geodesic between before and after
           (design §2.3) — the rule's change plus the residual change
           compose to the site's change with no detour. Operationally:
           [t''] and the after must be *tree-inclusion comparable* (one
           obtainable from the other by node deletion alone,
           [Tree_inclusion]) — the residual is then a pure insertion
           ([t'' ⊑ after]: the rule under-wrote, e.g. an emptied
           dependency array the site fills) or a pure deletion
           ([after ⊑ t'']: the rule over-wrote through a metavariable,
           e.g. [g(x+1)] where the site keeps only [x]). A detour —
           writing a value in neither before nor after, [f→h] where the
           change is [f→g] — is a relabel, which inclusion forbids in
           both directions. The deletion direction additionally requires
           the deleted content to be before-derived (it reached [t'']
           through a metavariable binding, not an invented template
           literal whose insertion the residual would have to undo).
           Inclusion is checked on the whole file, so a site whose
           remaining gap mixes insertions and deletions — or overlaps
           another rule's pending region — is shed to its residual:
           conservative, honest, never unsafe.

           Inclusion alone validates only the *residual* leg. The rule's
           own leg needs the net-progress guard below: a rule may delete
           content the after still needs and let the residual re-add it —
           inclusion holds ([t'' ⊑ after], the re-add is "pure
           insertion") yet the delete-then-readd is wasted work off the
           geodesic. Soaks hit exactly this: a coarsened rule emptying a
           function body to [{}], whose residual re-inserts the whole
           body — safe by reconstruction, worse than the raw diff by
           size. The guard is the compactness half of the safety story
           (spdiff's largest *common* part, MDL): the in-zone gap the
           rule leaves must be strictly smaller than the change it
           explains, so claiming the site states the change more
           compactly than the raw hunk would. *)
        let net_progress =
          let extent (rs, re, txt) = re - rs + String.length txt in
          let gap =
            List.fold_left
              (fun a ((rs, re, _) as r) ->
                if
                  List.exists
                    (fun (zs, ze) -> spans_overlap zs ze rs re)
                    zones
                then a + extent r
                else a)
              0 remaining
          in
          let explained =
            List.fold_left
              (fun a ((rs, re, _) as r) ->
                if
                  List.exists
                    (fun (ed : Matcher.edit) ->
                      spans_overlap ed.start_byte ed.end_byte rs re)
                    edits
                then a + extent r
                else a)
              0 si.si_regions
          in
          gap < explained
        in
        let decomposable =
          (not exact)
          && net_progress
          && (Tree_inclusion.included_src
                ~sub:(t'', bt.Tree.root)
                ~sup:(si.si_after, at.Tree.root)
             ||
             (Tree_inclusion.included_src
                ~sub:(si.si_after, at.Tree.root)
                ~sup:(t'', bt.Tree.root)
             && List.for_all
                  (fun (rs, re, txt) ->
                    txt <> ""
                    || string_mem ~sub:(String.sub t'' rs (re - rs))
                         si.si_before)
                  remaining))
        in
        if not (exact || decomposable) then no_fire
        else
          let resolved =
            (* a region is resolved iff some edit touches it and its
               t''-image carries no remaining change *)
            let idx = ref (-1) in
            List.filter_map
              (fun (rs, re, _) ->
                incr idx;
                let touched =
                  List.exists
                    (fun (ed : Matcher.edit) ->
                      spans_overlap ed.start_byte ed.end_byte rs re)
                    edits
                in
                if not touched then None
                else
                  let rs' = shift_pt rs and re' = shift_pt re in
                  if
                    List.for_all
                      (fun (qs, qe, _) -> not (spans_overlap rs' re' qs qe))
                      remaining
                  then Some !idx
                  else None)
              si.si_regions
          in
          {
            ev_exact = exact;
            ev_decomposable = decomposable;
            ev_fires = List.length edits;
            ev_resolved = resolved;
            ev_clean = (t'' = si.si_after || ws_collapse t'' = ws_collapse si.si_after);
          }
        end
      end
  with _ -> no_fire

let site_safe ~ctx ~language ~pattern_text (si : site_info) : bool =
  (site_eval ~ctx ~language ~pattern_text si).ev_exact

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
               && List.for_all (fun s -> s = v) surfaces ->
            let node_type =
              List.fold_left
                (fun acc s ->
                  match (acc, s) with
                  | Some t, PNode n when t = n.node_type -> acc
                  | None, PNode n -> Some n.node_type
                  | _ -> Some "")
                None subs
            in
            (match node_type with
            | Some nt when nt <> "" ->
                subst_hole h (Leaf { node_type = nt; value = v }) after
            | _ -> after)
        | _ -> after)
      ep.after orphans
  in
  { ep with after }

(** Re-specialize a cluster to its surviving instances: re-anti-unify
    their concrete patterns from scratch. Anti-unification only holes a
    position whose values differ among the inputs, so every hole in a
    freshly-formed cluster is witnessed by at least two distinct
    instantiations — but covering and safety shedding remove instances
    *after* formation, and a rule can otherwise be emitted with a hole
    its own remaining sites never vary on (more general than its
    evidence). Folding [anti_unify_edits] over the survivors' [ipat]s
    restores the witnessed-holes invariant: positions the survivors
    agree on collapse back to literals. The fold is safe without hole
    shifting because the inputs are fully concrete. Coarsening is
    re-applied at the end: the fold rebuilds the pattern from concrete
    ipats, so a coarsened orphan would otherwise resurface. *)
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

let cut_dendrogram ?(threshold = 0.35)
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
    has_concrete ep.before
    && has_concrete_edit ep
    && no_orphan_after_holes ep
    && (s = 0
       || float_of_int (edit_holes ep) /. float_of_int s < threshold)
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

(** A removal-only [.pat] body for a concrete removed text (no holes):
    every line of [text] prefixed with [- ]. Used by the safety gate's
    concrete-regroup fallback, where a group of instances shares the
    removed text verbatim and no [pat_node] is at hand. *)
let removal_body_of_text (text : string) : string =
  let buf = Buffer.create (String.length text + 32) in
  Buffer.add_string buf "@@\nmatch: strict\n@@\n";
  List.iter
    (fun line -> Buffer.add_string buf (Printf.sprintf "- %s\n" line))
    (String.split_on_char '\n' text);
  Buffer.contents buf

(** Safe instances of a removal-only cluster, with a concrete-regroup
    fallback (design §3.1). First the cluster's own (possibly holed)
    pattern is safety-checked per site; if fewer than [min_support]
    sites survive — the over-merge case, e.g. [- import _H0] whose
    application would remove every import in the file — the instances
    are regrouped by their literal removed text and each group ≥
    [min_support] is gated with its own concrete pattern. The fallback
    recovers the concrete-majority rule that the merged hole erased
    (intermediate generalisations between the hole and the concrete
    texts are not currently recovered). Returns
    [(pattern_text, instances)] groups to emit. *)
let safe_removal_groups ~ctx ~site_db ?(min_support = 2)
    (c : one_sided_cluster) : (string * one_sided_instance list) list =
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
(* Hashes of every subtree of [n] (incl. [n] itself). [Tree.hash] is a
   structural, position-independent digest, so it doubles as hdiff's
   "which common subtree" oracle (§4.3): a node is a common subtree of
   two trees iff its hash appears in both. Comparing hashes across the
   before/after parses is sound — the digest excludes position and source
   buffer. *)
let rec subtree_hashes (n : Tree.src Tree.t) (acc : int list) : int list =
  List.fold_left
    (fun a (c : Tree.src Tree.child) -> subtree_hashes c.node a)
    (n.Tree.hash :: acc)
    n.Tree.children

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
                    os_start_byte = node.start_byte;
                    os_end_byte = node.end_byte;
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

(* A candidate pattern with its evaluated semantics (§3.3): the true
   extension (files where it fires safely, with each site's evaluation)
   and the behavioural support (total fires over the extension). *)
type scored_candidate = {
  sc_pattern : string;
  sc_language : string;
  sc_support : int;
  sc_extension : (string * site_evaluation) list;
}

(** One tier of the pipeline (§3.3): propose → evaluate → select over a
    changeset, returning the selected rules sorted by support, unnumbered
    ([id = ""], [after = []] — the M2 tier loop in [summarize] assigns
    both). Tier 1 runs this on the raw changeset; tier n+1 re-runs it on
    the (intermediate, after) pairs the earlier tiers leave unexplained
    (design §4.4 recursive clustering). *)
let tier_rules ~on_file_for ~ctx (cs : changeset) : rule list =
  let site_db = build_site_db ~ctx cs in
  (* Evaluation of a candidate pattern at one site, memoized on
     (pattern body, file). PROPOSE's internal gates (the dendrogram cut,
     swap fusion, removal regrouping) and EVALUATE share this cache, so
     proposer-side checks pre-warm the evaluator. *)
  let eval_cache : (string * string, site_evaluation) Hashtbl.t =
    Hashtbl.create 256
  in
  let eval_at ~language ~pattern_text file =
    if language = "" then no_fire
    else
      let key = (pattern_text, file) in
      match Hashtbl.find_opt eval_cache key with
      | Some e -> e
      | None ->
          let e =
            match Hashtbl.find_opt site_db file with
            | None -> no_fire
            | Some si ->
                if si.si_language <> language then no_fire
                else site_eval ~ctx ~language ~pattern_text si
          in
          Hashtbl.add eval_cache key e;
          e
  in
  let pattern_safe_at ~language ~pattern_text file =
    (* M1.9b/c: a decomposable site is safely explained too (geodesic), so
       it counts when shaping clusters — without it a coarsened candidate's
       own decomposable instances would be shed and the cluster dissolve. *)
    let e = eval_at ~language ~pattern_text file in
    e.ev_exact || e.ev_decomposable
  in
  let safe_instances ep (insts : instance list) =
    let pattern_text = render_pattern_body ep in
    List.filter
      (fun (i : instance) ->
        pattern_safe_at ~language:i.language ~pattern_text i.file)
      insts
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
  let all_files =
    Hashtbl.fold (fun k _ acc -> k :: acc) site_db []
    |> List.sort String.compare
  in
  let two_sided_clusters =
    match initial with
    | [] -> []
    | [ c ] ->
        (* Single pre-grouped cluster — no dendrogram needed. Check
           coherence, safety, and min_size directly. *)
        if
          List.length c.instances >= 2
          && has_concrete c.pattern.before
          && has_concrete c.pattern.after
          && has_concrete_edit c.pattern
          && hole_frac c.pattern < 0.35
        then begin
          let safe = safe_instances c.pattern c.instances in
          if List.length safe >= 2 then
            [ respecialize { c with instances = safe } ]
          else []
        end
        else []
    | _ ->
      let root = build_dendrogram initial in
      if Sys.getenv_opt "CS_TRACE" <> None then
        Printf.eprintf "dendrogram built\n%!";
      let clusters, _singletons = cut_dendrogram ~safe_instances 2 root in
      if Sys.getenv_opt "CS_TRACE" <> None then begin
        Printf.eprintf "clusters after safety cut: %d\n%!"
          (List.length clusters);
        List.iter
          (fun c ->
            let intended =
              match c.pattern.before with
              | Hole _ -> "<hole>"
              | Leaf { node_type; _ } -> node_type
              | PNode { node_type; _ } -> node_type
            in
            let text = render_pat_node c.pattern.before in
            Printf.eprintf "  safe cluster: intended=%s insts=%d text=%S\n%!"
              intended
              (List.length c.instances)
              text)
          clusters
      end;
      List.map respecialize clusters
  in
  let candidates =
    collect_one_sided_candidates ?on_file:(on_file_for "one-sided") ~ctx cs
  in
  let os_clusters = cluster_one_sided candidates in
  let pairs = pair_one_sided_clusters os_clusters in
  let swap_pairs = List.filter_map (fun (r, a) -> fuse_swap r a) pairs in
  (* Safety-gate fused swaps like any other two-sided rule: shed unsafe
     sites, drop the swap when fewer than two fires (removed-side
     instances) survive. *)
  let swap_pairs =
    List.filter_map
      (fun (ep, insts) ->
        let pattern_text = render_pattern_body ep in
        let safe =
          List.filter
            (fun (i : one_sided_instance) ->
              pattern_safe_at ~language:i.os_language ~pattern_text i.os_file)
            insts
        in
        let fires =
          List.length (List.filter (fun i -> i.os_side = Before_side) safe)
        in
        if fires >= 2 then Some (ep, safe) else None)
      swap_pairs
  in
  (* Fusion-input arbitration: without the old covering contest, the
     multi-level emission hands us several clusters stating the same
     change at nested granularities (statement / declarator / member)
     with identical file sets — Jaccard would fuse them into one
     self-overlapping conjunctive whose sections collide at application
     time. Keep one representative per change-family, chosen by
     *evaluated* resolved regions over all changed files —
     proposer shaping on true semantics, not provenance. Evaluating over
     the cluster's own provenance files would undercount a tighter
     candidate whose instances were partly shed during clustering: a
     nested call-level cluster (provenance 2 files) and the enclosing
     statement-level cluster (provenance 3) resolve the SAME regions when
     each is evaluated globally, so the tie-break below picks the shorter
     (tighter) one rather than the broader by accident of provenance.
     Preference: more resolved regions, then shorter pattern text. *)
  let fusion_inputs =
    let cluster_language (c : cluster) =
      match c.instances with i :: _ -> i.language | [] -> ""
    in
    let resolved_of (c : cluster) =
      let pattern_text = render_pattern_body c.pattern in
      let language = cluster_language c in
      all_files
      |> List.concat_map (fun f ->
          let e = eval_at ~language ~pattern_text f in
          List.map (fun i -> (f, i)) e.ev_resolved)
    in
    let scored =
      List.map (fun c -> (c, resolved_of c)) two_sided_clusters
      |> List.sort (fun (a, ra) (b, rb) ->
          compare
            (-List.length ra, String.length (render_pattern_body a.pattern))
            (-List.length rb, String.length (render_pattern_body b.pattern)))
    in
    let claimed : (string * int, unit) Hashtbl.t = Hashtbl.create 32 in
    List.filter_map
      (fun (c, resolved) ->
        let fresh =
          List.filter (fun k -> not (Hashtbl.mem claimed k)) resolved
        in
        if fresh = [] then None
        else begin
          List.iter (fun k -> Hashtbl.replace claimed k ()) resolved;
          Some c
        end)
      scored
  in
  let nodes =
    List.map fusion_node_of_two_sided fusion_inputs
    @ List.map (fun (ep, insts) -> fusion_node_of_swap ep insts) swap_pairs
  in
  let groups = group_by_jaccard nodes in
  let group_outputs = List.concat_map materialise_group groups in
  (* ── PROPOSE boundary (§3.3) ─────────────────────────────────────
     Everything above — extraction, clustering, cuts, fusion — only
     *proposes* candidate patterns from here on. Instance bookkeeping
     (which sites a cluster was born from) stays behind this line; a
     rule's sites, support, and coverage are derived by evaluation
     below, from the candidate's behaviour alone. *)
  let cand_tbl : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  let cand_order = ref [] in
  let add_candidate ~language pattern_text =
    if language <> "" then begin
      let key = (pattern_text, language) in
      if not (Hashtbl.mem cand_tbl key) then begin
        Hashtbl.add cand_tbl key ();
        cand_order := key :: !cand_order
      end
    end
  in
  (* Conjunctive fusions (a singleton group materialises as the node's
     own pattern). *)
  List.iter
    (fun (sections, _sites, language, _support) ->
      add_candidate ~language
        (String.concat "\n" (List.map render_pattern_body sections)))
    group_outputs;
  (* Every fusion-input cluster and fused swap individually, too: a
     fused form and its components are distinct candidates, and
     selection arbitrates between them on coverage. *)
  List.iter
    (fun (c : cluster) ->
      let language =
        match c.instances with i :: _ -> i.language | [] -> ""
      in
      add_candidate ~language (render_pattern_body c.pattern))
    fusion_inputs;
  List.iter
    (fun (ep, (insts : one_sided_instance list)) ->
      let language =
        match insts with i :: _ -> i.os_language | [] -> ""
      in
      add_candidate ~language (render_pattern_body ep))
    swap_pairs;
  (* Removal-only clusters that did not pair with an Added cluster in
     M1.6 fusion: their (possibly concretely regrouped) [-]-only bodies.
     Addition-only clusters are not proposed — a [+]-only block has no
     anchor to apply; their changes fall to residuals. *)
  let used_removeds = List.map fst pairs in
  List.iter
    (fun c ->
      if c.os_cluster_side = Before_side && not (List.memq c used_removeds)
      then
        List.iter
          (fun (pattern_text, (insts : one_sided_instance list)) ->
            let language =
              match insts with i :: _ -> i.os_language | [] -> ""
            in
            add_candidate ~language pattern_text)
          (safe_removal_groups ~ctx ~site_db c))
    os_clusters;
  let cands = List.rev !cand_order in
  if Sys.getenv_opt "CS_TRACE" <> None then
    Printf.eprintf "candidates proposed: %d\n%!" (List.length cands);
  (* ── EVALUATE (§3.3): each candidate's true extension ──────────── *)
  let evaluated =
    List.filter_map
      (fun (pattern_text, language) ->
        let extension =
          List.filter_map
            (fun f ->
              let e = eval_at ~language ~pattern_text f in
              (* M1.9b: a decomposable site fires safely (geodesic) and
                 counts toward support; its in-zone gap becomes a
                 [rule=]-attributed residual. *)
              if (e.ev_exact || e.ev_decomposable) && e.ev_fires > 0 then
                Some (f, e)
              else None)
            all_files
        in
        let support =
          List.fold_left (fun a (_, e) -> a + e.ev_fires) 0 extension
        in
        if support < 2 then None
        else
          Some
            {
              sc_pattern = pattern_text;
              sc_language = language;
              sc_support = support;
              sc_extension = extension;
            })
      cands
  in
  if Sys.getenv_opt "CS_TRACE" <> None then
    Printf.eprintf "candidates with viable extensions: %d\n%!"
      (List.length evaluated);
  (* ── SELECT (§3.3): greedy set-cover over changed regions ────────
     A candidate's marginal value is the number of still-uncovered
     (file, region) pairs it resolves; it is eligible while that
     marginal is at least min_support. Reported support stays the
     global fire count over the full extension. Ties break to higher
     support, then shorter pattern text (the tighter statement), then
     text for determinism. Subsumption is inherent: a candidate
     resolving only covered regions is never selected. *)
  let covered : (string * int, unit) Hashtbl.t = Hashtbl.create 64 in
  let marginal sc =
    List.fold_left
      (fun a (f, e) ->
        a
        + List.length
            (List.filter
               (fun i -> not (Hashtbl.mem covered (f, i)))
               e.ev_resolved))
      0 sc.sc_extension
  in
  let remaining = ref evaluated in
  let selected = ref [] in
  let picking = ref true in
  while !picking do
    let best =
      List.fold_left
        (fun acc sc ->
          let m = marginal sc in
          if m < 2 then acc
          else
            (* Among candidates covering the same marginal regions, prefer
               the one that reconstructs its sites with no residual
               (clean) — e.g. an extraction [box($H).get() ⤳ $H] over a
               bare removal [box($H).get()] that deletes and defers the
               rest to a residual. Then higher support, then shorter
               pattern text, then text. *)
            let clean =
              List.length (List.filter (fun (_, e) -> e.ev_clean) sc.sc_extension)
            in
            let key =
              ( m,
                clean,
                sc.sc_support,
                -String.length sc.sc_pattern,
                sc.sc_pattern )
            in
            match acc with
            | Some (bkey, _) when bkey >= key -> acc
            | _ -> Some (key, sc))
        None !remaining
    in
    match best with
    | None -> picking := false
    | Some (_, sc) ->
        selected := sc :: !selected;
        remaining := List.filter (fun x -> x != sc) !remaining;
        List.iter
          (fun (f, e) ->
            List.iter
              (fun i -> Hashtbl.replace covered (f, i) ())
              e.ev_resolved)
          sc.sc_extension;
        if Sys.getenv_opt "CS_TRACE" <> None then
          Printf.eprintf "  selected: support=%d %S\n%!" sc.sc_support
            (String.sub sc.sc_pattern 0
               (min 60 (String.length sc.sc_pattern)))
  done;
  List.rev !selected
  |> List.map (fun sc ->
      {
        id = "";
        pattern_text = sc.sc_pattern;
        support = sc.sc_support;
        language = sc.sc_language;
        sites = List.map fst sc.sc_extension;
        after = [];
      })
  |> List.sort (fun a b -> compare b.support a.support)

let summarize ?progress ~ctx (cs : changeset) : summary =
  let on_file_for stage =
    match progress with
    | None -> None
    | Some p -> Some (fun ~idx ~total ~path -> p ~stage ~idx ~total ~path)
  in
  (* Apply [path]'s claiming rules to [src] in rule-id order — the
     application contract shared by the tier loop, the residual pass and
     the round-trip property. *)
  let apply_claiming rules path ~language src =
    List.fold_left
      (fun s (r : rule) ->
        if r.language = language && List.mem path r.sites then
          try
            Matcher.transform ~ctx ~language:r.language
              ~pattern_text:r.pattern_text ~source_text:s
          with _ -> s
        else s)
      src rules
  in
  (* ── M2 tier loop (§4.4) ─────────────────────────────────────────
     Run propose/evaluate/select; then rebuild the changeset from the
     (intermediate, after) pairs the rules so far leave unexplained —
     including files no rule claims, whose residuals join the global
     pool (§3.3 common factors) — and recurse. Each emitting tier
     strictly shrinks the unexplained gap (the net-progress guard), so
     the loop terminates when a tier emits nothing; the depth cap and
     the no-progress check are backstops, not the intended exit. A
     tier-n rule's per-site [after] lists the earlier rules claiming
     that site: its pattern matched the intermediate those rules
     produce, so id order is application order. *)
  let max_tiers = 5 in
  let intermediate_key c =
    List.filter_map
      (function
        | Modified { path; before_source; _ } -> Some (path, before_source)
        | Added _ | Deleted _ -> None)
      c.files
  in
  (* Drop tier rules that never fire under id-order application. A
     rule's sites and coverage are evaluated rule-independently against
     the tier's changeset (§3.3), but application composes sequentially —
     an earlier rule's edits can consume a later rule's matches entirely
     (R1 = [f($X,$Y) ⤳ g($X)] rewrites the call that R2 =
     [f($X+1,$Y) ⤳ g($X)] would have matched). Keeping such a rule
     emits a dead pattern whose claimed sites mislead; dropping it leaves
     its regions unexplained for the *next* tier, which re-proposes
     against the actual intermediate ([( $X+1 ) ⤳ ( $X )], after=R1). *)
  let prune_dead (prior : rule list) (tier : rule list) : rule list =
    let fired : (string * string, unit) Hashtbl.t = Hashtbl.create 8 in
    List.iter
      (function
        | Modified { path; language; before_source; _ } ->
            ignore
              (List.fold_left
                 (fun s (r : rule) ->
                   if r.language = language && List.mem path r.sites then begin
                     let s' =
                       try
                         Matcher.transform ~ctx ~language:r.language
                           ~pattern_text:r.pattern_text ~source_text:s
                       with _ -> s
                     in
                     if s' <> s then
                       Hashtbl.replace fired (r.pattern_text, r.language) ();
                     s'
                   end
                   else s)
                 before_source (prior @ tier))
        | Added _ | Deleted _ -> ())
      cs.files;
    List.filter
      (fun (r : rule) -> Hashtbl.mem fired (r.pattern_text, r.language))
      tier
  in
  let rec tier_loop tier_idx (cur : changeset) (acc : rule list) : rule list
      =
    let tier = prune_dead acc (tier_rules ~on_file_for ~ctx cur) in
    if tier = [] then acc
    else
      let offset = List.length acc in
      let numbered =
        List.mapi
          (fun i (r : rule) ->
            let id = Printf.sprintf "R%d" (offset + i + 1) in
            let after =
              List.filter_map
                (fun site ->
                  match
                    List.filter
                      (fun (p : rule) -> List.mem site p.sites)
                      acc
                  with
                  | [] -> None
                  | preds -> Some (site, List.map (fun p -> p.id) preds))
                r.sites
            in
            { r with id; after })
          tier
      in
      let acc = acc @ numbered in
      if tier_idx >= max_tiers then acc
      else
        let next_files =
          List.filter_map
            (function
              | Modified { path; language; before_source; after_source } ->
                  let inter =
                    apply_claiming acc path ~language before_source
                  in
                  if
                    inter = after_source
                    || ws_collapse inter = ws_collapse after_source
                  then None
                  else
                    Some
                      (Modified
                         {
                           path;
                           language;
                           before_source = inter;
                           after_source;
                         })
              | Added _ | Deleted _ -> None)
            cs.files
        in
        let next = { files = next_files } in
        if
          next_files = []
          || intermediate_key next = intermediate_key cur
        then acc
        else tier_loop (tier_idx + 1) next acc
  in
  let combined = tier_loop 1 cs [] in
  (* ── Chain-effect accounting (per-site) ──────────────────────────
     A rule's sites and support come from rule-independent evaluation
     (§3.3), but application composes sequentially in id order — an
     earlier rule can consume a later rule's matches at *some* of its
     sites while the later rule stays live at others (the fused-rescue
     shape: [assignee = null ⤳ assignees = emptySet()] is a no-op
     wherever the bare rename already ran, yet is the only safe rule at
     a file the rename cannot claim). Reporting evaluation-time sites
     would list files where the rule never actually edits anything.
     Walk the chain once per file, recording which (rule, file) pairs
     really fire and the final intermediate; then shrink each rule's
     sites, support, and after-attribution to its chain-effective
     extension. Selection, rule ids, and application order are NOT
     revisited — the chain (and so reconstruction) is already fixed;
     this pass only makes the bookkeeping describe it truthfully. A
     chain-pruned rule may legitimately report support below
     min_support: it was selected for coverage it genuinely provides
     at its surviving sites. *)
  let fires : (string * string, int) Hashtbl.t = Hashtbl.create 32 in
  let inters : (string, string) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (function
      | Modified { path; language; before_source; _ } ->
          let inter =
            List.fold_left
              (fun s (r : rule) ->
                if r.language = language && List.mem path r.sites then
                  try
                    let edits =
                      Matcher.transform_edits ~ctx ~language:r.language
                        ~pattern_text:r.pattern_text ~source_text:s
                    in
                    if edits = [] then s
                    else begin
                      Hashtbl.replace fires (r.id, path) (List.length edits);
                      Matcher.transform ~ctx ~language:r.language
                        ~pattern_text:r.pattern_text ~source_text:s
                    end
                  with _ -> s
                else s)
              before_source combined
          in
          Hashtbl.replace inters path inter
      | Added _ | Deleted _ -> ())
    cs.files;
  let combined =
    List.filter_map
      (fun (r : rule) ->
        let sites =
          List.filter (fun f -> Hashtbl.mem fires (r.id, f)) r.sites
        in
        if sites = [] then None
        else
          let support =
            List.fold_left
              (fun a f ->
                a + Option.value ~default:0 (Hashtbl.find_opt fires (r.id, f)))
              0 sites
          in
          (* Keep the tier-derived after-attribution, restricted to the
             surviving sites, and within each site to predecessors that
             actually edited there (a no-op predecessor did not shape
             the intermediate this rule matched). *)
          let after =
            List.filter_map
              (fun (site, preds) ->
                if not (List.mem site sites) then None
                else
                  match
                    List.filter
                      (fun pid -> Hashtbl.mem fires (pid, site))
                      preds
                  with
                  | [] -> None
                  | preds -> Some (site, preds))
              r.after
          in
          Some { r with sites; support; after })
      combined
  in
  (* M1.9 residual extraction: for each Modified file, the chain pass
     above already produced the intermediate (claiming rules applied in
     id order); diff it against the real after-source. The gap, if any,
     is the residual — computed against what the rules *actually*
     produce, so rules + residual reproduce the site's change by
     construction. Files no rule claims yield unattributed residuals
     (pure one-off changes); Added/Deleted files appear as [/dev/null]
     residuals (M1.7). Layout-only gaps are skipped — the same tolerance
     the safety gate's tree-level re-diff gives. *)
  let residuals =
    let rules_at f =
      List.filter (fun (r : rule) -> List.mem f r.sites) combined
    in
    let file_op_diff ~added path content =
      let buf = Buffer.create (String.length content + 64) in
      if added then begin
        Buffer.add_string buf "--- /dev/null\n";
        Buffer.add_string buf (Printf.sprintf "+++ b/%s\n" path)
      end
      else begin
        Buffer.add_string buf (Printf.sprintf "--- a/%s\n" path);
        Buffer.add_string buf "+++ /dev/null\n"
      end;
      Buffer.add_string buf "@@ ... @@\n";
      let lines =
        match List.rev (String.split_on_char '\n' content) with
        | "" :: rest -> List.rev rest
        | l -> List.rev l
      in
      List.iter
        (fun line ->
          Buffer.add_char buf (if added then '+' else '-');
          Buffer.add_string buf line;
          Buffer.add_char buf '\n')
        lines;
      Buffer.contents buf
    in
    List.filter_map
      (fun fc ->
        match fc with
        | Modified { path; language; before_source; after_source } ->
            let claiming = rules_at path in
            let inter =
              match Hashtbl.find_opt inters path with
              | Some s -> s
              | None -> before_source
            in
            if
              inter = after_source
              || ws_collapse inter = ws_collapse after_source
            then None
            else
              let d =
                residual_diff ~ctx ~language ~file_path:path ~original:inter
                  ~transformed:after_source ()
              in
              if d = "" then None
              else
                Some
                  {
                    res_file = path;
                    res_rules = List.map (fun (r : rule) -> r.id) claiming;
                    res_diff = d;
                  }
        | Added { path; after_source; _ } ->
            Some
              {
                res_file = path;
                res_rules = [];
                res_diff = file_op_diff ~added:true path after_source;
              }
        | Deleted { path; before_source; _ } ->
            Some
              {
                res_file = path;
                res_rules = [];
                res_diff = file_op_diff ~added:false path before_source;
              })
      cs.files
  in
  { rules = combined; residuals }

let format_summary (s : summary) : string =
  let buf = Buffer.create 256 in
  List.iteri
    (fun i (r : rule) ->
      if i > 0 then Buffer.add_char buf '\n';
      (* §9.3 tier attribution: [after=] in the header when every site
         has the same predecessors, per-site annotations otherwise (a
         common-factor rule follows different primaries at different
         sites). *)
      let after_of site = List.assoc_opt site r.after in
      let uniform =
        match r.sites with
        | [] -> None
        | first :: rest -> (
            match after_of first with
            | Some preds when List.for_all (fun x -> after_of x = Some preds) rest
              ->
                Some preds
            | _ -> None)
      in
      Buffer.add_string buf
        (Printf.sprintf "# rule %s  support=%d  language=%s%s\n" r.id
           r.support r.language
           (match uniform with
           | Some preds -> "  after=" ^ String.concat "," preds
           | None -> ""));
      Buffer.add_string buf r.pattern_text;
      if r.sites <> [] then begin
        Buffer.add_string buf (Printf.sprintf "# sites %s\n" r.id);
        List.iter
          (fun p ->
            let annot =
              if uniform <> None then ""
              else
                match after_of p with
                | Some preds -> "  after=" ^ String.concat "," preds
                | None -> ""
            in
            Buffer.add_string buf (p ^ annot ^ "\n"))
          r.sites
      end)
    s.rules;
  List.iteri
    (fun i (res : residual) ->
      if i > 0 || s.rules <> [] then Buffer.add_char buf '\n';
      (match res.res_rules with
      | [] -> Buffer.add_string buf "# residual\n"
      | ids ->
          Buffer.add_string buf
            (Printf.sprintf "# residual  rule=%s\n" (String.concat "," ids)));
      Buffer.add_string buf res.res_diff)
    s.residuals;
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
