(** Change-summary pattern layer: convert a tree-sitter node to the internal
    [pat_node] representation ([of_src]), render an [edit_pat] back to .pat-style
    spatch text ([render_*]), and anti-unify pattern pairs ([anti_unify_*]).
    Depends only on {!Cs_types} and {!Tree}. *)

open Cs_types

(* ── Conversion ──────────────────────────────────────────────────── *)

(** Build the inter-child template for a tree-sitter node. Walks the node's byte
    range from [start_byte] to [end_byte], emitting [Lit] parts for source bytes
    outside any kept child and [Slot j] for the [j]-th kept child (0-based among
    kept children). Children for which [keep c = false] have their byte range
    absorbed into the surrounding [Lit] so the rendered output preserves the
    original surface text. Captures source bytes that the grammar consumes
    silently (e.g. string-literal quote delimiters in Kotlin/TS, or
    leading/trailing whitespace inside a parenthesised list). *)
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

(** True if any byte in the node's range that is not covered by a child is
    non-whitespace. These are "silently-consumed delimiters" — bytes the grammar
    embeds in the parent node's text but does not expose as children (e.g. the
    surrounding quotes of a string literal in Kotlin/TS, the slashes of a regex
    literal). Treat such nodes as leaves during [of_src]: anti-unification then
    holes the whole node when its content varies, rather than holing inside the
    delimiters and rendering a placeholder embedded inside a string literal —
    which the pattern parser would misread as the literal characters of the
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
    unnamed child nodes rather than consuming them silently. These nodes have
    full byte-coverage by children (so [has_silent_concrete_delimiters] misses
    them) but still wrap their content in tokens that would sandwich any
    rendered hole into a string-literal token. Treat as a leaf when an unnamed
    child has text equal to a string-quote character. Limited to double-quote,
    single-quote, and backtick; the slash is excluded because it is also the
    division operator and would misclassify binary expressions. *)
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
      Buffer.add_string buf
        (Printf.sprintf "metavar %s: single\n" (hole_name h)))
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

(** Surgical render (§3.2 anchored realisations): lines common to both sides
    become context lines, so the transform's edit spans only the changed lines.
    With the whole-construct [-]/[+] render, the rule's landing zone covers the
    entire matched construct — including holed arguments whose interior carries
    *other* changes at the site, which the content gate then rejects ("remaining
    change overlapping the landing zone"). Context lines shrink the zone to the
    delta's own lines. Alignment is longest-common-prefix/suffix over rendered
    lines — exact for single-delta anchored variants; falls back to the full
    [-]/[+] render when nothing aligns. *)
let render_pattern_body_surgical (ep : edit_pat) : string =
  let b_lines = String.split_on_char '\n' (render_pat_node ep.before) in
  let a_lines = String.split_on_char '\n' (render_pat_node ep.after) in
  let rec common_prefix acc = function
    | b :: bs, a :: as_ when b = a -> common_prefix (b :: acc) (bs, as_)
    | rest -> (List.rev acc, rest)
  in
  let prefix, (b_rest, a_rest) = common_prefix [] (b_lines, a_lines) in
  let suffix, (b_mid_rev, a_mid_rev) =
    common_prefix [] (List.rev b_rest, List.rev a_rest)
  in
  let suffix = List.rev suffix in
  let b_mid = List.rev b_mid_rev and a_mid = List.rev a_mid_rev in
  if prefix = [] && suffix = [] then render_pattern_body ep
  else begin
    let holes = edit_hole_list ep in
    let buf = Buffer.create 128 in
    Buffer.add_string buf "@@\nmatch: strict\n";
    List.iter
      (fun h ->
        Buffer.add_string buf
          (Printf.sprintf "metavar %s: single\n" (hole_name h)))
      holes;
    Buffer.add_string buf "@@\n";
    List.iter
      (fun l -> Buffer.add_string buf (Printf.sprintf "  %s\n" l))
      prefix;
    List.iter (fun l -> Buffer.add_string buf (Printf.sprintf "- %s\n" l)) b_mid;
    List.iter (fun l -> Buffer.add_string buf (Printf.sprintf "+ %s\n" l)) a_mid;
    List.iter
      (fun l -> Buffer.add_string buf (Printf.sprintf "  %s\n" l))
      suffix;
    Buffer.contents buf
  end

(** Render a removal-only pattern as a .pat-style block body containing only [-]
    lines. Used for unpaired Before_side one-sided clusters that survive M1.6
    fusion as bare removals. *)
let render_removal_only_body (p : pat_node) : string =
  let holes = collect_holes [] p |> List.sort compare in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "@@\n";
  Buffer.add_string buf "match: strict\n";
  List.iter
    (fun h ->
      Buffer.add_string buf
        (Printf.sprintf "metavar %s: single\n" (hole_name h)))
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

(** A pattern is "concrete" iff it contains at least one named [Leaf] or a
    keyword-shaped unnamed token (one whose text contains an alphabetic
    character — [array], [function], [class] etc.). Empty [PNode]s whose
    node_type is pure punctuation ([,], [(], [;]) don't count: the pattern is
    structural scaffolding without a keyword anchor and matches arbitrary
    content of the right shape. The keyword carve-out is what lets a PHP rule
    like [array($H0) -> [$H0]] survive coherence: the [array] keyword is the
    distinguishing concrete signal even though every named-leaf descendant
    becomes a hole. *)
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
  | PNode { children; _ } ->
      List.exists (fun c -> has_concrete c.child) children

(** [has_adjacent_holes p] is true iff rendering [p] would place two holes
    side-by-side with no token between them — concrete or punctuation. Such a
    fragment ([_H0_H1]) is unmatchable: the tokenizer-based matcher can't bind
    two adjacent metavars with no anchor. Computed over the render [template] so
    it mirrors {!render_pat_node} exactly: a [Lit] is a literal token (a
    separator, including punctuation like [(] / [,]), a [Slot] renders a child.
    A [(_H0)] or [_H0(_H1, _H2)] is fine — the parens/comma sit between holes —
    but a hole directly followed by a hole-only subtree is not. *)
let has_adjacent_holes p =
  let rec toks acc node =
    match node with
    | Hole _ -> true :: acc
    | Leaf _ -> false :: acc
    | PNode { children = []; _ } -> false :: acc
    | PNode { children; template; _ } ->
        let arr = Array.of_list children in
        List.fold_left
          (fun a -> function
            | Lit _ -> false :: a
            | Slot i -> toks a arr.(i).child)
          acc template
  in
  let rec adjacent = function
    | true :: (true :: _ as rest) -> true || adjacent rest
    | _ :: rest -> adjacent rest
    | [] -> false
  in
  adjacent (List.rev (toks [] p))

(* ── Anti-unification ────────────────────────────────────────────── *)

(** Build a recursive anti-unifier parameterised on a [hole_for] function. When
    [hole_for] is shared across multiple invocations (as in [anti_unify_edits]
    across the before and after sides), identical concrete-pair differences
    reuse the same hole index. *)
let mk_anti_unify hole_for =
  let rec go p1 p2 =
    match (p1, p2) with
    | Hole h1, Hole h2 when h1 = h2 -> Hole h1
    | Leaf l1, Leaf l2 when l1.node_type = l2.node_type && l1.value = l2.value
      ->
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
                { field_name = None; child = Hole (hole_for c1.child c2.child) })
            n1.children n2.children
        in
        (* Collapse-to-single-hole: if anti-unifying a node's children leaves it
           rendering as adjacent holes ([_H0_H1]) — two holes with no token
           between them — the fragment is unmatchable and the safety gate rejects
           it (safe=0), forcing the dendrogram cut down to shape-homogeneous
           sub-clusters. Collapse the whole node to one hole: it stays anchored
           by its surrounding context and reproduces the subtree verbatim (the
           shared [hole_for] memo gives the before- and after-side occurrences
           the same index, so it round-trips).

           [has_adjacent_holes] (a render-order check), not "every child is a
           [Hole]": Kotlin buries call args under a [call_suffix] wrapper, so
           divergent args give [call_expression[Hole; call_suffix[Hole]]] —
           rendered [_H0_H1] though the second child is a PNode, not a bare hole.
           And not "no concrete leaf", which would wrongly collapse a
           parens-anchored [(_H0)] (punctuation is non-concrete but a real
           anchoring token). A single hole or a hole flanked by tokens is left
           alone, so single-child wrappers never collapse — preserving the
           before/after hole alignment that a re-keyed wrapper hole would break. *)
        let node = PNode { n1 with children } in
        if has_adjacent_holes node then Hole (hole_for p1 p2) else node
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

(** Anti-unify two single [pat_node]s (used for one-sided candidate clustering
    in M1.6a). Uses its own hole counter — no cross-side sharing. *)
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

(* ── Coherence predicates ────────────────────────────────────────── *)
(* Pattern-level predicates shared by the clustering ({!Cs_cluster}) and
   proposal ({!Cs_propose}) phases — a pattern's hole fraction and the
   coherence-gate tests of design §3.1. *)

let hole_frac ep =
  let s = edit_size ep in
  if s = 0 then 0.0 else float_of_int (edit_holes ep) /. float_of_int s

let rec collect_leaf_values acc = function
  | Hole _ -> acc
  | Leaf { value; _ } -> value :: acc
  | PNode { children; _ } ->
      List.fold_left (fun a c -> collect_leaf_values a c.child) acc children

(** [same_shape_mod_holes p1 p2] holds iff [p1] and [p2] have identical
    structural shape — same node types at every position and the same number of
    children at every [PNode]. Leaf {e values} are not compared: a pure rename
    [foo -> bar] is same-shape, but a call with different argument counts
    ([foo(a, b)] vs [bar(a)]) is not.

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

(** True if the edit pattern carries at least one concrete edit signal — either
    (a) the multiset of named-leaf values differs between [-] and [+] sides (a
    concrete rename), or (b) the shape differs modulo holes (a structural edit
    such as a dropped argument). A pattern whose only differences are in which
    hole-variable index appears where is substance-free: it's a generic
    "something in this context changed" matcher that fires at every Modified
    ancestor and drowns more specific rules. Rejected at coherence time. *)
let has_concrete_edit (ep : edit_pat) : bool =
  let b = List.sort compare (collect_leaf_values [] ep.before) in
  let a = List.sort compare (collect_leaf_values [] ep.after) in
  b <> a || not (same_shape_mod_holes ep.before ep.after)

(** No replace-side hole appears that lacks a binding source on the match side.
    A pattern with a [+]-side metavariable not present on the [-] side is
    rejected by the spatch engine at apply time ("Metavars in replacement not
    bound in match"), so emitting it would produce an unapplicable rule. This
    usually means the cluster's anti-unification dropped some context that
    carried the binding source — fall back to the coherent dendrogram parent
    instead. See design doc §4.3. *)
let no_orphan_after_holes (ep : edit_pat) : bool =
  let before_holes = collect_holes [] ep.before in
  let after_holes = collect_holes [] ep.after in
  List.for_all (fun h -> List.mem h before_holes) after_holes
