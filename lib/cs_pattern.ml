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
  | Ellipsis -> "..."
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
  | Ellipsis -> acc
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

(** Field-mode render (Piece B): render a declaration-anchored [edit_pat] whose
    change is confined to one child (e.g. a function body) as a [match: field]
    rule. The unchanged signature children render as a context line, the changed
    child as [-]/[+], and children selected by [omit] (presence-varying or
    edge-positioned optionals — return type, modifiers, annotations) are dropped
    so field mode ignores them. Falls back to {!render_pattern_body} when the
    edit is not a single-child change of a [PNode] pair (so callers can use it
    unconditionally). [omit] receives a child's [field_name]. *)
let render_pattern_body_field ?(omit = fun (_ : string option) -> false)
    (ep : edit_pat) : string =
  match (ep.before, ep.after) with
  | PNode b, PNode a
    when b.node_type = a.node_type
         && List.length b.children = List.length a.children ->
      let bch = Array.of_list b.children in
      let ach = Array.of_list a.children in
      let n = Array.length bch in
      let changed i =
        render_pat_node bch.(i).child <> render_pat_node ach.(i).child
      in
      let changed_idxs = List.filter changed (List.init n (fun i -> i)) in
      let omit_idx i =
        (not (List.mem i changed_idxs)) && omit bch.(i).field_name
      in
      (match changed_idxs with
      | [ ci ] ->
          (* Drop omitted slots and one adjacent separator [Lit] each
             (preceding if present — the [": "] of a return type — else the
             following, for a leading modifier list). *)
          let pruned =
            let rec go acc = function
              | [] -> List.rev acc
              | Slot j :: rest when omit_idx j -> (
                  match acc with
                  | Lit _ :: acc' -> go acc' rest
                  | _ -> (
                      match rest with Lit _ :: r -> go acc r | r -> go acc r))
              | p :: rest -> go (p :: acc) rest
            in
            go [] b.template
          in
          let render_parts parts =
            let buf = Buffer.create 64 in
            List.iter
              (function
                | Lit s -> Buffer.add_string buf s
                | Slot j -> Buffer.add_string buf (render_pat_node bch.(j).child))
              parts;
            Buffer.contents buf
          in
          let rec split acc = function
            | Slot j :: rest when j = ci -> (List.rev acc, rest)
            | p :: rest -> split (p :: acc) rest
            | [] -> (List.rev acc, [])
          in
          let pre_parts, suf_parts = split [] pruned in
          let rstrip s =
            let j = ref (String.length s) in
            while !j > 0 && (s.[!j - 1] = ' ' || s.[!j - 1] = '\t') do
              decr j
            done;
            String.sub s 0 !j
          in
          let prefix = rstrip (render_parts pre_parts) in
          let suffix = render_parts suf_parts in
          let minus = render_pat_node bch.(ci).child in
          let plus = render_pat_node ach.(ci).child in
          let holes = edit_hole_list ep in
          let buf = Buffer.create 128 in
          Buffer.add_string buf "@@\nmatch: field\n";
          List.iter
            (fun h ->
              Buffer.add_string buf
                (Printf.sprintf "metavar %s: single\n" (hole_name h)))
            holes;
          Buffer.add_string buf "@@\n";
          let emit marker text =
            List.iter
              (fun l -> Buffer.add_string buf (Printf.sprintf "%s%s\n" marker l))
              (String.split_on_char '\n' text)
          in
          if prefix <> "" then emit "  " prefix;
          emit "- " minus;
          emit "+ " plus;
          if String.trim suffix <> "" then emit "  " suffix;
          Buffer.contents buf
      | _ -> render_pattern_body ep)
  | _ -> render_pattern_body ep

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
  | Ellipsis -> 0
  | Leaf _ -> 0
  | PNode { children; _ } ->
      List.fold_left (fun a c -> a + count_holes c.child) 0 children

let rec pat_size = function
  | Hole _ -> 1
  | Ellipsis -> 1
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
  | Ellipsis -> false
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
    | Ellipsis -> true :: acc
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

(* ── Sequence (ellipsis) generalisation — Piece C ──────────────────── *)

(* The first / last character a node renders to, computed by walking only the
   left / right spine (no full render). Used to detect a bracket-delimited node
   whether its delimiters are silent template [Lit]s (TS/Kotlin strings) or
   unnamed child tokens (Kotlin's [(]/[)] are children, not Lits) — so detection
   is by surface bracket shape, language-generally, not by node type. *)
let rec first_rendered_char node =
  match node with
  | Hole _ -> Some '_'
  | Ellipsis -> Some '.'
  | Leaf { value; _ } -> if value = "" then None else Some value.[0]
  | PNode { children = []; node_type; _ } ->
      if node_type = "" then None else Some node_type.[0]
  | PNode { children; template; _ } ->
      let arr = Array.of_list children in
      let rec go = function
        | Lit s :: rest ->
            let s' = String.trim s in
            if s' <> "" then Some s'.[0] else go rest
        | Slot i :: rest -> (
            match first_rendered_char arr.(i).child with
            | Some _ as r -> r
            | None -> go rest)
        | [] -> None
      in
      go template

let rec last_rendered_char node =
  match node with
  | Hole _ -> Some '_'
  | Ellipsis -> Some '.'
  | Leaf { value; _ } ->
      if value = "" then None else Some value.[String.length value - 1]
  | PNode { children = []; node_type; _ } ->
      if node_type = "" then None
      else Some node_type.[String.length node_type - 1]
  | PNode { children; template; _ } ->
      let arr = Array.of_list children in
      let rec go = function
        | Lit s :: rest ->
            let s' = String.trim s in
            if s' <> "" then Some s'.[String.length s' - 1] else go rest
        | Slot i :: rest -> (
            match last_rendered_char arr.(i).child with
            | Some _ as r -> r
            | None -> go rest)
        | [] -> None
      in
      go (List.rev template)

(** A node's surrounding bracket pair if it renders as a delimited list —
    [(...)], [[...]], [{...}], [<...>]. Returns the opening/closing chars.
    Identifies parameter / argument / type-argument lists and collection
    literals by bracket shape, not node type (language-general). *)
let seq_brackets p =
  match (first_rendered_char p, last_rendered_char p) with
  | Some o, Some c when String.contains "([{<" o && String.contains ")]}>" c ->
      Some (o, c)
  | _ -> None

let node_has_ellipsis = function
  | PNode { children; _ } ->
      List.exists
        (function { child = Ellipsis; _ } -> true | _ -> false)
        children
  | _ -> false

let rec contains_ellipsis = function
  | Ellipsis -> true
  | Hole _ | Leaf _ -> false
  | PNode { children; _ } ->
      List.exists (fun c -> contains_ellipsis c.child) children

(** Replace a delimited node's interior with a single [Ellipsis], keeping its
    bracket pair — [(a, b)] / [<S, E>] become [(...)] / [<...>]. *)
let ellipsis_list_of node_type is_named (o, c) =
  PNode
    {
      node_type;
      is_named;
      children = [ { field_name = None; child = Ellipsis } ];
      template = [ Lit (String.make 1 o); Slot 0; Lit (String.make 1 c) ];
    }

(* ── Field-aware alignment — Piece B increment 2 ───────────────────── *)

let node_type_of = function
  | Hole _ -> "_hole"
  | Ellipsis -> "_ellipsis"
  | Leaf { node_type; _ } | PNode { node_type; _ } -> node_type

(** Longest common subsequence of two child lists, matching on field name when
    both sides have one (rare — most grammars, e.g. tree-sitter-kotlin, assign
    almost none) else on node type. Returns the matched [(i, j)] index pairs in
    order; unmatched children (a return type present on one side only) fall out
    and are dropped by the caller. *)
let lcs_align (a : pat_child array) (b : pat_child array) : (int * int) list =
  let na = Array.length a and nb = Array.length b in
  let matches c1 c2 =
    match (c1.field_name, c2.field_name) with
    | Some f1, Some f2 -> f1 = f2
    | _ -> node_type_of c1.child = node_type_of c2.child
  in
  let dp = Array.make_matrix (na + 1) (nb + 1) 0 in
  for i = na - 1 downto 0 do
    for j = nb - 1 downto 0 do
      dp.(i).(j) <-
        (if matches a.(i) b.(j) then 1 + dp.(i + 1).(j + 1)
         else max dp.(i + 1).(j) dp.(i).(j + 1))
    done
  done;
  let rec bt i j acc =
    if i >= na || j >= nb then List.rev acc
    else if matches a.(i) b.(j) then bt (i + 1) (j + 1) ((i, j) :: acc)
    else if dp.(i + 1).(j) >= dp.(i).(j + 1) then bt (i + 1) j acc
    else bt i (j + 1) acc
  in
  bt 0 0 []

(** Rebuild a template after dropping some slots: [newidx_of j] gives the kept
    child's new index, or [None] if dropped. A dropped slot takes one adjacent
    separator [Lit] with it (preceding if present — the [": "] of a return type
    — else following, for a leading modifier list). *)
let remap_template template newidx_of =
  let rec go acc = function
    | [] -> List.rev acc
    | Slot j :: rest -> (
        match newidx_of j with
        | Some nj -> go (Slot nj :: acc) rest
        | None -> (
            match acc with
            | Lit _ :: acc' -> go acc' rest
            | _ -> ( match rest with Lit _ :: r -> go acc r | r -> go acc r)))
    | (Lit _ as l) :: rest -> go (l :: acc) rest
  in
  go [] template

(* ── Anti-unification ────────────────────────────────────────────── *)

(** Build a recursive anti-unifier parameterised on a [hole_for] function. When
    [hole_for] is shared across multiple invocations (as in [anti_unify_edits]
    across the before and after sides), identical concrete-pair differences
    reuse the same hole index. *)
let mk_anti_unify ?(allow_ellipsis = true) hole_for =
  let rec go p1 p2 =
    match (p1, p2) with
    | Hole h1, Hole h2 when h1 = h2 -> Hole h1
    | Ellipsis, Ellipsis -> Ellipsis
    | Leaf l1, Leaf l2 when l1.node_type = l2.node_type && l1.value = l2.value
      ->
        Leaf l1
    | PNode n1, PNode n2
      when n1.node_type = n2.node_type && n1.is_named = n2.is_named -> (
        let same_count =
          List.length n1.children = List.length n2.children
        in
        match seq_brackets p1 with
        | Some delims
          when allow_ellipsis
               && ((not same_count) || node_has_ellipsis p1
                  || node_has_ellipsis p2) ->
            (* Piece C: a delimited list whose arity differs across the two
               sides (or that one side already generalised) becomes [(...)] —
               the interior is a sequence wildcard, the brackets are kept as the
               anchor. This is what lets functions/calls of different arity merge
               into one cluster instead of fragmenting per-arity. *)
            ellipsis_list_of n1.node_type n1.is_named delims
        | _ when same_count ->
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
            (* Collapse to a single hole when the recursed node has no surviving
               anchor: adjacent holes ([_H0_H1], unmatchable) or — with Piece C —
               a node that contains an ellipsis yet has no concrete token
               ([_H0(...)], "any call"). The ellipsis is kept only when a concrete
               sibling anchors it ([unwrap(...)], [fun f(...)]); without one the
               whole subtree folds, restoring the pre-ellipsis behaviour where a
               varying nested call became a clean single hole. Restricted to
               ellipsis-bearing nodes so non-ellipsis anti-unification is
               byte-identical to before. *)
            let multi_child =
              match children with _ :: _ :: _ -> true | _ -> false
            in
            if
              has_adjacent_holes node
              || (multi_child && contains_ellipsis node
                 && not (has_concrete node))
            then Hole (hole_for p1 p2)
            else node
        | _ ->
            (* Differing child count, not a bracketed list. If one side's
               children FULLY embed in the other by node type (the pure
               "optional child present on one side" case — return type,
               modifiers, annotations), align and drop the extras instead of
               collapsing to a bare hole. A genuine structural divergence (both
               sides carry unique children) still collapses. *)
            let a = Array.of_list n1.children in
            let b = Array.of_list n2.children in
            let pairs = lcs_align a b in
            if
              (* Not a bracketed list — those are ellipsis-or-collapse territory
                 (dropping an argument would diverge from the other side). *)
              seq_brackets p1 = None
              && pairs <> []
              && List.length pairs
                 = min (Array.length a) (Array.length b)
            then
              field_align ~node_type:n1.node_type ~is_named:n1.is_named
                ~template:n1.template a b pairs
            else Hole (hole_for p1 p2))
    | _ -> Hole (hole_for p1 p2)
  and field_align ~node_type ~is_named ~template a b pairs =
    (* Align children by the LCS, recurse the matched ones, drop the unmatched
       (one-sided optional) children, and rebuild the template — so declarations
       of different signature shape merge into one cluster. *)
    let merged_children =
      List.map
        (fun (i, j) ->
          let c1 = a.(i) and c2 = b.(j) in
          if c1.field_name = c2.field_name then
            { field_name = c1.field_name; child = go c1.child c2.child }
          else
            { field_name = None; child = Hole (hole_for c1.child c2.child) })
        pairs
    in
    let kept = List.map fst pairs in
    let newidx_of j =
      let rec find k = function
        | [] -> None
        | x :: _ when x = j -> Some k
        | _ :: rest -> find (k + 1) rest
      in
      find 0 kept
    in
    PNode
      {
        node_type;
        is_named;
        children = merged_children;
        template = remap_template template newidx_of;
      }
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
  (* Ellipsis ([...]) is sound only on the match ([-]) side, where it is a
     constraint. On the replace ([+]) side an [...] with no match-side source is
     an orphan the engine cannot bind (an added, arity-varying list — e.g. a
     useCallback dependency array); the empty-skeleton + residual path handles
     that instead. Both calls share one [hole_for] so hole indices still align
     across the two sides. *)
  let hole_for = make_hole_for () in
  let before = mk_anti_unify ~allow_ellipsis:true hole_for e1.before e2.before in
  (* The replace side may ellipsize only when the match side did — a preserved
     list (e.g. function params, present on both sides) then gets matching
     [(...)] and binds; an added, arity-varying list (e.g. a useCallback
     dependency array, present only on the after side) does not ellipsize and
     falls to the hole → empty-skeleton + residual path instead of becoming an
     orphan [...]. *)
  let after =
    mk_anti_unify ~allow_ellipsis:(contains_ellipsis before) hole_for e1.after
      e2.after
  in
  { before; after }

(** Anti-unify two single [pat_node]s (used for one-sided candidate clustering
    in M1.6a). Uses its own hole counter — no cross-side sharing. *)
let anti_unify_pat (p1 : pat_node) (p2 : pat_node) : pat_node =
  let go = mk_anti_unify (make_hole_for ()) in
  go p1 p2

let rec max_hole_node = function
  | Hole h -> h
  | Ellipsis -> -1
  | Leaf _ -> -1
  | PNode { children; _ } ->
      List.fold_left (fun m c -> max m (max_hole_node c.child)) (-1) children

let max_hole ep = max (max_hole_node ep.before) (max_hole_node ep.after)

let rec shift_holes_node offset = function
  | Hole h -> Hole (h + offset)
  | Ellipsis -> Ellipsis
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
  | Ellipsis -> acc
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
  | Ellipsis, Ellipsis -> true
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
    instead. See design doc §4.3.

    The same applies to the sequence wildcard [...] (Piece C): an [...] on the
    [+] side with no [...] on the [-] side is an orphan the engine cannot bind
    (an added, arity-varying list — e.g. a useCallback dependency array — which
    the empty-skeleton + residual path handles instead). A [...] on both sides
    is a preserved list (e.g. [fun f(...)] in match and replace) and is fine. *)
let no_orphan_after_holes (ep : edit_pat) : bool =
  let before_holes = collect_holes [] ep.before in
  let after_holes = collect_holes [] ep.after in
  List.for_all (fun h -> List.mem h before_holes) after_holes
  && ((not (contains_ellipsis ep.after)) || contains_ellipsis ep.before)
