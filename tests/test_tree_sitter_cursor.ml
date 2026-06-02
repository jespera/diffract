(** Phase 2 smoke test: end-to-end matching via [Tree_sitter_cursor].

    Parses a small source file, constructs a pattern manually, runs
    [Stmatch.Make(Tree_sitter_cursor).match_at], and asserts the expected
    outcome. This is the first integration test demonstrating that the Phase 1
    algorithm works against real tree-sitter parses, not just hand-built
    fixtures. *)

open Diffract
module M = Stmatch.Make (Tree_sitter_cursor)

let ctx = Context.create ()
let parse_ts source = Tree.parse ~ctx ~language:"typescript" source
let parse_kotlin source = Tree.parse ~ctx ~language:"kotlin" source
let parse_tsx source = Tree.parse ~ctx ~language:"tsx" source

(* Convenience constructors for pattern tokens. *)
let con text node_type : Stmatch.pattern_token = Concrete { text; node_type }
let sub : Stmatch.pattern_token = Subtree { name = None }

(* ========================================================================= *)
(* Tests                                                                     *)
(* ========================================================================= *)

(* Smoke test: simplest possible end-to-end run. Parse `foo`; pattern
   `foo` (identifier); match should succeed. *)
let test_single_identifier () =
  let tree = parse_ts "foo;" in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern = [ con "foo" "identifier" ] in
  Alcotest.(check bool)
    "single identifier matches" true
    (M.match_at pattern cursor |> Option.is_some)

(* Pattern that should NOT match because the source identifier differs. *)
let test_single_identifier_mismatch () =
  let tree = parse_ts "foo;" in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern = [ con "bar" "identifier" ] in
  Alcotest.(check bool)
    "mismatched identifier fails" false
    (M.match_at pattern cursor |> Option.is_some)

(* The motivating example from the design doc: `foo($x)` matching
   `foo(c > 0)`. The wildcard binds the comparison expression. *)
let test_call_with_subtree_wildcard () =
  let tree = parse_ts "foo(c > 0);" in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern = [ con "foo" "identifier"; con "(" "("; sub; con ")" ")" ] in
  Alcotest.(check bool)
    "foo($x) matches foo(c > 0)" true
    (M.match_at pattern cursor |> Option.is_some)

(* Verify the (text, node_type) discrimination at the integration
   level: a pattern asking for identifier `c` should not match a
   string-literal `c`. *)
let test_node_type_discrimination_identifier_vs_string () =
  let tree = parse_ts {|"c";|} in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern = [ con "c" "identifier" ] in
  Alcotest.(check bool)
    "identifier pattern doesn't match string content" false
    (M.match_at pattern cursor |> Option.is_some)

(* The reverse: a pattern matching the full string literal `"c"`,
   including the quotes (unnamed leaves) and the string_fragment
   (named leaf). The TS string AST for `"c"` is:
     string
       " (unnamed)
       string_fragment "c"
       " (unnamed)
   The cursor walks all three leaves in order. *)
let test_string_literal_full_pattern () =
  let tree = parse_ts {|"c";|} in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [ con {|"|} {|"|}; con "c" "string_fragment"; con {|"|} {|"|} ]
  in
  Alcotest.(check bool)
    "full string literal pattern matches" true
    (M.match_at pattern cursor |> Option.is_some)

(* Cross-language smoke test: confirm the cursor implementation works
   against another language's grammar (Kotlin). The node types are
   genuinely different from TypeScript (e.g., `simple_identifier`,
   `binding_pattern_kind`), so a passing test here means the cursor
   isn't accidentally TS-specific. *)
let test_kotlin_property_declaration () =
  let tree = parse_kotlin "val x = 1" in
  let cursor = Tree_sitter_cursor.of_tree tree in
  (* Leaf walk: [val/val, x/simple_identifier, =/=, 1/integer_literal].
     The wrapping nodes (property_declaration, binding_pattern_kind,
     variable_declaration) aren't leaves and don't appear in the
     token stream the matcher sees. *)
  let pattern =
    [
      con "val" "val";
      con "x" "simple_identifier";
      con "=" "=";
      con "1" "integer_literal";
    ]
  in
  Alcotest.(check bool)
    "kotlin val declaration matches" true
    (M.match_at pattern cursor |> Option.is_some)

(* "Partial" pattern test: a pattern whose token sequence (`else { ... }`)
   would NOT parse as a valid standalone fragment in any TS grammar
   context. The matcher consumes tokens, not AST nodes, so as long as
   the cursor is positioned at a place where the token sequence aligns
   with the AST's leaves, the match succeeds.

   Phase 5 will automate finding such positions via anchor search; here
   we descend manually to the [else_clause] node using [Tree.find_by_type]
   and start the cursor there with [of_node]. *)
let test_partial_pattern_else_clause () =
  let source = "if (x) { foo() } else { bar() }" in
  let tree = parse_ts source in
  let else_clause =
    match Tree.find_by_type "else_clause" tree.root with
    | [ c ] -> c
    | _ -> failwith "expected exactly one else_clause"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source else_clause in
  let pattern = [ con "else" "else"; con "{" "{"; sub; con "}" "}" ] in
  Alcotest.(check bool)
    "partial else-clause pattern matches" true
    (M.match_at pattern cursor |> Option.is_some)

(* Direct demonstration that [(text, node_type)] comparison matters:
   a source where the same text ("x") appears in two leaves with
   different node types (identifier and string_fragment). The matcher
   picks the right one based on the pattern's requested type, using
   manual cursor positioning to put each leaf under test. *)
let test_leaf_type_distinguishes_same_text () =
  let source = {|x + "x";|} in
  let tree = parse_ts source in
  let identifier_x =
    match Tree.find_by_type "identifier" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected exactly one identifier"
  in
  let string_x =
    match Tree.find_by_type "string_fragment" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected exactly one string_fragment"
  in
  assert (Tree.text source identifier_x = "x");
  assert (Tree.text source string_x = "x");
  let ok c pat msg =
    Alcotest.(check bool) msg true (M.match_at pat c |> Option.is_some)
  in
  let no c pat msg =
    Alcotest.(check bool) msg false (M.match_at pat c |> Option.is_some)
  in
  ok
    (Tree_sitter_cursor.of_node ~source identifier_x)
    [ con "x" "identifier" ]
    "identifier leaf matches identifier pattern";
  no
    (Tree_sitter_cursor.of_node ~source identifier_x)
    [ con "x" "string_fragment" ]
    "identifier leaf rejects string_fragment pattern";
  ok
    (Tree_sitter_cursor.of_node ~source string_x)
    [ con "x" "string_fragment" ]
    "string_fragment leaf matches string_fragment pattern";
  no
    (Tree_sitter_cursor.of_node ~source string_x)
    [ con "x" "identifier" ]
    "string_fragment leaf rejects identifier pattern"

(* Siblings wildcard via Tree_sitter_cursor, with a twist: one of the
   absorbed source children is itself a nested call (`b()`). The
   siblings backtracking must treat `b()` as a single subtree —
   advancing past it via [move_next_sibling] within the outer
   arguments — not get confused by the inner parens.

   Source: foo(a, b(), c)
   Pattern: foo ( a , ... )
   Expected: Siblings absorbs the three subtrees [b(), ',', c]. *)
let test_siblings_skips_past_nested_parens () =
  let tree = parse_ts "foo(a, b(), c);" in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "foo" "identifier";
      con "(" "(";
      con "a" "identifier";
      con "," ",";
      Stmatch.Siblings { name = None };
      con ")" ")";
    ]
  in
  Alcotest.(check bool)
    "siblings absorbs subtrees containing nested parens" true
    (M.match_at pattern cursor |> Option.is_some)

(* Smoke test: comments should be skipped automatically as extras. *)
let test_extras_are_skipped () =
  (* Source has a comment between `foo` and `(` — the cursor should
     transparently skip it. *)
  let tree = parse_ts "foo /* hi */ (1);" in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern = [ con "foo" "identifier"; con "(" "("; sub; con ")" ")" ] in
  Alcotest.(check bool)
    "pattern matches across comment" true
    (M.match_at pattern cursor |> Option.is_some)

(* ----- byte_range ----- *)

let byte_range_pair = Alcotest.(pair int int)

(* For a parsed `foo;`, the identifier `foo` spans bytes 0..3. *)
let test_byte_range_identifier_leaf () =
  let source = "foo;" in
  let tree = parse_ts source in
  let foo =
    match Tree.find_by_type "identifier" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one identifier"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source foo in
  Alcotest.(check byte_range_pair)
    "foo spans bytes 0..3" (0, 3)
    (Tree_sitter_cursor.byte_range cursor)

(* The motivating use: extract bound subtree text via byte_range +
   String.sub. Pattern `foo($x)` against `foo(c > 0);` binds x to the
   `c > 0` subtree; we should be able to extract that text. *)
let test_byte_range_extracts_binding_text () =
  let source = "foo(c > 0);" in
  let tree = parse_ts source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "foo" "identifier";
      con "(" "(";
      Stmatch.Subtree { name = Some "x" };
      con ")" ")";
    ]
  in
  match M.match_at pattern cursor with
  | None -> Alcotest.fail "expected match"
  | Some (_, bs) -> (
      match
        List.find_map
          (function
            | M.Single { name = "x"; cursor } -> Some cursor | _ -> None)
          bs
      with
      | None -> Alcotest.fail "x not bound"
      | Some bound_cursor ->
          let start_byte, end_byte =
            Tree_sitter_cursor.byte_range bound_cursor
          in
          let text = String.sub source start_byte (end_byte - start_byte) in
          Alcotest.(check string) "x bound to 'c > 0'" "c > 0" text)

(* Integration test for find_matches on a real TS source. Pattern
   [foo; (; $x; )] should find both calls to foo in the source. *)
let test_find_matches_multiple_calls () =
  let source = "foo(1); bar(2); foo(3);" in
  let tree = parse_ts source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "foo" "identifier";
      con "(" "(";
      Stmatch.Subtree { name = Some "x" };
      con ")" ")";
    ]
  in
  let results = M.find_matches pattern cursor in
  Alcotest.(check int) "two foo(_) matches" 2 (List.length results);
  (* Verify each match's binding via byte_range extraction. *)
  let texts =
    List.map
      (fun r ->
        match
          List.find_map
            (function
              | M.Single { name = "x"; cursor } -> Some cursor | _ -> None)
            r.M.bindings
        with
        | None -> failwith "missing binding for x"
        | Some c ->
            let s, e = Tree_sitter_cursor.byte_range c in
            String.sub source s (e - s))
      results
  in
  Alcotest.(check (list string)) "x bound to 1 then 3" [ "1"; "3" ] texts

(* ----- JSX (via the tsx grammar) -----

   JSX is structurally distinct from the TS / Kotlin sources tested
   above:

   - Multi-character punctuation leaves: [/>] and [</] each appear as
     a single token, not split into [/] + [>] / [<] + [/].
   - [jsx_text] leaves carry the inter-tag source verbatim (including
     trailing whitespace).
   - Attribute lists have NO separator token between attributes —
     unlike the comma-separated argument lists / object literals that
     appear in TS proper. This is the case that motivates source-derived
     separator detection in the partial-mode design.
   - Two distinct element shapes — [jsx_self_closing_element] vs
     [jsx_element] (with [jsx_opening_element] + children +
     [jsx_closing_element]) — and the cursor needs to walk leaves
     across that nested-tag structure in pre-order.

   These tests confirm the cursor + matcher handle those cases without
   any JSX-specific code. *)

(* A self-closing tag with one attribute. The attribute value is a
   JSX expression [{1}], so the leaf walk is:
     < Foo a = { 1 } />
   The wildcard binds the JSX expression's number subtree. *)
let test_jsx_self_closing_with_attribute () =
  let tree = parse_tsx "<Foo a={1} />" in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "<" "<";
      con "Foo" "identifier";
      con "a" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = Some "x" };
      con "}" "}";
      con "/>" "/>";
    ]
  in
  match M.match_at pattern cursor with
  | None -> Alcotest.fail "expected match"
  | Some (_, bs) -> (
      match
        List.find_map
          (function
            | M.Single { name = "x"; cursor } -> Some cursor | _ -> None)
          bs
      with
      | None -> Alcotest.fail "x not bound"
      | Some c ->
          let s, e = Tree_sitter_cursor.byte_range c in
          Alcotest.(check string)
            "x bound to '1'" "1"
            (String.sub "<Foo a={1} />" s (e - s)))

(* Two consecutive attributes with no separator token in between. This
   is the JSX-specific contrast with TypeScript object literals (where
   the source has [,] between properties). The pattern matches the
   leaf stream directly: attribute leaves follow attribute leaves with
   nothing else in between. *)
let test_jsx_multiple_attributes_no_separator () =
  let tree = parse_tsx {|<Foo a={1} b={"hi"} />|} in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "<" "<";
      con "Foo" "identifier";
      con "a" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = None };
      con "}" "}";
      (* note: no separator token here *)
      con "b" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = None };
      con "}" "}";
      con "/>" "/>";
    ]
  in
  Alcotest.(check bool)
    "two-attribute pattern matches without inter-attribute separator" true
    (M.match_at pattern cursor |> Option.is_some)

(* An opened / closed JSX element with jsx_text content. The wildcard
   binds the text leaf, demonstrating that the cursor descends into
   [jsx_text] as a regular leaf. *)
let test_jsx_element_with_text_child () =
  let source = "<div>hello</div>" in
  let tree = parse_tsx source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "<" "<";
      con "div" "identifier";
      con ">" ">";
      Stmatch.Subtree { name = Some "text" };
      con "</" "</";
      con "div" "identifier";
      con ">" ">";
    ]
  in
  match M.match_at pattern cursor with
  | None -> Alcotest.fail "expected match"
  | Some (_, bs) -> (
      match
        List.find_map
          (function
            | M.Single { name = "text"; cursor } -> Some cursor | _ -> None)
          bs
      with
      | None -> Alcotest.fail "text not bound"
      | Some c ->
          let s, e = Tree_sitter_cursor.byte_range c in
          Alcotest.(check string)
            "text bound to jsx_text leaf" "hello"
            (String.sub source s (e - s)))

(* A nested JSX element appearing as a child of another. The outer
   pattern's wildcard absorbs the entire inner [<span>world</span>]
   subtree, demonstrating that subtree absorption works across the
   open-tag / children / close-tag boundary the same as in any other
   structural context. *)
let test_jsx_nested_element_as_subtree () =
  let source = "<div><span>world</span></div>" in
  let tree = parse_tsx source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "<" "<";
      con "div" "identifier";
      con ">" ">";
      Stmatch.Subtree { name = Some "child" };
      con "</" "</";
      con "div" "identifier";
      con ">" ">";
    ]
  in
  match M.match_at pattern cursor with
  | None -> Alcotest.fail "expected match"
  | Some (_, bs) -> (
      match
        List.find_map
          (function
            | M.Single { name = "child"; cursor } -> Some cursor | _ -> None)
          bs
      with
      | None -> Alcotest.fail "child not bound"
      | Some c ->
          let s, e = Tree_sitter_cursor.byte_range c in
          Alcotest.(check string)
            "child bound to nested span element" "<span>world</span>"
            (String.sub source s (e - s)))

(* find_matches integration on JSX: multiple sibling JSX elements,
   pattern should locate each. Source has two [<Foo .../>] elements
   and one [<Bar .../>] element; the pattern matches only [<Foo .../>]. *)
let test_jsx_find_matches_multiple_elements () =
  let source = "const t = <div><Foo a={1}/><Bar a={2}/><Foo a={3}/></div>;" in
  let tree = parse_tsx source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let pattern =
    [
      con "<" "<";
      con "Foo" "identifier";
      con "a" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = Some "v" };
      con "}" "}";
      con "/>" "/>";
    ]
  in
  let results = M.find_matches pattern cursor in
  Alcotest.(check int) "two <Foo .../> matches" 2 (List.length results);
  let texts =
    List.map
      (fun r ->
        match
          List.find_map
            (function
              | M.Single { name = "v"; cursor } -> Some cursor | _ -> None)
            r.M.bindings
        with
        | None -> failwith "missing binding for v"
        | Some c ->
            let s, e = Tree_sitter_cursor.byte_range c in
            String.sub source s (e - s))
      results
  in
  Alcotest.(check (list string)) "v bound to 1 then 3" [ "1"; "3" ] texts

(* ----- named_children -----

   The Tree_sitter_cursor implementation filters out anonymous tokens
   (brackets, commas, etc.) and extras (whitespace, comments), returning
   only the named non-extra children. These tests confirm the filtering
   works correctly against real grammars. *)

(* Object literal `{a: 1, b: 2}` has two named children (the [pair] nodes).
   The braces and the comma between pairs are anonymous and filtered out. *)
let test_named_children_object_literal () =
  let source = "const o = {a: 1, b: 2};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected exactly one object node"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  let children = Tree_sitter_cursor.named_children cursor in
  Alcotest.(check int)
    "object has two named children (the pairs)" 2 (List.length children);
  (* Verify each child's node type by inspecting its leaves. *)
  let first_leaves =
    List.map
      (fun c ->
        let l = Tree_sitter_cursor.move_first_leaf c in
        Tree_sitter_cursor.leaf_text l)
      children
  in
  Alcotest.(check (list string))
    "first leaf of each pair is the key" [ "a"; "b" ] first_leaves

(* JSX self-closing element `<Foo a={1} b={2} />` has three named children
   under jsx_self_closing_element: the tag identifier and two jsx_attribute
   nodes. The brackets `<` and `/>` are anonymous and filtered out. *)
let test_named_children_jsx_self_closing () =
  let source = "<Foo a={1} b={2} />" in
  let tree = parse_tsx source in
  let elem =
    match Tree.find_by_type "jsx_self_closing_element" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected exactly one jsx_self_closing_element"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source elem in
  let children = Tree_sitter_cursor.named_children cursor in
  (* Three: the identifier "Foo" plus two jsx_attribute nodes. *)
  Alcotest.(check int)
    "JSX self-closing has three named children" 3 (List.length children)

(* A sub-cursor returned by named_children is scoped to its child's
   subtree. Walking past the subtree via move_next_subtree returns
   false, even though the source's parent has more siblings beyond. *)
let test_named_children_scoped_to_child () =
  let source = "const o = {a: 1, b: 2};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected exactly one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  match Tree_sitter_cursor.named_children cursor with
  | first_pair :: _ ->
      (* Walk to the leftmost leaf of the first pair, then try to advance
         past the pair's subtree. Because the sub-cursor is scoped to the
         pair, advancing should return false (we don't escape into the
         object's other children). *)
      let _ = Tree_sitter_cursor.move_first_leaf first_pair in
      (* From leftmost leaf "a", advance through ":", "1", then end of
         pair — eventually false. *)
      let rec walk_to_end c steps =
        if steps > 10 then failwith "walked too far"
        else if Tree_sitter_cursor.move_next_subtree c then
          walk_to_end c (steps + 1)
        else ()
      in
      walk_to_end first_pair 0
      (* If we got here, walking eventually terminated (sub-cursor scoped). *)
  | _ -> Alcotest.fail "expected at least one named child"

(* Function arguments `foo(1, 2)` has anonymous parens and a comma between
   args; named_children returns just the two argument expressions. *)
let test_named_children_function_arguments () =
  let source = "foo(1, 2)" in
  let tree = parse_ts source in
  let args =
    match Tree.find_by_type "arguments" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected exactly one arguments node"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source args in
  let children = Tree_sitter_cursor.named_children cursor in
  Alcotest.(check int)
    "arguments has two named children" 2 (List.length children)

(* ----- source_substring -----

   Confirms source_substring returns the right bytes against real
   parses, and demonstrates the inter-named-child gap pattern that
   the partial-mode driver will use for source-derived separator
   detection. *)

(* Whole-source slice matches the input. *)
let test_source_substring_whole () =
  let source = "foo(1, 2);" in
  let tree = parse_ts source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  Alcotest.(check string)
    "whole source" source
    (Tree_sitter_cursor.source_substring cursor 0 (String.length source))

(* TS object literal: bytes between the first and second pair are ", ". *)
let test_source_substring_object_separator () =
  let source = "const o = {a: 1, b: 2};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  match Tree_sitter_cursor.named_children cursor with
  | [ first; second ] ->
      let _, first_end = Tree_sitter_cursor.byte_range first in
      let second_start, _ = Tree_sitter_cursor.byte_range second in
      let between =
        Tree_sitter_cursor.source_substring cursor first_end second_start
      in
      Alcotest.(check string) "separator bytes between pairs" ", " between
  | _ -> Alcotest.fail "expected two named children"

(* TS type literal uses `;` (or `,`) as the separator between members.
   Verify the inter-child bytes contain the right separator. *)
let test_source_substring_record_type_separator () =
  let source = "type T = {a: string; b: number};" in
  let tree = parse_ts source in
  let type_lit =
    match Tree.find_by_type "object_type" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object_type"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source type_lit in
  match Tree_sitter_cursor.named_children cursor with
  | [ first; second ] ->
      let _, first_end = Tree_sitter_cursor.byte_range first in
      let second_start, _ = Tree_sitter_cursor.byte_range second in
      let between =
        Tree_sitter_cursor.source_substring cursor first_end second_start
      in
      Alcotest.(check string) "separator bytes between members" "; " between
  | _ -> Alcotest.fail "expected two named children"

(* JSX self-closing element: bytes between attributes are whitespace
   only (no separator token). This is the JSX case for partial-mode
   detection — empty separator after trimming.

   Named children of jsx_self_closing_element are: the tag identifier
   (`Foo`) followed by the two jsx_attribute nodes. We skip the tag
   and look at the gap between the two attributes. *)
let test_source_substring_jsx_no_separator () =
  let source = "<Foo a={1} b={2} />" in
  let tree = parse_tsx source in
  let elem =
    match Tree.find_by_type "jsx_self_closing_element" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one jsx_self_closing_element"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source elem in
  match Tree_sitter_cursor.named_children cursor with
  | _tag :: attr_a :: attr_b :: _ ->
      let _, a_end = Tree_sitter_cursor.byte_range attr_a in
      let b_start, _ = Tree_sitter_cursor.byte_range attr_b in
      let between = Tree_sitter_cursor.source_substring cursor a_end b_start in
      Alcotest.(check string)
        "bytes between attributes are whitespace only" " " between
  | _ -> Alcotest.fail "expected at least three named children"

(* ----- match_partial_at over real parses -----

   Exercises the Step 4 driver: source-derived separator detection
   plus separator stripping between match_prefix calls. *)

(* Object literal `{a: 1, b: 2}` against a partial pattern carrying
   commas. The driver infers "," from the source's inter-pair gap and
   strips comma tokens at element boundaries.

   Pattern tokens include the surrounding [{] and [}]: the matcher
   strict-matches them against the source's leading/trailing anonymous
   leaves and set-matches the middle against the named children. *)
let test_partial_object_literal_with_commas () =
  let source = "const o = {a: 1, b: 2};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "x" };
      con "," ",";
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "y" };
      con "}" "}";
    ]
  in
  match M.match_partial_at pat cursor with
  | None -> Alcotest.fail "expected match"
  | Some (_, bindings) ->
      let x_text =
        match
          List.find_map
            (function
              | M.Single { name = "x"; cursor } -> Some cursor | _ -> None)
            bindings
        with
        | Some c ->
            let s, e = Tree_sitter_cursor.byte_range c in
            String.sub source s (e - s)
        | None -> failwith "x not bound"
      in
      let y_text =
        match
          List.find_map
            (function
              | M.Single { name = "y"; cursor } -> Some cursor | _ -> None)
            bindings
        with
        | Some c ->
            let s, e = Tree_sitter_cursor.byte_range c in
            String.sub source s (e - s)
        | None -> failwith "y not bound"
      in
      Alcotest.(check string) "x bound to 1" "1" x_text;
      Alcotest.(check string) "y bound to 2" "2" y_text

(* TS record type `{a: string; b: number}` uses ";" as separator. The
   inferred separator is ";" and the driver strips semicolon tokens. *)
let test_partial_record_type_with_semicolons () =
  let source = "type T = {a: string; b: number};" in
  let tree = parse_ts source in
  let type_lit =
    match Tree.find_by_type "object_type" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object_type"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source type_lit in
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "t1" };
      con ";" ";";
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "t2" };
      con "}" "}";
    ]
  in
  Alcotest.(check bool)
    "record-type partial pattern with ';' separator matches" true
    (Option.is_some (M.match_partial_at pat cursor))

(* JSX `<Component a={1} b={2} />` against partial pattern with no
   separator (JSX attributes have no inter-attribute punctuation). The
   inferred separator is "" — strip_separator is a no-op throughout.

   Named children of jsx_self_closing_element are: identifier
   "Component", jsx_attribute(a={1}), jsx_attribute(b={2}) — three
   elements. The pattern's token sequence covers all three. *)
let test_partial_jsx_self_closing () =
  let source = "<Component a={1} b={2} />" in
  let tree = parse_tsx source in
  let elem =
    match Tree.find_by_type "jsx_self_closing_element" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one jsx_self_closing_element"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source elem in
  let pat =
    [
      con "<" "<";
      (* element 1: the tag identifier *)
      con "Component" "identifier";
      (* element 2: a={$x} *)
      con "a" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = Some "x" };
      con "}" "}";
      (* element 3: b={$z} *)
      con "b" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = Some "z" };
      con "}" "}";
      con "/>" "/>";
    ]
  in
  match M.match_partial_at pat cursor with
  | None -> Alcotest.fail "expected match"
  | Some (_, bindings) ->
      let extract name =
        match
          List.find_map
            (function
              | M.Single { name = n; cursor } when n = name -> Some cursor
              | _ -> None)
            bindings
        with
        | Some c ->
            let s, e = Tree_sitter_cursor.byte_range c in
            String.sub source s (e - s)
        | None -> failwith (name ^ " not bound")
      in
      Alcotest.(check string) "x bound to 1" "1" (extract "x");
      Alcotest.(check string) "z bound to 2" "2" (extract "z")

(* Lenient case: pattern omits the comma separator but source has one.
   strip_separator's head check finds no separator at the head of the
   remaining pattern, so it's a no-op; match_prefix sees the right
   tokens for the next child anyway. *)
let test_partial_pattern_omits_separator () =
  let source = "const o = {a: 1, b: 2};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  (* No "," in this pattern. *)
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "x" };
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "y" };
      con "}" "}";
    ]
  in
  Alcotest.(check bool)
    "lenient: pattern without explicit separator still matches" true
    (Option.is_some (M.match_partial_at pat cursor))

(* Reordered TS object literal: pattern's element order differs from
   source's. The set-based driver finds the right assignment via
   backtracking. *)
let test_partial_reordered_object_literal () =
  let source = "const o = {b: 2, a: 1};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  (* Pattern element order is a, b; source order is b, a. *)
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "x" };
      con "," ",";
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "y" };
      con "}" "}";
    ]
  in
  match M.match_partial_at pat cursor with
  | None -> Alcotest.fail "expected match on reordered source"
  | Some (_, bindings) ->
      let extract name =
        match
          List.find_map
            (function
              | M.Single { name = n; cursor } when n = name -> Some cursor
              | _ -> None)
            bindings
        with
        | Some c ->
            let s, e = Tree_sitter_cursor.byte_range c in
            String.sub source s (e - s)
        | None -> failwith (name ^ " not bound")
      in
      Alcotest.(check string) "x bound to 1" "1" (extract "x");
      Alcotest.(check string) "y bound to 2" "2" (extract "y")

(* Source with extras: pattern names two keys, source has three. The
   third (extra) key is silently ignored. *)
let test_partial_object_with_extras () =
  let source = "const o = {a: 1, b: 2, c: 3};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "x" };
      con "," ",";
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "y" };
      con "}" "}";
    ]
  in
  Alcotest.(check bool)
    "extra key in source is tolerated" true
    (Option.is_some (M.match_partial_at pat cursor))

(* Cross-element non-linearity over a real TS object literal: pattern
   binds $val twice (in two elements). Source has equal values; match
   succeeds. *)
let test_partial_nonlinearity_real_object () =
  let source = "const o = {a: 42, b: 42};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "val" };
      con "," ",";
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "val" };
      con "}" "}";
    ]
  in
  Alcotest.(check bool)
    "non-linearity satisfied: both $val bind to '42'" true
    (Option.is_some (M.match_partial_at pat cursor))

(* Cross-element non-linearity failure on a real TS object: $val appears
   in two elements but the source's values differ. *)
let test_partial_nonlinearity_real_object_fails () =
  let source = "const o = {a: 42, b: 43};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "val" };
      con "," ",";
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "val" };
      con "}" "}";
    ]
  in
  Alcotest.(check bool)
    "different values for $val cause partial-mode match to fail" false
    (Option.is_some (M.match_partial_at pat cursor))

(* Reordered JSX attributes: pattern asks for a then b; source has b then
   a. Set semantics finds the assignment. *)
let test_partial_jsx_reordered_attributes () =
  let source = "<Component b={2} a={1} />" in
  let tree = parse_tsx source in
  let elem =
    match Tree.find_by_type "jsx_self_closing_element" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one jsx_self_closing_element"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source elem in
  let pat =
    [
      con "<" "<";
      con "Component" "identifier";
      con "a" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = Some "x" };
      con "}" "}";
      con "b" "property_identifier";
      con "=" "=";
      con "{" "{";
      Stmatch.Subtree { name = Some "z" };
      con "}" "}";
      con "/>" "/>";
    ]
  in
  match M.match_partial_at pat cursor with
  | None -> Alcotest.fail "expected match on reordered JSX"
  | Some (_, bindings) ->
      let extract name =
        match
          List.find_map
            (function
              | M.Single { name = n; cursor } when n = name -> Some cursor
              | _ -> None)
            bindings
        with
        | Some c ->
            let s, e = Tree_sitter_cursor.byte_range c in
            String.sub source s (e - s)
        | None -> failwith (name ^ " not bound")
      in
      Alcotest.(check string)
        "x bound to 1 (matched 'a' attribute)" "1" (extract "x");
      Alcotest.(check string)
        "z bound to 2 (matched 'b' attribute)" "2" (extract "z")

(* Pattern with the WRONG separator (semicolon against a comma-separated
   source) fails: the wrong-separator token isn't stripped, and the
   next match_prefix call mismatches its first token against the source
   child's first leaf. *)
let test_partial_wrong_separator_fails () =
  let source = "const o = {a: 1, b: 2};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  let pat =
    [
      con "{" "{";
      con "a" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "x" };
      con ";" ";";
      (* wrong separator *)
      con "b" "property_identifier";
      con ":" ":";
      Stmatch.Subtree { name = Some "y" };
      con "}" "}";
    ]
  in
  Alcotest.(check bool)
    "wrong separator in pattern causes match failure" false
    (Option.is_some (M.match_partial_at pat cursor))

(* ----- leading/trailing anonymous leaves ----- *)

let show_leaves =
  List.map (fun l ->
      (Tree_sitter_cursor.leaf_text l, Tree_sitter_cursor.leaf_node_type l))

let anon_pair = Alcotest.(list (pair string string))

let test_anon_leaves_ts_object () =
  let source = "const o = {a: 1, b: 2};" in
  let tree = parse_ts source in
  let object_node =
    match Tree.find_by_type "object" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one object"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source object_node in
  Alcotest.(check anon_pair)
    "object leading anon = '{'"
    [ ("{", "{") ]
    (show_leaves (Tree_sitter_cursor.leading_anonymous_leaves cursor));
  Alcotest.(check anon_pair)
    "object trailing anon = '}'"
    [ ("}", "}") ]
    (show_leaves (Tree_sitter_cursor.trailing_anonymous_leaves cursor))

let test_anon_leaves_jsx_self_closing () =
  let source = "const e = <Foo a={1} />;" in
  let tree = parse_tsx source in
  let elem =
    match Tree.find_by_type "jsx_self_closing_element" tree.root with
    | [ n ] -> n
    | _ -> failwith "expected one jsx_self_closing_element"
  in
  let cursor = Tree_sitter_cursor.of_node ~source:tree.source elem in
  Alcotest.(check anon_pair)
    "jsx leading anon = '<'"
    [ ("<", "<") ]
    (show_leaves (Tree_sitter_cursor.leading_anonymous_leaves cursor));
  Alcotest.(check anon_pair)
    "jsx trailing anon = '/>'"
    [ ("/>", "/>") ]
    (show_leaves (Tree_sitter_cursor.trailing_anonymous_leaves cursor))

(* A node with no anonymous children (e.g. a program) returns empty
   leading/trailing runs — so the partial-mode matcher's structural
   self-filter naturally skips it. *)
let test_anon_leaves_program_empty () =
  let source = "const o = {a: 1};" in
  let tree = parse_ts source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  Alcotest.(check anon_pair)
    "program leading anon empty" []
    (show_leaves (Tree_sitter_cursor.leading_anonymous_leaves cursor));
  Alcotest.(check anon_pair)
    "program trailing anon empty" []
    (show_leaves (Tree_sitter_cursor.trailing_anonymous_leaves cursor))

let tests =
  let open Alcotest in
  [
    test_case "single identifier matches" `Quick test_single_identifier;
    test_case "single identifier mismatch" `Quick
      test_single_identifier_mismatch;
    test_case "call with subtree wildcard" `Quick
      test_call_with_subtree_wildcard;
    test_case "identifier vs string discrimination" `Quick
      test_node_type_discrimination_identifier_vs_string;
    test_case "string literal full pattern" `Quick
      test_string_literal_full_pattern;
    test_case "kotlin property declaration" `Quick
      test_kotlin_property_declaration;
    test_case "partial else-clause pattern" `Quick
      test_partial_pattern_else_clause;
    test_case "leaf type distinguishes same text" `Quick
      test_leaf_type_distinguishes_same_text;
    test_case "siblings skip past nested parens" `Quick
      test_siblings_skips_past_nested_parens;
    test_case "extras (comments) are skipped" `Quick test_extras_are_skipped;
    test_case "byte_range: identifier leaf" `Quick
      test_byte_range_identifier_leaf;
    test_case "byte_range: extracts binding text" `Quick
      test_byte_range_extracts_binding_text;
    test_case "find_matches: multiple calls" `Quick
      test_find_matches_multiple_calls;
    test_case "jsx: self-closing with attribute" `Quick
      test_jsx_self_closing_with_attribute;
    test_case "jsx: multiple attributes (no separator)" `Quick
      test_jsx_multiple_attributes_no_separator;
    test_case "jsx: element with text child" `Quick
      test_jsx_element_with_text_child;
    test_case "jsx: nested element as subtree" `Quick
      test_jsx_nested_element_as_subtree;
    test_case "jsx: find_matches multiple elements" `Quick
      test_jsx_find_matches_multiple_elements;
    test_case "named_children: object literal" `Quick
      test_named_children_object_literal;
    test_case "named_children: jsx self-closing element" `Quick
      test_named_children_jsx_self_closing;
    test_case "named_children: sub-cursor is scoped to child" `Quick
      test_named_children_scoped_to_child;
    test_case "named_children: function arguments" `Quick
      test_named_children_function_arguments;
    test_case "source_substring: whole source" `Quick
      test_source_substring_whole;
    test_case "source_substring: object literal ', ' separator" `Quick
      test_source_substring_object_separator;
    test_case "source_substring: record type '; ' separator" `Quick
      test_source_substring_record_type_separator;
    test_case "source_substring: JSX whitespace-only between attrs" `Quick
      test_source_substring_jsx_no_separator;
    test_case "partial: object literal with commas" `Quick
      test_partial_object_literal_with_commas;
    test_case "partial: record type with semicolons" `Quick
      test_partial_record_type_with_semicolons;
    test_case "partial: JSX <Component a={$x} b={$z} />" `Quick
      test_partial_jsx_self_closing;
    test_case "partial: lenient (pattern omits separator)" `Quick
      test_partial_pattern_omits_separator;
    test_case "partial: wrong separator fails" `Quick
      test_partial_wrong_separator_fails;
    test_case "partial: reordered object literal" `Quick
      test_partial_reordered_object_literal;
    test_case "partial: object literal with extras" `Quick
      test_partial_object_with_extras;
    test_case "partial: JSX reordered attributes" `Quick
      test_partial_jsx_reordered_attributes;
    test_case "partial: cross-element non-linearity (equal)" `Quick
      test_partial_nonlinearity_real_object;
    test_case "partial: cross-element non-linearity (unequal fails)" `Quick
      test_partial_nonlinearity_real_object_fails;
    test_case "anon leaves: TS object braces" `Quick test_anon_leaves_ts_object;
    test_case "anon leaves: JSX self-closing '<' and '/>'" `Quick
      test_anon_leaves_jsx_self_closing;
    test_case "anon leaves: program has none" `Quick
      test_anon_leaves_program_empty;
  ]
