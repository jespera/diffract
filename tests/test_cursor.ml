(** Test cursor implementation + navigation tests.

    [Test_cursor] is a {!Diffract.Cursor.S} over an immutable in-memory tree
    with a mutable zipper-style cursor for navigation. The tree is built with
    the [leaf] / [node] DSL; the cursor mutates a path record but the underlying
    tree is shared. *)

open Diffract

(* ========================================================================= *)
(* Tree representation                                                       *)
(* ========================================================================= *)

type tree = {
  node_type : string;
  text : string; (* meaningful only for leaves *)
  children : tree list;
}
(** A simple n-ary tree. Each node has a node type; leaves additionally carry
    text. Internal nodes' [text] field is unused. *)

let leaf node_type text = { node_type; text; children = [] }
let node node_type children = { node_type; text = ""; children }
let is_leaf t = t.children = []

(* ========================================================================= *)
(* Test cursor                                                               *)
(* ========================================================================= *)

(** The cursor's mutable state is a "stack of frames": each frame records a tree
    node and the remaining right siblings to visit after we finish with the
    current node. The top of the stack is the current position. *)
module Test_cursor : sig
  include Cursor.S

  val of_tree : tree -> t
end = struct
  type leaf = { leaf_text : string; leaf_node_type : string }

  (* A frame represents a node we've descended into, plus the right
     siblings that follow it (to be visited after the entire subtree
     rooted at the current node is done). *)
  type frame = { mutable curr : tree; mutable right : tree list }

  (* [absolute_root] is the original tree this cursor was constructed from;
     it stays fixed under cloning and sub-cursor creation, so that
     [byte_range] and [source_substring] always report positions in the
     same coordinate system as the cursor was initially built in. *)
  type t = { mutable stack : frame list; absolute_root : tree }

  let of_tree t =
    { stack = [ { curr = t; right = [] } ]; absolute_root = t }

  let clone c =
    {
      stack = List.map (fun f -> { curr = f.curr; right = f.right }) c.stack;
      absolute_root = c.absolute_root;
    }

  (* See [Cursor.S.narrow]: reset the stack so the current node is the
     navigation root and [move_next_subtree] can't climb above it. *)
  let narrow c =
    match c.stack with
    | [] -> failwith "Test_cursor: empty stack (logic error)"
    | f :: _ ->
        { stack = [ { curr = f.curr; right = [] } ];
          absolute_root = c.absolute_root }

  let current_node c =
    match c.stack with
    | [] -> failwith "Test_cursor: empty stack (logic error)"
    | f :: _ -> f.curr

  let rec move_first_leaf c =
    let n = current_node c in
    if is_leaf n then { leaf_text = n.text; leaf_node_type = n.node_type }
    else
      begin match n.children with
      | [] -> failwith "Test_cursor: unreachable (is_leaf checked)"
      | first_child :: rest_children ->
          (* Descend: push a new frame for the first child, with the
             remaining children as its "right" siblings. *)
          c.stack <- { curr = first_child; right = rest_children } :: c.stack;
          move_first_leaf c
      end

  let move_first_child c =
    let n = current_node c in
    match n.children with
    | [] -> false
    | first_child :: rest_children ->
        c.stack <- { curr = first_child; right = rest_children } :: c.stack;
        true

  let move_next_sibling c =
    match c.stack with
    | [] -> false
    | f :: _ -> (
        match f.right with
        | [] -> false
        | next :: rest ->
            f.curr <- next;
            f.right <- rest;
            true)

  let rec move_next_subtree c =
    if move_next_sibling c then true
    else
      match c.stack with
      | [] | [ _ ] -> false (* at root or empty: no more subtrees *)
      | _ :: parent_rest ->
          c.stack <- parent_rest;
          move_next_subtree c

  let leaf_text l = l.leaf_text
  let leaf_node_type l = l.leaf_node_type

  (* Structural equality on the current node via OCaml's polymorphic [=].
     Safe here because the test tree type has no mutable fields. *)
  let subtree_equal c1 c2 = current_node c1 = current_node c2

  (* Test trees don't model the named/anonymous distinction, so every node
     is treated as named (see [Cursor.S.is_named]). *)
  let is_named _ = true

  (* Synthetic byte range: pretend the tree was rendered by concatenating
     leaf text in pre-order. Walk [absolute_root] in pre-order, counting
     bytes; when we encounter the current node (identified by physical
     equality), record its byte span.

     Walking [absolute_root] (rather than the bottom of the stack) means
     sub-cursors created from a parent — e.g., via [named_children] —
     report positions in the parent's coordinate system, matching the
     semantics of [Tree_sitter_cursor] (where every node carries an
     absolute byte offset from the original parse). *)
  let byte_range c =
    let target = current_node c in
    let pos = ref 0 in
    let result = ref None in
    let rec walk node =
      let in_target = node == target in
      let start = !pos in
      if node.children = [] then pos := !pos + String.length node.text
      else List.iter walk node.children;
      if in_target then result := Some (start, !pos)
    in
    walk c.absolute_root;
    match !result with
    | Some r -> r
    | None -> failwith "Test_cursor.byte_range: target node not found in tree"

  (* The test tree doesn't model the named-vs-anonymous distinction
     (no [is_named] flag), so every child is returned. Test authors
     constructing trees for partial-mode tests are responsible for
     structuring children appropriately — i.e. pair-like nodes as
     direct children of a container node, with no separate punctuation
     leaves at the container level.

     Sub-cursors preserve the parent's [absolute_root] so that their
     [byte_range] and [source_substring] report positions in the
     parent's coordinate system, not relative to the child. The stack
     starts at the child with no right-siblings, so navigation can't
     escape the child's subtree. *)
  let named_children c =
    let n = current_node c in
    List.map
      (fun child ->
        {
          stack = [ { curr = child; right = [] } ];
          absolute_root = c.absolute_root;
        })
      n.children

  (* No named/anonymous distinction in test trees, so [all_children] is the
     same as [named_children]. *)
  let all_children = named_children

  (* Test fixtures don't model the named-vs-anonymous distinction, so there
     are no anonymous delimiter runs to expose. Returning [[]] for both
     means [match_partial_at] on a Test_cursor consumes its full pattern
     against the children's set — which is exactly the bracket-less
     semantics every existing hand-built partial-mode test relies on. *)
  let leading_anonymous_leaves _ = []
  let trailing_anonymous_leaves _ = []

  (* The test tree has no explicit source string. We synthesize one
     on demand by walking [absolute_root] in pre-order and concatenating
     leaf text — the same convention used by [byte_range]. *)
  let source_substring c start_byte end_byte =
    let buf = Buffer.create 16 in
    let rec walk node =
      if node.children = [] then Buffer.add_string buf node.text
      else List.iter walk node.children
    in
    walk c.absolute_root;
    String.sub (Buffer.contents buf) start_byte (end_byte - start_byte)
end

(* ========================================================================= *)
(* Convenience: shorthand tree builders                                      *)
(* ========================================================================= *)

(* For terse tests, single-letter labels: identifier-typed leaves with
   their letter as text, internal nodes typed "n" with children. *)
let id text = leaf "identifier" text
let nd children = node "n" children

(* ========================================================================= *)
(* Tests                                                                     *)
(* ========================================================================= *)

let with_cursor t f =
  let c = Test_cursor.of_tree t in
  f c

let test_first_leaf_on_leaf () =
  with_cursor (id "a") @@ fun c ->
  let l = Test_cursor.move_first_leaf c in
  Alcotest.(check string) "leaf text" "a" (Test_cursor.leaf_text l);
  Alcotest.(check string)
    "leaf type" "identifier"
    (Test_cursor.leaf_node_type l)

let test_first_leaf_descends () =
  (* nd(a, b, c): first_leaf descends to a *)
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  let l = Test_cursor.move_first_leaf c in
  Alcotest.(check string) "leftmost leaf" "a" (Test_cursor.leaf_text l)

let test_first_leaf_descends_deep () =
  (* nd(nd(nd(a)), b): first_leaf descends through three levels *)
  with_cursor (nd [ nd [ nd [ id "a" ] ]; id "b" ]) @@ fun c ->
  let l = Test_cursor.move_first_leaf c in
  Alcotest.(check string) "deeply nested leftmost" "a" (Test_cursor.leaf_text l)

let test_first_child_on_leaf () =
  with_cursor (id "a") @@ fun c ->
  Alcotest.(check bool)
    "no children on leaf" false
    (Test_cursor.move_first_child c)

let test_first_child_descends () =
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  Alcotest.(check bool) "descends" true (Test_cursor.move_first_child c);
  let l = Test_cursor.move_first_leaf c in
  Alcotest.(check string) "now at a" "a" (Test_cursor.leaf_text l)

let test_next_sibling () =
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  (* at a *)
  Alcotest.(check bool) "to b" true (Test_cursor.move_next_sibling c);
  Alcotest.(check string)
    "now b" "b"
    (Test_cursor.leaf_text (Test_cursor.move_first_leaf c));
  Alcotest.(check bool) "to c" true (Test_cursor.move_next_sibling c);
  Alcotest.(check string)
    "now c" "c"
    (Test_cursor.leaf_text (Test_cursor.move_first_leaf c));
  Alcotest.(check bool)
    "no more siblings" false
    (Test_cursor.move_next_sibling c)

let test_next_subtree_within_parent () =
  (* nd(a, b): from a, next_subtree → b *)
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  (* at a *)
  Alcotest.(check bool) "advanced" true (Test_cursor.move_next_subtree c);
  let l = Test_cursor.move_first_leaf c in
  Alcotest.(check string) "now b" "b" (Test_cursor.leaf_text l)

let test_next_subtree_climbs () =
  (* nd(nd(a), b): from a, next_subtree climbs out of nd(a) to b *)
  with_cursor (nd [ nd [ id "a" ]; id "b" ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  (* at nd(a) *)
  assert (Test_cursor.move_first_child c);
  (* at a *)
  Alcotest.(check bool) "climbs to b" true (Test_cursor.move_next_subtree c);
  let l = Test_cursor.move_first_leaf c in
  Alcotest.(check string) "now b" "b" (Test_cursor.leaf_text l)

let test_next_subtree_at_root () =
  with_cursor (id "a") @@ fun c ->
  Alcotest.(check bool)
    "no more at root" false
    (Test_cursor.move_next_subtree c)

let test_next_subtree_exhausts () =
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  (* a *)
  assert (Test_cursor.move_next_subtree c);
  (* b *)
  Alcotest.(check bool) "exhausted" false (Test_cursor.move_next_subtree c)

let test_clone_independence () =
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  let c2 = Test_cursor.clone c in
  (* Mutate c, c2 should be unaffected *)
  assert (Test_cursor.move_first_child c);
  Alcotest.(check string)
    "c at a" "a"
    (Test_cursor.leaf_text (Test_cursor.move_first_leaf c));
  Alcotest.(check string)
    "c2 unchanged at root (descends to a)" "a"
    (Test_cursor.leaf_text (Test_cursor.move_first_leaf c2))

(* ----- byte_range ----- *)

let byte_range_pair = Alcotest.(pair int int)

(* Rendered as "abc" — bytes 0..3. *)
let test_byte_range_root () =
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  Alcotest.(check byte_range_pair) "root spans 0..3" (0, 3) (Test_cursor.byte_range c)

let test_byte_range_single_leaf_child () =
  (* Rendered "ab", cursor at leaf "a" → (0,1). *)
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  Alcotest.(check byte_range_pair) "leaf a spans 0..1" (0, 1) (Test_cursor.byte_range c)

let test_byte_range_second_sibling () =
  (* Rendered "ab", cursor at leaf "b" → (1,2). *)
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  assert (Test_cursor.move_next_sibling c);
  Alcotest.(check byte_range_pair) "leaf b spans 1..2" (1, 2) (Test_cursor.byte_range c)

let test_byte_range_multi_char_leaf () =
  (* leaf "abc" + leaf "de" → "abcde", first leaf spans (0,3). *)
  with_cursor (nd [ leaf "id" "abc"; leaf "id" "de" ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  Alcotest.(check byte_range_pair) "abc spans 0..3" (0, 3) (Test_cursor.byte_range c)

let test_byte_range_nested () =
  (* nd[a, nd[b, c]]: rendered "abc". Inner nd spans (1,3). *)
  with_cursor (nd [ id "a"; nd [ id "b"; id "c" ] ]) @@ fun c ->
  assert (Test_cursor.move_first_child c);
  assert (Test_cursor.move_next_sibling c);
  Alcotest.(check byte_range_pair) "inner nd spans 1..3" (1, 3) (Test_cursor.byte_range c)

let test_byte_range_independent_of_clone () =
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  let c2 = Test_cursor.clone c in
  assert (Test_cursor.move_first_child c);
  Alcotest.(check byte_range_pair) "c at leaf a: (0,1)" (0, 1) (Test_cursor.byte_range c);
  Alcotest.(check byte_range_pair) "c2 unchanged at root: (0,2)" (0, 2) (Test_cursor.byte_range c2)

(* ----- named_children ----- *)

(* Leaf has no children: named_children returns an empty list. *)
let test_named_children_of_leaf () =
  with_cursor (id "a") @@ fun c ->
  Alcotest.(check int) "leaf has no named children" 0
    (List.length (Test_cursor.named_children c))

(* Internal node returns one cursor per direct child. *)
let test_named_children_count () =
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  Alcotest.(check int) "three children" 3
    (List.length (Test_cursor.named_children c))

(* Each returned cursor walks its own subtree and produces the expected
   leaf text. *)
let test_named_children_leaf_texts () =
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  let texts =
    Test_cursor.named_children c
    |> List.map (fun child ->
           Test_cursor.leaf_text (Test_cursor.move_first_leaf child))
  in
  Alcotest.(check (list string)) "leaves in order" [ "a"; "b"; "c" ] texts

(* Compound children: each sub-cursor walks its own subtree. *)
let test_named_children_compound () =
  let inner = nd [ id "x"; id "y" ] in
  with_cursor (nd [ id "a"; inner ]) @@ fun c ->
  match Test_cursor.named_children c with
  | [ child1; child2 ] ->
      Alcotest.(check string)
        "first sub-cursor at 'a'" "a"
        (Test_cursor.leaf_text (Test_cursor.move_first_leaf child1));
      (* Second sub-cursor's leftmost leaf is 'x' (inside the inner node). *)
      Alcotest.(check string)
        "second sub-cursor's leftmost leaf is 'x'" "x"
        (Test_cursor.leaf_text (Test_cursor.move_first_leaf child2))
  | _ -> Alcotest.fail "expected exactly two children"

(* A sub-cursor returned by named_children is scoped to its child's
   subtree — move_next_subtree from its last leaf returns false, even
   though the parent has more siblings. *)
let test_named_children_scoped () =
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  match Test_cursor.named_children c with
  | [ child1; _; _ ] ->
      let leaf_a = Test_cursor.move_first_leaf child1 in
      Alcotest.(check string) "sub-cursor at 'a'" "a" (Test_cursor.leaf_text leaf_a);
      (* Inside child1's scope, there are no more subtrees past 'a'. *)
      Alcotest.(check bool)
        "advancing past child's scope returns false" false
        (Test_cursor.move_next_subtree child1)
  | _ -> Alcotest.fail "expected three children"

(* named_children at a position other than the root: cursor descended
   to an inner node, then enumerate its children. *)
let test_named_children_at_inner_node () =
  let inner = nd [ id "x"; id "y" ] in
  with_cursor (nd [ id "a"; inner ]) @@ fun c ->
  (* Descend to the inner node by skipping the first child. *)
  assert (Test_cursor.move_first_child c);
  assert (Test_cursor.move_next_sibling c);
  let children = Test_cursor.named_children c in
  Alcotest.(check int) "inner node has two children" 2 (List.length children);
  let texts =
    List.map
      (fun child ->
        Test_cursor.leaf_text (Test_cursor.move_first_leaf child))
      children
  in
  Alcotest.(check (list string)) "inner children: x, y" [ "x"; "y" ] texts

(* Original cursor is independent of the sub-cursors returned. *)
let test_named_children_clone_independence () =
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  let children = Test_cursor.named_children c in
  let _ =
    List.map
      (fun child -> Test_cursor.move_first_leaf child)
      children
  in
  (* Original cursor still at root. *)
  Alcotest.(check byte_range_pair)
    "original at root after enumeration" (0, 2)
    (Test_cursor.byte_range c)

(* ----- source_substring ----- *)

(* The synthesized source for nd[a, b, c] is "abc"; slicing produces the
   expected substrings. *)
let test_source_substring_basic () =
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  Alcotest.(check string) "whole source" "abc"
    (Test_cursor.source_substring c 0 3);
  Alcotest.(check string) "middle char" "b"
    (Test_cursor.source_substring c 1 2);
  Alcotest.(check string) "first two" "ab"
    (Test_cursor.source_substring c 0 2)

(* Empty range yields empty string. *)
let test_source_substring_empty () =
  with_cursor (nd [ id "a"; id "b" ]) @@ fun c ->
  Alcotest.(check string) "empty range" ""
    (Test_cursor.source_substring c 1 1)

(* Multi-character leaves: synthesized source concatenates each leaf's
   full text. *)
let test_source_substring_multi_char () =
  with_cursor (nd [ leaf "id" "hello"; leaf "id" "world" ]) @@ fun c ->
  Alcotest.(check string) "whole" "helloworld"
    (Test_cursor.source_substring c 0 10);
  Alcotest.(check string) "second leaf" "world"
    (Test_cursor.source_substring c 5 10)

(* Cursor's current position doesn't affect source_substring — same
   bytes regardless of where the cursor is parked. *)
let test_source_substring_position_independent () =
  with_cursor (nd [ id "a"; id "b"; id "c" ]) @@ fun c ->
  let at_root = Test_cursor.source_substring c 0 3 in
  assert (Test_cursor.move_first_child c);
  let after_descent = Test_cursor.source_substring c 0 3 in
  Alcotest.(check string) "same source slice regardless of position" at_root
    after_descent

(* The motivating use: read bytes between two adjacent named children
   to detect what separator the source uses. *)
let test_source_substring_inter_child_gap () =
  (* Trees aren't a great fit since they don't model separator punctuation
     between children directly. But we can build a tree whose pre-order
     rendering simulates one: leaves "a", ",", "b" gives synthesized
     source "a,b". *)
  with_cursor (nd [ id "a"; leaf "," ","; id "b" ]) @@ fun c ->
  let children = Test_cursor.named_children c in
  match children with
  | [ child_a; _comma; child_b ] ->
      let _, a_end = Test_cursor.byte_range child_a in
      let b_start, _ = Test_cursor.byte_range child_b in
      Alcotest.(check string)
        "separator bytes between a and b" ","
        (Test_cursor.source_substring c a_end b_start)
  | _ -> Alcotest.fail "expected three children"

let tests =
  let open Alcotest in
  [
    test_case "first_leaf on leaf" `Quick test_first_leaf_on_leaf;
    test_case "first_leaf descends one level" `Quick test_first_leaf_descends;
    test_case "first_leaf descends multiple levels" `Quick
      test_first_leaf_descends_deep;
    test_case "first_child on leaf returns false" `Quick
      test_first_child_on_leaf;
    test_case "first_child descends" `Quick test_first_child_descends;
    test_case "next_sibling iterates siblings" `Quick test_next_sibling;
    test_case "next_subtree within parent" `Quick
      test_next_subtree_within_parent;
    test_case "next_subtree climbs parent" `Quick test_next_subtree_climbs;
    test_case "next_subtree at root returns false" `Quick
      test_next_subtree_at_root;
    test_case "next_subtree exhausts" `Quick test_next_subtree_exhausts;
    test_case "clone is independent" `Quick test_clone_independence;
    test_case "byte_range: root" `Quick test_byte_range_root;
    test_case "byte_range: leaf first child" `Quick
      test_byte_range_single_leaf_child;
    test_case "byte_range: second sibling" `Quick test_byte_range_second_sibling;
    test_case "byte_range: multi-char leaf" `Quick
      test_byte_range_multi_char_leaf;
    test_case "byte_range: nested subtree" `Quick test_byte_range_nested;
    test_case "byte_range: clone independence" `Quick
      test_byte_range_independent_of_clone;
    test_case "named_children: leaf has none" `Quick
      test_named_children_of_leaf;
    test_case "named_children: count" `Quick test_named_children_count;
    test_case "named_children: leaf texts" `Quick
      test_named_children_leaf_texts;
    test_case "named_children: compound children" `Quick
      test_named_children_compound;
    test_case "named_children: sub-cursor is scoped" `Quick
      test_named_children_scoped;
    test_case "named_children: at inner node" `Quick
      test_named_children_at_inner_node;
    test_case "named_children: original cursor unaffected" `Quick
      test_named_children_clone_independence;
    test_case "source_substring: basic slices" `Quick
      test_source_substring_basic;
    test_case "source_substring: empty range" `Quick
      test_source_substring_empty;
    test_case "source_substring: multi-char leaves" `Quick
      test_source_substring_multi_char;
    test_case "source_substring: position-independent" `Quick
      test_source_substring_position_independent;
    test_case "source_substring: inter-child gap" `Quick
      test_source_substring_inter_child_gap;
  ]
