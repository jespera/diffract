(** Tests for the Tree module *)

open Diffract

let ctx = Context.create ()

let parse_ts source =
  let tree = Diffract.parse_tree ~ctx ~language:"typescript" source in
  (tree.root, tree.source)

(* Test: Root node type *)
let test_root_node_type () =
  let root, _ = parse_ts "const x = 1;" in
  Alcotest.(check string) "root is program" "program" root.Tree.node_type

(* Test: Can find child nodes *)
let test_find_children () =
  let root, _ = parse_ts "function foo() {}" in
  let children = root.Tree.named_children in
  Alcotest.(check bool) "has children" true (List.length children > 0);
  let child_types = List.map (fun n -> n.Tree.node_type) children in
  Alcotest.(check bool)
    "contains function_declaration" true
    (List.mem "function_declaration" child_types)

(* Test: Text extraction *)
let test_text_extraction () =
  let source = "const x = 42;" in
  let tree = Diffract.parse_tree ~ctx ~language:"typescript" source in
  let text = Tree.text tree.source tree.root in
  Alcotest.(check string) "text matches source" source text

(* Test: Text extraction for child *)
let test_child_text_extraction () =
  let source = "const x = 42;" in
  let tree = Diffract.parse_tree ~ctx ~language:"typescript" source in
  let numbers = Tree.find_by_type "number" tree.root in
  Alcotest.(check bool) "found number" true (List.length numbers > 0);
  let num = List.hd numbers in
  Alcotest.(check string) "number text" "42" (Tree.text tree.source num)

(* Test: find_by_type *)
let test_find_by_type () =
  let source = "function foo(a, b) { return a + b; }" in
  let root, _ = parse_ts source in
  let identifiers = Tree.find_by_type "identifier" root in
  (* Should find: foo, a, b, a, b *)
  Alcotest.(check bool)
    "found multiple identifiers" true
    (List.length identifiers >= 4)

(* Test: named_children vs children *)
let test_named_vs_all_children () =
  let source = "(1 + 2)" in
  let root, _ = parse_ts source in
  let exprs = Tree.find_by_type "binary_expression" root in
  Alcotest.(check bool) "found binary_expression" true (List.length exprs > 0);
  let expr = List.hd exprs in
  let named_count = List.length expr.Tree.named_children in
  let all_count = List.length expr.Tree.children in
  (* binary_expression has named children (operands) and anonymous (+) *)
  Alcotest.(check bool)
    "all_count >= named_count" true (all_count >= named_count)

(* Test: child_by_field_name returns option *)
let test_child_by_field_name () =
  let root, _ = parse_ts "const x = 1;" in
  (* Root doesn't have a nonexistent_field *)
  let missing = Tree.child_by_field_name root "nonexistent_field" in
  Alcotest.(check bool) "missing field is None" true (Option.is_none missing)

(* Test: traverse visits all nodes *)
let test_traverse () =
  let source = "const x = 1;" in
  let root, _ = parse_ts source in
  let count = ref 0 in
  Tree.traverse (fun _ -> incr count) root;
  Alcotest.(check bool) "visited multiple nodes" true (!count > 1)

let tests =
  [
    Alcotest.test_case "root node type" `Quick test_root_node_type;
    Alcotest.test_case "find children" `Quick test_find_children;
    Alcotest.test_case "text extraction" `Quick test_text_extraction;
    Alcotest.test_case "child text extraction" `Quick test_child_text_extraction;
    Alcotest.test_case "find_by_type" `Quick test_find_by_type;
    Alcotest.test_case "named vs all children" `Quick test_named_vs_all_children;
    Alcotest.test_case "child_by_field_name" `Quick test_child_by_field_name;
    Alcotest.test_case "traverse" `Quick test_traverse;
  ]
