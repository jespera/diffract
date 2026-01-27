(** Tests for the Diff module *)

open Diffract

let diff_ts before after =
  Diffract.diff ~language:"typescript" ~before ~after

(* Test: Identical sources produce no changes *)
let test_identical_no_changes () =
  let source = "const x = 1;" in
  let result = diff_ts source source in
  Alcotest.(check int) "no changes" 0 (List.length result.changes)

(* Test: Added node detection *)
let test_added_node () =
  let before = "const x = 1;" in
  let after = "const x = 1;\nconst y = 2;" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  let has_added = List.exists (function
    | Diff.Added _ -> true
    | _ -> false
  ) flat in
  Alcotest.(check bool) "has added change" true has_added

(* Test: Removed node detection *)
let test_removed_node () =
  let before = "const x = 1;\nconst y = 2;" in
  let after = "const x = 1;" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  let has_removed = List.exists (function
    | Diff.Removed _ -> true
    | _ -> false
  ) flat in
  Alcotest.(check bool) "has removed change" true has_removed

(* Test: Modified node detection *)
let test_modified_node () =
  let before = "function foo() { return 1; }" in
  let after = "function foo() { return 2; }" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  Alcotest.(check bool) "has changes" true (List.length flat > 0)

(* Test: Replaced node detection - different types *)
let test_replaced_different_types () =
  (* number vs string literal have different node types *)
  let before = "const x = 1;" in
  let after = "const x = \"hello\";" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  (* Should detect some change at the value level *)
  Alcotest.(check bool) "has changes" true (List.length flat > 0);
  (* Check if there's a Replaced or the number/string types appear in changes *)
  let has_type_change = List.exists (fun change ->
    let node_type = Diff.change_node_type change in
    node_type = "number" || node_type = "string" ||
    match change with Diff.Replaced _ -> true | _ -> false
  ) flat in
  Alcotest.(check bool) "has type-related change" true has_type_change

(* Test: Leaf text change detection - the bug we fixed *)
let test_leaf_text_change () =
  let before = "console.log(x);" in
  let after = "console.warn(x);" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  (* Should detect that "log" changed to "warn" *)
  Alcotest.(check bool) "detected leaf change" true (List.length flat > 0);
  (* The deepest change should be the property_identifier *)
  let has_prop_change = List.exists (fun change ->
    Diff.change_node_type change = "property_identifier"
  ) flat in
  Alcotest.(check bool) "detected property_identifier change" true has_prop_change

(* Test: nodes_equal with same text *)
let test_nodes_equal_same () =
  let source = "const x = 1;" in
  let tree = Diffract.parse_tree ~language:"typescript" source in
  let root = tree.root in
  let src = tree.source in
  Alcotest.(check bool) "same node equals itself"
    true (Diff.nodes_equal src root src root)

(* Test: nodes_equal with different text *)
let test_nodes_equal_different () =
  let source1 = "const x = 1;" in
  let source2 = "const x = 2;" in
  let tree1 = Diffract.parse_tree ~language:"typescript" source1 in
  let tree2 = Diffract.parse_tree ~language:"typescript" source2 in
  let root1 = tree1.root in
  let root2 = tree2.root in
  let src1 = tree1.source in
  let src2 = tree2.source in
  Alcotest.(check bool) "different content not equal"
    false (Diff.nodes_equal src1 root1 src2 root2)

(* Test: same_shape ignores text *)
let test_same_shape () =
  let source1 = "const x = 1;" in
  let source2 = "const y = 2;" in
  let tree1 = Diffract.parse_tree ~language:"typescript" source1 in
  let tree2 = Diffract.parse_tree ~language:"typescript" source2 in
  let root1 = tree1.root in
  let root2 = tree2.root in
  Alcotest.(check bool) "same shape despite different text"
    true (Diff.same_shape root1 root2)

(* Test: different shape *)
let test_different_shape () =
  let source1 = "const x = 1;" in
  let source2 = "function foo() {}" in
  let tree1 = Diffract.parse_tree ~language:"typescript" source1 in
  let tree2 = Diffract.parse_tree ~language:"typescript" source2 in
  let root1 = tree1.root in
  let root2 = tree2.root in
  Alcotest.(check bool) "different shapes"
    false (Diff.same_shape root1 root2)

(* Test: flatten_changes preserves all changes *)
let test_flatten_preserves_changes () =
  let before = "function foo(a) { return a; }" in
  let after = "function foo(a, b) { return a + b; }" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  (* Flattened should have at least as many as top-level *)
  Alcotest.(check bool) "flatten produces changes"
    true (List.length flat >= List.length result.changes)

(* Test: change_context provides location info *)
let test_change_context () =
  let before = "const x = 1;" in
  let after = "const x = 2;" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  List.iter (fun change ->
    let ctx = Diff.change_context change in
    (* Context should have valid index *)
    Alcotest.(check bool) "context has valid index"
      true (ctx.index >= 0)
  ) flat

let tests = [
  Alcotest.test_case "identical no changes" `Quick test_identical_no_changes;
  Alcotest.test_case "added node" `Quick test_added_node;
  Alcotest.test_case "removed node" `Quick test_removed_node;
  Alcotest.test_case "modified node" `Quick test_modified_node;
  Alcotest.test_case "replaced different types" `Quick test_replaced_different_types;
  Alcotest.test_case "leaf text change" `Quick test_leaf_text_change;
  Alcotest.test_case "nodes_equal same" `Quick test_nodes_equal_same;
  Alcotest.test_case "nodes_equal different" `Quick test_nodes_equal_different;
  Alcotest.test_case "same_shape" `Quick test_same_shape;
  Alcotest.test_case "different_shape" `Quick test_different_shape;
  Alcotest.test_case "flatten preserves" `Quick test_flatten_preserves_changes;
  Alcotest.test_case "change context" `Quick test_change_context;
]
