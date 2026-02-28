(** Tests for the Tree_diff module *)

open Diffract

let ctx = Context.create ()
let parse source = Diffract.parse_tree ~ctx ~language:"typescript" source

(** Helper: check if a node_change is Unchanged *)
let is_unchanged = function Tree_diff.Unchanged -> true | _ -> false

(** Helper: count change types in child_changes *)
let count_changes child_changes =
  let same = ref 0 in
  let changed = ref 0 in
  let removed = ref 0 in
  let added = ref 0 in
  List.iter
    (function
      | Tree_diff.Same _ -> incr same
      | Tree_diff.Changed _ -> incr changed
      | Tree_diff.Removed _ -> incr removed
      | Tree_diff.Added _ -> incr added)
    child_changes;
  (!same, !changed, !removed, !added)

(* Test: Identical trees produce Unchanged *)
let test_identical_trees () =
  let source = "function foo() { return 1; }" in
  let before = parse source in
  let after = parse source in
  let d = Tree_diff.diff ~before ~after in
  Alcotest.(check bool)
    "identical trees are Unchanged" true
    (is_unchanged d.root_change)

(* Test: Single leaf change drills down to the changed node *)
let test_single_leaf_change () =
  let before = parse "const x = 1;" in
  let after = parse "const x = 2;" in
  let d = Tree_diff.diff ~before ~after in
  Alcotest.(check bool)
    "root is not Unchanged" false
    (is_unchanged d.root_change);
  (* Should drill down to the number literal *)
  let pairs = Tree_diff.change_pairs d in
  Alcotest.(check bool) "has change pairs" true (List.length pairs > 0);
  let pair = List.hd pairs in
  let before_text = Tree.text pair.before_source pair.before_node in
  let after_text = Tree.text pair.after_source pair.after_node in
  Alcotest.(check string) "before is 1" "1" before_text;
  Alcotest.(check string) "after is 2" "2" after_text

(* Test: Statement added is detected *)
let test_statement_added () =
  let before = parse "function f() { const a = 1; }" in
  let after = parse "function f() { const a = 1; const b = 2; }" in
  let d = Tree_diff.diff ~before ~after in
  Alcotest.(check bool) "not unchanged" false (is_unchanged d.root_change);
  (* Find the statement_block level where the addition happened *)
  let rec find_addition = function
    | Tree_diff.Unchanged -> false
    | Tree_diff.Replaced -> false
    | Tree_diff.Modified { child_changes } ->
        List.exists
          (function
            | Tree_diff.Added _ -> true
            | Tree_diff.Changed { change; _ } -> find_addition change
            | _ -> false)
          child_changes
  in
  Alcotest.(check bool) "found addition" true (find_addition d.root_change)

(* Test: Statement removed is detected *)
let test_statement_removed () =
  let before = parse "function f() { const a = 1; const b = 2; }" in
  let after = parse "function f() { const a = 1; }" in
  let d = Tree_diff.diff ~before ~after in
  let rec find_removal = function
    | Tree_diff.Unchanged -> false
    | Tree_diff.Replaced -> false
    | Tree_diff.Modified { child_changes } ->
        List.exists
          (function
            | Tree_diff.Removed _ -> true
            | Tree_diff.Changed { change; _ } -> find_removal change
            | _ -> false)
          child_changes
  in
  Alcotest.(check bool) "found removal" true (find_removal d.root_change)

(* Test: Statement reordering is handled — matching finds nodes by hash *)
let test_statement_reorder () =
  let before = parse "function f() { const a = 1; const b = 2; }" in
  let after = parse "function f() { const b = 2; const a = 1; }" in
  let d = Tree_diff.diff ~before ~after in
  (* Both statements exist in both trees, just reordered.
     The matching should find them by hash. The diff should not report
     any Replaced changes — just reordering. *)
  let pairs = Tree_diff.change_pairs d in
  Alcotest.(check int) "no replaced pairs" 0 (List.length pairs)

(* Test: Mixed changes — add + remove + modify *)
let test_mixed_changes () =
  let before =
    parse "function f() { const a = 1; const b = 2; const c = 3; }"
  in
  let after =
    parse "function f() { const a = 1; const b = 99; const d = 4; }"
  in
  let d = Tree_diff.diff ~before ~after in
  let pairs = Tree_diff.change_pairs d in
  (* b changed from 2 to 99, c removed, d added *)
  Alcotest.(check bool) "has change pairs" true (List.length pairs > 0);
  (* Verify the modification: 2 → 99 *)
  let found_mod =
    List.exists
      (fun (p : Tree_diff.change_pair) ->
        let bt = Tree.text p.before_source p.before_node in
        let at = Tree.text p.after_source p.after_node in
        bt = "2" && at = "99")
      pairs
  in
  Alcotest.(check bool) "found 2→99 change" true found_mod

(* Test: Formatting-only change produces Unchanged *)
let test_formatting_only () =
  let before = parse "function f() { return 1; }" in
  let after = parse "function f() {\n  return 1;\n}" in
  let d = Tree_diff.diff ~before ~after in
  Alcotest.(check bool)
    "formatting only is Unchanged" true
    (is_unchanged d.root_change)

(* Test: change_pairs extracts leaf-level pairs *)
let test_change_pairs_extraction () =
  let before = parse "const x = foo(1, 2);" in
  let after = parse "const x = bar(1, 2);" in
  let d = Tree_diff.diff ~before ~after in
  let pairs = Tree_diff.change_pairs d in
  Alcotest.(check bool) "has pairs" true (List.length pairs > 0);
  (* The change is foo → bar at the identifier level *)
  let found =
    List.exists
      (fun (p : Tree_diff.change_pair) ->
        let bt = Tree.text p.before_source p.before_node in
        let at = Tree.text p.after_source p.after_node in
        bt = "foo" && at = "bar")
      pairs
  in
  Alcotest.(check bool) "found foo→bar" true found

(* Test: Nested change drills down correctly *)
let test_nested_change () =
  let before = parse "function f() { if (true) { return 1; } }" in
  let after = parse "function f() { if (true) { return 2; } }" in
  let d = Tree_diff.diff ~before ~after in
  let pairs = Tree_diff.change_pairs d in
  Alcotest.(check bool) "has pairs" true (List.length pairs > 0);
  let pair = List.hd pairs in
  let before_text = Tree.text pair.before_source pair.before_node in
  let after_text = Tree.text pair.after_source pair.after_node in
  Alcotest.(check string) "before is 1" "1" before_text;
  Alcotest.(check string) "after is 2" "2" after_text

(* Test: Duplicate subtrees — one changed, one not *)
let test_duplicate_subtrees () =
  let before = parse "const a = foo(1); const b = foo(1);" in
  let after = parse "const a = foo(1); const b = foo(2);" in
  let d = Tree_diff.diff ~before ~after in
  let pairs = Tree_diff.change_pairs d in
  (* Only the second foo(1)→foo(2) should show as changed *)
  Alcotest.(check bool) "has change pairs" true (List.length pairs > 0);
  let found =
    List.exists
      (fun (p : Tree_diff.change_pair) ->
        let bt = Tree.text p.before_source p.before_node in
        let at = Tree.text p.after_source p.after_node in
        bt = "1" && at = "2")
      pairs
  in
  Alcotest.(check bool) "found 1→2 change" true found

let tests =
  [
    Alcotest.test_case "identical trees" `Quick test_identical_trees;
    Alcotest.test_case "single leaf change" `Quick test_single_leaf_change;
    Alcotest.test_case "statement added" `Quick test_statement_added;
    Alcotest.test_case "statement removed" `Quick test_statement_removed;
    Alcotest.test_case "statement reorder" `Quick test_statement_reorder;
    Alcotest.test_case "mixed changes" `Quick test_mixed_changes;
    Alcotest.test_case "formatting only" `Quick test_formatting_only;
    Alcotest.test_case "change pairs extraction" `Quick
      test_change_pairs_extraction;
    Alcotest.test_case "nested change" `Quick test_nested_change;
    Alcotest.test_case "duplicate subtrees" `Quick test_duplicate_subtrees;
  ]
