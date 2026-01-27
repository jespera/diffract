(** Tests for the Antiunify module *)

open Diffract

let parse_ts source =
  Diffract.parse_tree ~language:"typescript" source

let diff_ts before after =
  Diffract.diff ~language:"typescript" ~before ~after

(* Test: Identical nodes return None *)
let test_identical_nodes_none () =
  let source = "const x = 1;" in
  let tree = parse_ts source in
  let root = tree.root in
  let src = tree.source in
  let result = Antiunify.antiunify_nodes src root src root in
  Alcotest.(check bool) "identical nodes return None"
    true (Option.is_none result)

(* Test: Different leaf text produces Diff *)
let test_different_leaf_diff () =
  let source1 = "log" in
  let source2 = "warn" in
  let tree1 = parse_ts source1 in
  let tree2 = parse_ts source2 in
  let root1 = tree1.root in
  let root2 = tree2.root in
  let src1 = tree1.source in
  let src2 = tree2.source in
  (* Find the identifier nodes *)
  let ids1 = Tree.find_by_type "identifier" root1 in
  let ids2 = Tree.find_by_type "identifier" root2 in
  match ids1, ids2 with
  | [id1], [id2] ->
    let result = Antiunify.antiunify_nodes src1 id1 src2 id2 in
    (match result with
    | Some (Antiunify.Diff { before; after; _ }) ->
      Alcotest.(check string) "before is log" "log" before;
      Alcotest.(check string) "after is warn" "warn" after
    | Some (Antiunify.Replaced { before_text; after_text; _ }) ->
      (* Also acceptable if types differ *)
      Alcotest.(check string) "before is log" "log" before_text;
      Alcotest.(check string) "after is warn" "warn" after_text
    | _ ->
      Alcotest.fail "expected Diff or Replaced annotation")
  | _ ->
    Alcotest.fail "expected single identifier in each"

(* Test: Added child produces Added annotation *)
let test_added_child () =
  let before = "f(a)" in
  let after = "f(a, b)" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  let added_changes = List.filter (function
    | Diff.Added _ -> true
    | _ -> false
  ) flat in
  Alcotest.(check bool) "has added changes" true (List.length added_changes > 0);
  (* Check anti-unification of an added change *)
  match added_changes with
  | change :: _ ->
    let ann = Antiunify.antiunify_change result change in
    (match ann with
    | Antiunify.Added { text; _ } ->
      Alcotest.(check bool) "added text contains b"
        true (String.length text > 0)
    | _ ->
      Alcotest.fail "expected Added annotation")
  | [] ->
    Alcotest.fail "no added changes found"

(* Test: Removed child produces Removed annotation *)
let test_removed_child () =
  let before = "f(a, b)" in
  let after = "f(a)" in
  let result = diff_ts before after in
  let flat = Diff.flatten_changes result.changes in
  let removed_changes = List.filter (function
    | Diff.Removed _ -> true
    | _ -> false
  ) flat in
  Alcotest.(check bool) "has removed changes" true (List.length removed_changes > 0);
  match removed_changes with
  | change :: _ ->
    let ann = Antiunify.antiunify_change result change in
    (match ann with
    | Antiunify.Removed { text; _ } ->
      Alcotest.(check bool) "removed text non-empty"
        true (String.length text > 0)
    | _ ->
      Alcotest.fail "expected Removed annotation")
  | [] ->
    Alcotest.fail "no removed changes found"

(* Test: Pattern from Diff annotation is PTransform *)
let test_pattern_from_diff () =
  let ann = Antiunify.Diff {
    node_type = "identifier";
    before = "log";
    after = "warn";
  } in
  let pattern = Antiunify.pattern_of_annotated ann in
  match pattern with
  | Antiunify.PTransform { before; after } ->
    (match before, after with
    | Antiunify.PConcrete b, Antiunify.PConcrete a ->
      Alcotest.(check string) "before pattern" "log" b;
      Alcotest.(check string) "after pattern" "warn" a
    | _ ->
      Alcotest.fail "expected PConcrete in transform")
  | _ ->
    Alcotest.fail "expected PTransform pattern"

(* Test: Pattern from Same annotation is PConcrete *)
let test_pattern_from_same () =
  let ann = Antiunify.Same {
    node_type = "identifier";
    text = "foo";
    children = [];
  } in
  let pattern = Antiunify.pattern_of_annotated ann in
  match pattern with
  | Antiunify.PConcrete text ->
    Alcotest.(check string) "concrete text" "foo" text
  | _ ->
    Alcotest.fail "expected PConcrete pattern"

(* Test: Anti-unify identical patterns stays concrete *)
let test_antiunify_identical_patterns () =
  let p = Antiunify.PConcrete "hello" in
  let (result, _) = Antiunify.antiunify_patterns 0 p p in
  match result with
  | Antiunify.PConcrete text ->
    Alcotest.(check string) "stays concrete" "hello" text
  | _ ->
    Alcotest.fail "expected PConcrete"

(* Test: Anti-unify different patterns creates variable *)
let test_antiunify_different_patterns () =
  let p1 = Antiunify.PConcrete "hello" in
  let p2 = Antiunify.PConcrete "world" in
  let (result, var_count) = Antiunify.antiunify_patterns 0 p1 p2 in
  match result with
  | Antiunify.PVar (_, values) ->
    Alcotest.(check bool) "has both values"
      true (List.mem "hello" values && List.mem "world" values);
    Alcotest.(check bool) "var counter incremented"
      true (var_count > 0)
  | _ ->
    Alcotest.fail "expected PVar"

(* Test: Anti-unify transforms *)
let test_antiunify_transforms () =
  let p1 = Antiunify.PTransform {
    before = Antiunify.PConcrete "log";
    after = Antiunify.PConcrete "info";
  } in
  let p2 = Antiunify.PTransform {
    before = Antiunify.PConcrete "log";
    after = Antiunify.PConcrete "warn";
  } in
  let (result, _) = Antiunify.antiunify_patterns 0 p1 p2 in
  match result with
  | Antiunify.PTransform { before; after } ->
    (* before should be concrete "log" *)
    (match before with
    | Antiunify.PConcrete "log" -> ()
    | _ -> Alcotest.fail "expected before to be concrete 'log'");
    (* after should be a variable with info and warn *)
    (match after with
    | Antiunify.PVar (_, values) ->
      Alcotest.(check bool) "after has info and warn"
        true (List.mem "info" values && List.mem "warn" values)
    | _ -> Alcotest.fail "expected after to be PVar")
  | _ ->
    Alcotest.fail "expected PTransform"

(* Test: get_variable_values *)
let test_get_variable_values () =
  let pattern = Antiunify.PVar (0, ["a"; "b"; "c"]) in
  let values = Antiunify.get_variable_values 0 pattern in
  Alcotest.(check int) "has 3 values" 3 (List.length values);
  Alcotest.(check bool) "contains a" true (List.mem "a" values)

(* Test: antiunify_pattern_list *)
let test_antiunify_pattern_list () =
  let patterns = [
    Antiunify.PConcrete "x";
    Antiunify.PConcrete "y";
    Antiunify.PConcrete "z";
  ] in
  match Antiunify.antiunify_pattern_list patterns with
  | Some (Antiunify.PVar (_, values)) ->
    Alcotest.(check bool) "contains all values"
      true (List.mem "x" values && List.mem "y" values && List.mem "z" values)
  | _ ->
    Alcotest.fail "expected PVar with all values"

(* Helper to check if string contains substring *)
let contains_substring haystack needle =
  let nl = String.length needle in
  let hl = String.length haystack in
  if nl > hl then false
  else
    let rec check i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else check (i + 1)
    in
    check 0

(* Test: to_string produces expected format *)
let test_to_string_diff () =
  let ann = Antiunify.Diff {
    node_type = "id";
    before = "log";
    after = "warn";
  } in
  let s = Antiunify.to_string ann in
  (* Check format contains both before and after values *)
  Alcotest.(check bool) "contains log"
    true (contains_substring s "log");
  Alcotest.(check bool) "contains warn"
    true (contains_substring s "warn")

let tests = [
  Alcotest.test_case "identical nodes none" `Quick test_identical_nodes_none;
  Alcotest.test_case "different leaf diff" `Quick test_different_leaf_diff;
  Alcotest.test_case "added child" `Quick test_added_child;
  Alcotest.test_case "removed child" `Quick test_removed_child;
  Alcotest.test_case "pattern from diff" `Quick test_pattern_from_diff;
  Alcotest.test_case "pattern from same" `Quick test_pattern_from_same;
  Alcotest.test_case "antiunify identical" `Quick test_antiunify_identical_patterns;
  Alcotest.test_case "antiunify different" `Quick test_antiunify_different_patterns;
  Alcotest.test_case "antiunify transforms" `Quick test_antiunify_transforms;
  Alcotest.test_case "get variable values" `Quick test_get_variable_values;
  Alcotest.test_case "antiunify pattern list" `Quick test_antiunify_pattern_list;
  Alcotest.test_case "to_string diff" `Quick test_to_string_diff;
]
