(** Benchmarks for index-based pattern matching

    This benchmark compares traversal-based vs index-based matching,
    particularly for multi-pattern scenarios where indexing provides
    significant benefits. *)

open Diffract

(* Generate source with multiple console calls and classes *)
let generate_source ~num_logs ~num_errors ~num_classes =
  let logs = List.init num_logs (fun i ->
    Printf.sprintf "console.log(\"log%d\");" i
  ) |> String.concat "\n" in
  let errors = List.init num_errors (fun i ->
    Printf.sprintf "console.error(\"err%d\");" i
  ) |> String.concat "\n" in
  let classes = List.init num_classes (fun i ->
    Printf.sprintf "class C%d { method%d() { return %d; } }" i i i
  ) |> String.concat "\n" in
  logs ^ "\n" ^ errors ^ "\n" ^ classes

(* Patterns that match different node types *)
let patterns = [
  {|@@
match: strict
metavar $msg: single
@@
console.log($msg)|};
  {|@@
match: strict
metavar $msg: single
@@
console.error($msg)|};
  {|@@
match: strict
metavar $name: single
metavar $body: sequence
@@
class $name { $body }|};
]

(* Time a function, returning (result, elapsed_seconds) *)
let time_once f =
  Gc.full_major ();
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  Gc.full_major ();
  (result, elapsed)

(* Time multiple runs, return median time *)
let time_median n f =
  let times = Array.init n (fun _ ->
    Gc.full_major ();
    let (_, elapsed) = time_once f in
    elapsed
  ) in
  Gc.full_major ();
  Array.sort compare times;
  times.(n / 2)

(* Match patterns using traversal (O(n*k) where n=nodes, k=patterns) *)
let bench_traversal_multi ~language ~patterns ~source_text =
  List.concat_map (fun pattern_text ->
    Match.find_matches ~language ~pattern_text ~source_text
  ) patterns

(* Match patterns using index (O(n + k*m) where m=matches per type) *)
let bench_indexed_multi ~language ~patterns ~source_text =
  Match.find_matches_multi ~language ~patterns ~source_text

let print_header title =
  Printf.printf "\n%s\n" (String.make 65 '=');
  Printf.printf "%s\n" title;
  Printf.printf "%s\n\n" (String.make 65 '=')

let () =
  print_header "Index-Based Pattern Matching Benchmarks";

  (* Test 1: Multi-pattern matching at different scales *)
  print_header "Test 1: Multi-Pattern Matching (3 patterns)";
  Printf.printf "Comparing traversal vs index for varying source sizes.\n\n";

  Printf.printf "%-18s %12s %12s %10s %8s\n"
    "Source Size" "Traversal" "Indexed" "Speedup" "Matches";
  Printf.printf "%s\n" (String.make 65 '-');

  let sizes = [
    (5, 3, 2);     (* small: ~40 nodes *)
    (20, 10, 5);   (* medium: ~150 nodes *)
    (50, 20, 10);  (* large: ~350 nodes *)
  ] in

  List.iter (fun (num_logs, num_errors, num_classes) ->
    let source = generate_source ~num_logs ~num_errors ~num_classes in
    let label = Printf.sprintf "%d nodes" (num_logs + num_errors + num_classes * 3) in

    let traversal_time = time_median 3 (fun () ->
      bench_traversal_multi ~language:"typescript" ~patterns ~source_text:source
    ) in

    let indexed_time = time_median 3 (fun () ->
      bench_indexed_multi ~language:"typescript" ~patterns ~source_text:source
    ) in

    let matches = bench_indexed_multi ~language:"typescript" ~patterns ~source_text:source in
    let speedup = if indexed_time > 0.0 then traversal_time /. indexed_time else 0.0 in

    Printf.printf "%-18s %10.2f ms %10.2f ms %9.2fx %8d\n"
      label
      (traversal_time *. 1000.0)
      (indexed_time *. 1000.0)
      speedup
      (List.length matches);

    Gc.full_major ()
  ) sizes;

  (* Test 2: Scaling with number of patterns *)
  print_header "Test 2: Scaling with Number of Patterns";
  Printf.printf "Fixed source, increasing pattern count.\n";
  Printf.printf "Shows how speedup improves with more patterns.\n\n";

  let source = generate_source ~num_logs:20 ~num_errors:10 ~num_classes:5 in

  Printf.printf "%-15s %12s %12s %10s\n"
    "# Patterns" "Traversal" "Indexed" "Speedup";
  Printf.printf "%s\n" (String.make 52 '-');

  for n = 1 to 3 do
    let pattern_subset = List.filteri (fun i _ -> i < n) patterns in

    let traversal_time = time_median 3 (fun () ->
      bench_traversal_multi ~language:"typescript" ~patterns:pattern_subset ~source_text:source
    ) in

    let indexed_time = time_median 3 (fun () ->
      bench_indexed_multi ~language:"typescript" ~patterns:pattern_subset ~source_text:source
    ) in

    let speedup = if indexed_time > 0.0 then traversal_time /. indexed_time else 0.0 in

    Printf.printf "%-15d %10.2f ms %10.2f ms %9.2fx\n"
      n
      (traversal_time *. 1000.0)
      (indexed_time *. 1000.0)
      speedup;

    Gc.full_major ()
  done;

  (* Test 3: Selective patterns - rare node types *)
  print_header "Test 3: Selective Patterns (Rare vs Common Types)";
  Printf.printf "Patterns matching rare types benefit most from indexing.\n\n";

  (* Source with many expressions but few classes *)
  let selective_source = {|
const a = 1 + 2;
const b = 3 + 4;
const c = a + b;
const d = c * 2;
const e = d - 1;
const f = e / 2;
const g = f + a;
const h = g - b;
class RareClass {
  rareMethod() { return 42; }
}
const i = h + c;
const j = i * d;
|} in

  let test_patterns = [
    ("class (rare)", {|@@
match: strict
metavar $name: single
metavar $body: sequence
@@
class $name { $body }|});
    ("const (common)", {|@@
match: strict
metavar $name: single
metavar $val: single
@@
const $name = $val|});
  ] in

  Printf.printf "%-20s %12s %12s %10s %8s\n"
    "Pattern Type" "Traversal" "Indexed" "Speedup" "Matches";
  Printf.printf "%s\n" (String.make 68 '-');

  List.iter (fun (label, pattern_text) ->
    let traversal_time = time_median 3 (fun () ->
      Match.find_matches ~language:"typescript" ~pattern_text ~source_text:selective_source
    ) in

    (* For fair comparison, index approach includes index building *)
    let indexed_time = time_median 3 (fun () ->
      Match.find_matches_multi ~language:"typescript"
        ~patterns:[pattern_text] ~source_text:selective_source
    ) in

    let matches = Match.find_matches ~language:"typescript"
        ~pattern_text ~source_text:selective_source in
    let speedup = if indexed_time > 0.0 then traversal_time /. indexed_time else 0.0 in

    Printf.printf "%-20s %10.2f ms %10.2f ms %9.2fx %8d\n"
      label
      (traversal_time *. 1000.0)
      (indexed_time *. 1000.0)
      speedup
      (List.length matches);

    Gc.full_major ()
  ) test_patterns;

  (* Test 4: Index reuse - amortized cost *)
  print_header "Test 4: Index Reuse (Amortized Cost)";
  Printf.printf "When index is pre-built, queries are very fast.\n\n";

  let source = generate_source ~num_logs:20 ~num_errors:10 ~num_classes:5 in
  let source_tree = Tree.parse ~language:"typescript" source in
  let index = Match.build_index source_tree.root in

  Printf.printf "Index built for source with ~130 nodes.\n\n";
  Printf.printf "%-35s %10s %8s\n" "Pattern" "Query" "Matches";
  Printf.printf "%s\n" (String.make 58 '-');

  List.iter (fun pattern_text ->
    let pattern = Match.parse_pattern ~language:"typescript" pattern_text in
    let query_time = time_median 5 (fun () ->
      Match.find_matches_with_index ~index ~pattern
        ~source:source_tree.source ~source_root:source_tree.root
    ) in
    let matches = Match.find_matches_with_index ~index ~pattern
        ~source:source_tree.source ~source_root:source_tree.root in
    let pattern_preview =
      let lines = String.split_on_char '\n' pattern_text in
      List.nth lines (List.length lines - 1)
    in
    Printf.printf "%-35s %8.3f ms %8d\n"
      pattern_preview (query_time *. 1000.0) (List.length matches);
    Gc.full_major ()
  ) patterns;

  print_header "Summary";
  Printf.printf "Index-based matching provides speedups by:\n\n";
  Printf.printf "  1. Building index once, querying many patterns\n";
  Printf.printf "     - Traversal: O(n) per pattern = O(n*k) total\n";
  Printf.printf "     - Indexed:   O(n) index + O(m) per pattern = O(n + k*m)\n";
  Printf.printf "     - Where m << n for selective patterns\n\n";
  Printf.printf "  2. Filtering candidates by node type\n";
  Printf.printf "     - Only match against nodes of the right type\n";
  Printf.printf "     - Rare patterns skip most nodes entirely\n\n";
  Printf.printf "Use find_matches_multi when matching multiple patterns.\n";
  Printf.printf "Use build_index + find_matches_with_index for custom workflows.\n"
