(** Benchmark for Stmatch.find_matches.

    Generates synthetic TypeScript sources of varying sizes and times how long
    find_matches takes against several representative patterns.

    Run: dune exec bench/find_matches.exe

    See bench/README.md for context and how to interpret the output. *)

open Diffract
module M = Stmatch.Make (Tree_sitter_cursor)

(* ========================================================================= *)
(* Source generation                                                         *)
(* ========================================================================= *)

(* Generate a TypeScript source with `n` blocks of mixed statements.
   Each block contributes ~5 statements. About 1 in 5 is a call to `foo`.
   Also includes calls with paired arguments (some matching, some not)
   to exercise within-argument non-linear matching. *)
let gen_source n =
  let buf = Buffer.create (n * 100) in
  Buffer.add_string buf "function bench() {\n";
  for i = 0 to n - 1 do
    Buffer.add_string buf (Printf.sprintf "  const a%d = %d;\n" i i);
    Buffer.add_string buf (Printf.sprintf "  bar(%d);\n" i);
    Buffer.add_string buf (Printf.sprintf "  baz(%d, %d);\n" i (i * 2));
    Buffer.add_string buf (Printf.sprintf "  qux(%d);\n" i);
    if i mod 5 = 0 then Buffer.add_string buf (Printf.sprintf "  foo(%d);\n" i);
    (* Calls with paired args at the same structural level (within one
       argument list, so Siblings can absorb between them). *)
    if i mod 6 = 0 then
      Buffer.add_string buf
        (Printf.sprintf "  pair(%d, between, %d);\n" i
           (if i mod 12 = 0 then i else i + 1))
  done;
  Buffer.add_string buf "}\n";
  Buffer.contents buf

(* ========================================================================= *)
(* Patterns                                                                  *)
(* ========================================================================= *)

(* Pattern A: distinctive Concrete first. Anchor search would target `foo`. *)
let pattern_distinctive =
  [
    Stmatch.Concrete { text = "foo"; node_type = "identifier" };
    Stmatch.Concrete { text = "("; node_type = "(" };
    Stmatch.Subtree { name = Some "x" };
    Stmatch.Concrete { text = ")"; node_type = ")" };
  ]

(* Pattern B: wildcard first; first Concrete is `(`. Anchor search couldn't
   easily apply (wildcard-first), and even if forced to anchor on `(`, that
   token is everywhere. *)
let pattern_wildcard_first =
  [
    Stmatch.Subtree { name = Some "fn" };
    Stmatch.Concrete { text = "("; node_type = "(" };
    Stmatch.Subtree { name = Some "x" };
    Stmatch.Concrete { text = ")"; node_type = ")" };
  ]

(* Pattern C: common Concrete first (`bar` instead of `foo`). For a 1-in-5
   distribution of foo's, bar appears 5x as often. *)
let pattern_common =
  [
    Stmatch.Concrete { text = "bar"; node_type = "identifier" };
    Stmatch.Concrete { text = "("; node_type = "(" };
    Stmatch.Subtree { name = Some "x" };
    Stmatch.Concrete { text = ")"; node_type = ")" };
  ]

(* Pattern D: non-linear within a single argument list — pair($x, ..., $x).
   The two $x must bind to equal subtrees; the ... absorbs whatever
   arguments are between. All within a single arguments node, so the
   Siblings absorption is at one structural level (it works). *)
let pattern_nonlinear =
  [
    Stmatch.Concrete { text = "pair"; node_type = "identifier" };
    Stmatch.Concrete { text = "("; node_type = "(" };
    Stmatch.Subtree { name = Some "x" };
    Stmatch.Concrete { text = ","; node_type = "," };
    Stmatch.Siblings { name = None };
    Stmatch.Concrete { text = ","; node_type = "," };
    Stmatch.Subtree { name = Some "x" };
    Stmatch.Concrete { text = ")"; node_type = ")" };
  ]

(* Pattern E: many concrete tokens — long sequence. Exercises the
   per-position match_at cost; each failed attempt walks more pattern
   tokens before failing. *)
let pattern_long =
  [
    Stmatch.Concrete { text = "baz"; node_type = "identifier" };
    Stmatch.Concrete { text = "("; node_type = "(" };
    Stmatch.Subtree { name = Some "x" };
    Stmatch.Concrete { text = ","; node_type = "," };
    Stmatch.Subtree { name = Some "y" };
    Stmatch.Concrete { text = ")"; node_type = ")" };
  ]

(* ========================================================================= *)
(* Timing                                                                    *)
(* ========================================================================= *)

let time_one f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed_ms = (Unix.gettimeofday () -. start) *. 1000.0 in
  (result, elapsed_ms)

(* Count named nodes in the tree (for context). *)
let rec count_nodes (n : _ Tree.t) =
  if n.is_extra then 0
  else
    1
    + List.fold_left
        (fun acc (c : _ Tree.child) -> acc + count_nodes c.node)
        0 n.children

(* ========================================================================= *)
(* Main                                                                      *)
(* ========================================================================= *)

let bench_pattern ~ctx ~name ~pattern ~source =
  let tree = Tree.parse ~ctx ~language:"typescript" source in
  let nodes = count_nodes tree.root in
  let bytes = String.length source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  let results, match_ms = time_one (fun () -> M.find_matches pattern cursor) in
  Printf.printf "  %-30s | %7d nodes | %6d B | %8.2f ms | %5d matches\n" name
    nodes bytes match_ms (List.length results)

let () =
  let ctx = Context.create () in
  let sizes = [ 100; 500; 2000; 10000 ] in
  Printf.printf "\nfind_matches benchmark (brute-force outer loop)\n";
  Printf.printf "================================================\n\n";
  List.iter
    (fun n ->
      let source = gen_source n in
      Printf.printf "n = %d (synthetic TS blocks):\n" n;
      bench_pattern ~ctx ~name:"distinctive-first: foo($x)"
        ~pattern:pattern_distinctive ~source;
      bench_pattern ~ctx ~name:"common-first: bar($x)" ~pattern:pattern_common
        ~source;
      bench_pattern ~ctx ~name:"wildcard-first: $fn($x)"
        ~pattern:pattern_wildcard_first ~source;
      bench_pattern ~ctx ~name:"long: baz($x,$y)" ~pattern:pattern_long ~source;
      bench_pattern ~ctx ~name:"non-linear: pair($x,...,$x)"
        ~pattern:pattern_nonlinear ~source;
      Printf.printf "\n")
    sizes
