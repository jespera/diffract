(** Benchmarks for pattern matching *)

open Bechamel
open Toolkit

(* Generate source code with N methods in a class *)
let generate_class_source n =
  let methods = List.init n (fun i ->
    Printf.sprintf "  method%d() { console.log(\"msg%d\"); }" i i
  ) |> String.concat "\n" in
  Printf.sprintf "class TestClass {\n%s\n}" methods

(* Simple pattern - no sequence metavars *)
let simple_pattern = {|@@
metavar $obj: single
metavar $method: single
metavar $arg: single
@@
$obj.$method($arg)|}

let simple_source = {|
console.log("a");
console.log("b");
Math.floor(3.14);
Math.ceil(2.5);
|}

(* Sequence pattern for class matching *)
let sequence_pattern = {|@@
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }|}

(* Nested pattern with sequence *)
let nested_sequence_pattern = {|@@
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }

@@
metavar $msg: single
@@
console.log($msg)|}

(* Counter for periodic GC *)
let gc_counter = ref 0

(* Run GC periodically to prevent memory buildup *)
let maybe_gc () =
  incr gc_counter;
  if !gc_counter mod 5 = 0 then Gc.full_major ()

(* Benchmark: simple pattern matching *)
let bench_simple =
  Test.make ~name:"simple_pattern"
    (Staged.stage (fun () ->
      ignore (Diffract.Match.find_matches
        ~language:"typescript"
        ~pattern_text:simple_pattern
        ~source_text:simple_source);
      maybe_gc ()))

(* Benchmark: sequence metavar with N children *)
let bench_sequence n =
  let source = generate_class_source n in
  Test.make ~name:(Printf.sprintf "sequence_%02d_children" n)
    (Staged.stage (fun () ->
      ignore (Diffract.Match.find_matches
        ~language:"typescript"
        ~pattern_text:sequence_pattern
        ~source_text:source);
      maybe_gc ()))

(* Benchmark: nested pattern with sequence metavar *)
let bench_nested_sequence n =
  let source = generate_class_source n in
  Test.make ~name:(Printf.sprintf "nested_seq_%02d_children" n)
    (Staged.stage (fun () ->
      ignore (Diffract.Match.find_nested_matches
        ~language:"typescript"
        ~pattern_text:nested_sequence_pattern
        ~source_text:source);
      maybe_gc ()))

(* All benchmarks *)
let benchmarks =
  Test.make_grouped ~name:"pattern_matching" [
    bench_simple;
    bench_sequence 2;
    bench_sequence 5;
    bench_sequence 10;
    bench_sequence 20;
    bench_nested_sequence 2;
    bench_nested_sequence 5;
    bench_nested_sequence 10;
    bench_nested_sequence 20;
  ]

(* Extract results and save to JSON *)
let save_results_json analyzed_results instances =
  let timestamp = Unix.gettimeofday () in
  let git_commit =
    try
      let ic = Unix.open_process_in "git rev-parse --short HEAD 2>/dev/null" in
      let commit = input_line ic in
      ignore (Unix.close_process_in ic);
      commit
    with _ -> "unknown"
  in

  (* Extract timing data from analyzed results *)
  let bench_data = ref [] in
  List.iter (fun instance ->
    let merged = Analyze.merge Analyze.(ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |]) [ instance ] analyzed_results in
    Hashtbl.iter (fun _measure_label results_tbl ->
      Hashtbl.iter (fun name ols_result ->
        match Analyze.OLS.estimates ols_result with
        | Some [ v ] ->
          bench_data := (name, v) :: !bench_data
        | _ -> ()
      ) results_tbl
    ) merged
  ) instances;

  (* Build JSON *)
  let benchmarks_json = List.map (fun (name, mean_ns) ->
    `Assoc [
      ("name", `String name);
      ("mean_ns", `Float mean_ns);
    ]
  ) !bench_data in

  let json = `Assoc [
    ("timestamp", `Float timestamp);
    ("git_commit", `String git_commit);
    ("benchmarks", `List benchmarks_json);
  ] in

  (* Save to file *)
  let timestamp_int = int_of_float timestamp in
  let filename = Printf.sprintf "benchmarks/results/%d-%s.json" timestamp_int git_commit in

  (* Ensure directory exists *)
  (try Unix.mkdir "benchmarks/results" 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  let oc = open_out filename in
  output_string oc (Yojson.Safe.pretty_to_string json);
  output_char oc '\n';
  close_out oc;
  Printf.printf "\nResults saved to %s\n" filename

let () =
  let instances = Instance.[ monotonic_clock ] in
  (* Use shorter quota to avoid tree-sitter memory issues *)
  let cfg = Benchmark.cfg ~limit:100 ~quota:(Time.second 0.5) () in

  Printf.printf "Running benchmarks...\n%!";
  let raw_results = Benchmark.all cfg instances benchmarks in

  (* Analyze results with OLS *)
  let ols = Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:[| Measure.run |] in

  (* Analyze for each instance *)
  let analyzed_results = List.map (fun instance ->
    Analyze.all ols instance raw_results
  ) instances in

  (* Print results *)
  Printf.printf "\n%-35s %15s\n" "Benchmark" "ns/iter";
  Printf.printf "%s\n" (String.make 55 '-');

  List.iter (fun analyzed ->
    Hashtbl.iter (fun name ols_result ->
      match Analyze.OLS.estimates ols_result with
      | Some [ v ] ->
        Printf.printf "%-35s %15.2f\n" name v
      | _ -> ()
    ) analyzed
  ) analyzed_results;

  (* Save JSON *)
  save_results_json analyzed_results instances
