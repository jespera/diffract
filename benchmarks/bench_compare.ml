(** Benchmark comparison tool

    {v
    Usage:
      bench_compare                        # Compare last 2 results
      bench_compare file1.json file2.json  # Compare specific files
      bench_compare --history              # Show history for all benchmarks
      bench_compare --gnuplot              # Generate gnuplot data file
    v} *)

let results_dir = "benchmarks/results"

(* Read a benchmark result JSON file *)
let read_result_file filename =
  let json = Yojson.Safe.from_file filename in
  let open Yojson.Safe.Util in
  let timestamp = json |> member "timestamp" |> to_float in
  let git_commit = json |> member "git_commit" |> to_string in
  let benchmarks =
    json |> member "benchmarks" |> to_list
    |> List.map (fun b ->
        let name = b |> member "name" |> to_string in
        let mean_ns = b |> member "mean_ns" |> to_float in
        (name, mean_ns))
  in
  (timestamp, git_commit, benchmarks)

(* List all result files sorted by timestamp *)
let list_result_files () =
  let files = Sys.readdir results_dir |> Array.to_list in
  files
  |> List.filter (fun f -> Filename.check_suffix f ".json")
  |> List.map (fun f -> Filename.concat results_dir f)
  |> List.sort String.compare

(* Format timestamp as date string *)
let format_timestamp ts =
  let tm = Unix.localtime ts in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min

(* Format nanoseconds nicely *)
let format_ns ns =
  if ns >= 1_000_000_000.0 then Printf.sprintf "%.2f s" (ns /. 1_000_000_000.0)
  else if ns >= 1_000_000.0 then Printf.sprintf "%.2f ms" (ns /. 1_000_000.0)
  else if ns >= 1_000.0 then Printf.sprintf "%.2f us" (ns /. 1_000.0)
  else Printf.sprintf "%.2f ns" ns

(* Calculate percentage change *)
let pct_change old_val new_val =
  if old_val = 0.0 then 0.0 else (new_val -. old_val) /. old_val *. 100.0

(* ANSI color codes *)
let red s = "\027[31m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"
let bold s = "\027[1m" ^ s ^ "\027[0m"

(* Color based on percentage change (positive = slower = red) *)
let color_pct pct =
  let s = Printf.sprintf "%+.1f%%" pct in
  if pct > 5.0 then red s else if pct < -5.0 then green s else yellow s

(* Print ASCII comparison table *)
let print_comparison (ts1, commit1, bench1) (ts2, commit2, bench2) =
  Printf.printf "\n%s\n" (bold "Benchmark Comparison");
  Printf.printf "  Baseline: %s (%s)\n" commit1 (format_timestamp ts1);
  Printf.printf "  Current:  %s (%s)\n\n" commit2 (format_timestamp ts2);

  Printf.printf "%-35s %15s %15s %12s\n" "Benchmark" "Baseline" "Current"
    "Change";
  Printf.printf "%s\n" (String.make 80 '-');

  (* Create map from bench1 for lookup *)
  let bench1_map =
    List.fold_left (fun acc (name, v) -> (name, v) :: acc) [] bench1
  in

  List.iter
    (fun (name, new_val) ->
      match List.assoc_opt name bench1_map with
      | Some old_val ->
          let change = pct_change old_val new_val in
          Printf.printf "%-35s %15s %15s %s\n" name (format_ns old_val)
            (format_ns new_val) (color_pct change)
      | None ->
          Printf.printf "%-35s %15s %15s %s\n" name "(new)" (format_ns new_val)
            "")
    bench2;
  Printf.printf "\n"

(* Print history for all benchmarks *)
let print_history () =
  let files = list_result_files () in
  if List.length files = 0 then begin
    Printf.printf "No benchmark results found in %s\n" results_dir;
    exit 1
  end;

  let results = List.map read_result_file files in

  Printf.printf "\n%s\n\n" (bold "Benchmark History");

  (* Collect all benchmark names *)
  let all_names =
    results
    |> List.concat_map (fun (_, _, benchmarks) -> List.map fst benchmarks)
    |> List.sort_uniq String.compare
  in

  (* Print header *)
  Printf.printf "%-25s" "Benchmark";
  List.iter (fun (_, commit, _) -> Printf.printf " %12s" commit) results;
  Printf.printf "\n";
  Printf.printf "%s\n" (String.make (25 + (13 * List.length results)) '-');

  (* Print each benchmark's history *)
  List.iter
    (fun name ->
      Printf.printf "%-25s"
        (if String.length name > 24 then String.sub name 0 24 else name);
      List.iter
        (fun (_, _, benchmarks) ->
          match List.assoc_opt name benchmarks with
          | Some v -> Printf.printf " %12s" (format_ns v)
          | None -> Printf.printf " %12s" "-")
        results;
      Printf.printf "\n")
    all_names;
  Printf.printf "\n"

(* Generate gnuplot data file *)
let generate_gnuplot_data () =
  let files = list_result_files () in
  if List.length files = 0 then begin
    Printf.printf "No benchmark results found in %s\n" results_dir;
    exit 1
  end;

  let results = List.map read_result_file files in

  (* Collect all benchmark names *)
  let all_names =
    results
    |> List.concat_map (fun (_, _, benchmarks) -> List.map fst benchmarks)
    |> List.sort_uniq String.compare
  in

  (* Write data file *)
  let data_file = "benchmarks/results/benchmark_history.dat" in
  let oc = open_out data_file in

  (* Header comment *)
  Printf.fprintf oc "# Benchmark history data for gnuplot\n";
  Printf.fprintf oc "# Column 1: timestamp\n";
  List.iteri
    (fun i name -> Printf.fprintf oc "# Column %d: %s\n" (i + 2) name)
    all_names;
  Printf.fprintf oc "\n";

  (* Data rows *)
  List.iter
    (fun (ts, _, benchmarks) ->
      Printf.fprintf oc "%.0f" ts;
      List.iter
        (fun name ->
          match List.assoc_opt name benchmarks with
          | Some v ->
              Printf.fprintf oc " %.2f" (v /. 1000.0)
              (* Convert to microseconds *)
          | None -> Printf.fprintf oc " 0")
        all_names;
      Printf.fprintf oc "\n")
    results;

  close_out oc;
  Printf.printf "Generated %s\n" data_file;

  (* Write gnuplot script *)
  let script_file = "benchmarks/results/plot_history.gp" in
  let oc = open_out script_file in
  Printf.fprintf oc "# Gnuplot script for benchmark history\n";
  Printf.fprintf oc "set terminal dumb 120 30\n";
  Printf.fprintf oc "set title 'Benchmark History'\n";
  Printf.fprintf oc "set xlabel 'Time'\n";
  Printf.fprintf oc "set ylabel 'Time (us)'\n";
  Printf.fprintf oc "set xdata time\n";
  Printf.fprintf oc "set timefmt '%%s'\n";
  Printf.fprintf oc "set format x '%%m/%%d'\n";
  Printf.fprintf oc "set key outside right\n";
  Printf.fprintf oc "plot ";
  List.iteri
    (fun i name ->
      if i > 0 then Printf.fprintf oc ", \\\n     ";
      Printf.fprintf oc "'%s' using 1:%d with linespoints title '%s'" data_file
        (i + 2) name)
    all_names;
  Printf.fprintf oc "\n";
  close_out oc;
  Printf.printf "Generated %s\n" script_file;
  Printf.printf "\nTo view ASCII graph: gnuplot %s\n" script_file;
  Printf.printf
    "For PNG output, change 'set terminal dumb' to 'set terminal png' and add \
     'set output \"graph.png\"'\n"

(* Simple ASCII sparkline *)
let sparkline values =
  let min_v = List.fold_left min infinity values in
  let max_v = List.fold_left max neg_infinity values in
  let range = max_v -. min_v in
  let chars = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |] in
  values
  |> List.map (fun v ->
      let normalized = if range = 0.0 then 0.5 else (v -. min_v) /. range in
      let idx = int_of_float (normalized *. 7.0) |> min 7 |> max 0 in
      chars.(idx))
  |> String.concat ""

(* Print mini history with sparklines *)
let print_mini_history () =
  let files = list_result_files () in
  if List.length files = 0 then begin
    Printf.printf "No benchmark results found in %s\n" results_dir;
    exit 1
  end;

  let results = List.map read_result_file files in

  Printf.printf "\n%s\n\n"
    (bold
       (Printf.sprintf "Benchmark Trends (last %d runs)" (List.length results)));

  (* Collect all benchmark names *)
  let all_names =
    results
    |> List.concat_map (fun (_, _, benchmarks) -> List.map fst benchmarks)
    |> List.sort_uniq String.compare
  in

  Printf.printf "%-35s %15s %15s  %s\n" "Benchmark" "First" "Latest" "Trend";
  Printf.printf "%s\n" (String.make 80 '-');

  List.iter
    (fun name ->
      let values =
        List.filter_map
          (fun (_, _, benchmarks) -> List.assoc_opt name benchmarks)
          results
      in
      if List.length values >= 2 then begin
        let first = List.hd values in
        let last = List.hd (List.rev values) in
        let change = pct_change first last in
        Printf.printf "%-35s %15s %15s  %s %s\n" name (format_ns first)
          (format_ns last) (sparkline values) (color_pct change)
      end)
    all_names;
  Printf.printf "\n"

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [ "--history" ] -> print_history ()
  | [ "--gnuplot" ] -> generate_gnuplot_data ()
  | [ "--trends" ] -> print_mini_history ()
  | [ file1; file2 ] ->
      let r1 = read_result_file file1 in
      let r2 = read_result_file file2 in
      print_comparison r1 r2
  | [] ->
      let files = list_result_files () in
      if List.length files < 2 then begin
        Printf.printf "Need at least 2 benchmark results to compare.\n";
        Printf.printf
          "Run benchmarks with: dune exec benchmarks/bench_match.exe\n";
        exit 1
      end;
      let last_two =
        files |> List.rev |> fun l ->
        match l with a :: b :: _ -> [ b; a ] | _ -> l
      in
      let r1 = read_result_file (List.hd last_two) in
      let r2 = read_result_file (List.nth last_two 1) in
      print_comparison r1 r2
  | _ ->
      Printf.printf "Usage:\n";
      Printf.printf
        "  bench_compare                      Compare last 2 results\n";
      Printf.printf
        "  bench_compare file1.json file2.json  Compare specific files\n";
      Printf.printf "  bench_compare --history            Show history table\n";
      Printf.printf
        "  bench_compare --trends             Show trends with sparklines\n";
      Printf.printf
        "  bench_compare --gnuplot            Generate gnuplot data files\n";
      exit 1
