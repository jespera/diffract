(** Benchmark for [Matcher.apply_edits].

    [apply_edits] splices an edit list into a source string. The original
    implementation rebuilt the whole string per edit (a [String.sub]/[^] fold),
    which is O(edits x size); it now does a single forward Buffer pass, O(size).
    This benchmark drives the public API with synthetic inputs to show the
    scaling.

    Run: dune exec bench/apply_edits.exe *)

open Diffract

let time ?(warmup = 1) ?(iters = 5) f =
  for _ = 1 to warmup do
    ignore (Sys.opaque_identity (f ()))
  done;
  let samples =
    List.init iters (fun _ ->
        let t0 = Unix.gettimeofday () in
        ignore (Sys.opaque_identity (f ()));
        (Unix.gettimeofday () -. t0) *. 1000.0)
  in
  let sorted = List.sort compare samples in
  (List.hd sorted, List.nth sorted (iters / 2))

(* A source of [n] bytes and [e] non-overlapping single-byte edits spread
   evenly across it. Each edit grows its span (1 byte -> 2), the shape the
   surgical-edit path produces. *)
let make_input n e =
  let source = String.make n 'x' in
  let spacing = n / e in
  let edits =
    List.init e (fun i ->
        let pos = i * spacing in
        { Matcher.start_byte = pos; end_byte = pos + 1; replacement = "YY" })
  in
  (source, edits)

let () =
  Printf.printf "\nMatcher.apply_edits  (source bytes x #edits)\n";
  Printf.printf "  %-22s | %12s | %12s\n" "size" "best ms" "median ms";
  Printf.printf "  %s\n" (String.make 52 '-');
  List.iter
    (fun (n, e) ->
      let source, edits = make_input n e in
      let best, med = time (fun () -> Matcher.apply_edits source edits) in
      Printf.printf "  %7d B x %5d        | %12.3f | %12.3f\n" n e best med)
    [ (50_000, 500); (200_000, 2_000); (500_000, 4_000); (1_000_000, 8_000) ];
  Printf.printf "\n"
