(** Scaling test for sequence matching *)

(* Generate source code with N methods in a class *)
let generate_class_source n =
  let methods =
    List.init n (fun i ->
        Printf.sprintf "  method%d() { console.log(\"msg%d\"); }" i i)
    |> String.concat "\n"
  in
  Printf.sprintf "class TestClass {\n%s\n}" methods

let ctx = Diffract.Context.create ()

(* Sequence pattern for class matching *)
let sequence_pattern =
  {|@@
match: strict
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }|}

(* Nested pattern with sequence *)
let nested_sequence_pattern =
  {|@@
match: strict
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }

@@
match: strict
metavar $msg: single
@@
console.log($msg)|}

(* Time a single run with aggressive GC *)
let time_once f =
  Gc.full_major ();
  Gc.compact ();
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  ignore result;
  Gc.full_major ();
  elapsed

(* Time multiple runs, return median *)
let time_median n f =
  let times =
    Array.init n (fun _ ->
        Printf.printf ".%!";
        time_once f)
  in
  Printf.printf " ";
  Array.sort compare times;
  times.(n / 2)

let () =
  Printf.printf "\n%s\n" "=== Sequence Matching Scaling Test ===";
  Printf.printf "\n%-10s %15s %15s %10s\n" "Children" "Sequence (ms)"
    "Nested (ms)" "Ratio";
  Printf.printf "%s\n" (String.make 55 '-');

  let sizes = [ 2; 5; 10; 20; 50; 100; 200 ] in
  let prev_seq = ref 0.0 in

  List.iter
    (fun n ->
      Printf.printf "n=%-3d " n;
      let source = generate_class_source n in

      (* Sequence benchmark - just 3 runs *)
      let seq_time =
        time_median 3 (fun () ->
            Diffract.Match.find_matches ~ctx ~language:"typescript"
              ~pattern_text:sequence_pattern ~source_text:source)
      in

      (* Nested benchmark *)
      let nested_time =
        time_median 3 (fun () ->
            Diffract.Match.find_nested_matches ~ctx ~language:"typescript"
              ~pattern_text:nested_sequence_pattern ~source_text:source)
      in

      let ratio = if !prev_seq > 0.0 then seq_time /. !prev_seq else 1.0 in
      prev_seq := seq_time;

      Printf.printf "=> %-10d %12.2f ms %12.2f ms %10.2fx\n" n
        (seq_time *. 1000.0) (nested_time *. 1000.0) ratio)
    sizes;

  Printf.printf "\n%s\n" "=== Analysis ===";
  Printf.printf "If ratio is ~constant: O(n) scaling\n";
  Printf.printf "If ratio doubles when n doubles: O(n²) scaling\n";
  Printf.printf "If ratio more than doubles: O(n^k) for k>2\n"
