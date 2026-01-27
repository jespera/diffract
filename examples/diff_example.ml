(* Example: Diff two TypeScript code snippets *)

let before = {|
function hello(name: string): void {
  console.log("Hello, " + name);
}

function add(a: number, b: number): number {
  return a + b;
}
|}

let after = {|
function hello(name: string): void {
  console.warn("Hello, " + name);
}

function add(a: number, b: number, c: number): number {
  return a + b + c;
}

function subtract(a: number, b: number): number {
  return a - b;
}
|}

let () =
  let result = Diffract.diff ~language:"typescript" ~before ~after in

  print_endline "=== Diff Result ===\n";
  print_endline (Diffract.Diff.to_string result);

  print_endline "\n=== Flattened Changes ===\n";
  let flat = Diffract.Diff.flatten_changes result.changes in
  List.iter (fun change ->
    let ctx = Diffract.Diff.change_context change in
    let node_type = Diffract.Diff.change_node_type change in
    let before_text = Diffract.Diff.change_text_before result change in
    let after_text = Diffract.Diff.change_text_after result change in
    Printf.printf "  %s (%s):\n" node_type
      (match ctx.parent_type with Some p -> p | None -> "root");
    (match before_text with
     | Some t -> Printf.printf "    - %s\n" (String.escaped t)
     | None -> ());
    (match after_text with
     | Some t -> Printf.printf "    + %s\n" (String.escaped t)
     | None -> ());
    print_newline ()
  ) flat
