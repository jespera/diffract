(* Example: Traverse a TypeScript file and print function names *)

let () =
  let ctx = Diffract.Context.create () in
  let source = {|
function hello(name: string): void {
  console.log("Hello, " + name);
}

function goodbye(name: string): void {
  console.log("Goodbye, " + name);
}

const greet = (x: string) => console.log(x);
|} in

  let tree = Diffract.parse_tree ~ctx ~language:"typescript" source in
  let root = tree.root in

  Printf.printf "Root node type: %s\n" root.Diffract.Tree.node_type;
  Printf.printf "Number of children: %d\n\n" (List.length root.Diffract.Tree.named_children);

  (* Find all function declarations *)
  let functions = Diffract.Tree.find_by_type "function_declaration" root in
  Printf.printf "Found %d function declarations:\n" (List.length functions);
  List.iter (fun fn ->
    match Diffract.Tree.child_by_field_name fn "name" with
    | Some name_node ->
      let name = Diffract.Tree.text tree.source name_node in
      Printf.printf "  - %s (line %d)\n" name (name_node.start_point.row + 1)
    | None ->
      Printf.printf "  - anonymous function\n"
  ) functions;

  (* Find all identifiers *)
  print_endline "\nAll identifiers:";
  let identifiers = Diffract.Tree.find_by_type "identifier" root in
  List.iter (fun id ->
    Printf.printf "  %s\n" (Diffract.Tree.text tree.source id)
  ) identifiers;

  (* Custom traversal: find all string literals *)
  print_endline "\nString literals:";
  Diffract.Tree.traverse (fun node ->
    if node.Diffract.Tree.node_type = "string" then
      Printf.printf "  %s\n" (Diffract.Tree.text tree.source node)
  ) root
