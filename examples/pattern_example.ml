(* Example: Pattern matching on TypeScript AST *)

open Diffract
open Diffract.Pattern

let source = {|
function hello(name: string): void {
  console.log("Hello, " + name);
}

function goodbye(name: string): void {
  console.log("Goodbye, " + name);
}

const add = (a: number, b: number) => a + b;

console.log("direct call");
console.warn("warning!");
|}

let () =
  let tree = Diffract.parse_tree ~language:"typescript" source in
  let source = tree.source in
  let root = tree.root in

  (* Pattern: Match function declarations and capture name *)
  let func_pattern = Pattern.(
    node "function_declaration" [
      field "name" (node "identifier" [capture "func_name"])
    ]
  ) in

  print_endline "=== Function declarations ===";
  let matches = Pattern.find_all source func_pattern root in
  List.iter (fun m ->
    match Pattern.get_capture "func_name" m with
    | Some name_node ->
      let name = Tree.text source name_node in
      let pos = Tree.start_point m.node in
      Printf.printf "  %s (line %d)\n" name (pos.row + 1)
    | None -> ()
  ) matches;

  (* Pattern: Match console.log calls *)
  let console_log_pattern = Pattern.(
    node "call_expression" [
      field "function" (
        node "member_expression" [
          field "object" (node "identifier" [has_text "console"]);
          field "property" (node "property_identifier" [has_text "log"])
        ]
      );
      field "arguments" (any_node [capture "args"])
    ]
  ) in

  print_endline "\n=== console.log calls ===";
  let matches = Pattern.find_all source console_log_pattern root in
  List.iter (fun m ->
    let pos = Tree.start_point m.node in
    Printf.printf "  line %d: %s\n" (pos.row + 1) (Tree.text source m.node)
  ) matches;

  (* Pattern: Match any console.* call *)
  let any_console_pattern = Pattern.(
    node "call_expression" [
      capture "call";
      field "function" (
        node "member_expression" [
          field "object" (node "identifier" [has_text "console"]);
          field "property" (node "property_identifier" [capture "method"])
        ]
      )
    ]
  ) in

  print_endline "\n=== All console.* calls ===";
  let matches = Pattern.find_all source any_console_pattern root in
  List.iter (fun m ->
    match Pattern.get_capture "method" m with
    | Some method_node ->
      let method_name = Tree.text source method_node in
      let pos = Tree.start_point m.node in
      Printf.printf "  console.%s at line %d\n" method_name (pos.row + 1)
    | None -> ()
  ) matches;

  (* Pattern: Match arrow functions *)
  let arrow_pattern = Pattern.(
    node "arrow_function" [capture "arrow"]
  ) in

  print_endline "\n=== Arrow functions ===";
  let matches = Pattern.find_all source arrow_pattern root in
  List.iter (fun m ->
    let pos = Tree.start_point m.node in
    Printf.printf "  line %d: %s\n" (pos.row + 1) (Tree.text source m.node)
  ) matches;

  (* Pattern: Match string literals containing "Hello" *)
  let hello_strings = Pattern.(
    node "string" [
      text_matches (fun s -> String.length s >= 6 && String.sub s 0 6 = "\"Hello");
      capture "str"
    ]
  ) in

  print_endline "\n=== Strings starting with 'Hello' ===";
  let matches = Pattern.find_all source hello_strings root in
  List.iter (fun m ->
    match Pattern.get_capture "str" m with
    | Some str_node ->
      Printf.printf "  %s\n" (Tree.text source str_node)
    | None -> ()
  ) matches
