(** Internal OCaml tree representation for parsed source code *)

(** Position in source code *)
type point = { row: int; column: int }

(** A child node with optional field name *)
type child = {
  field_name: string option;
  node: t;
}

(** A tree node with all data copied from tree-sitter *)
and t = {
  node_type: string;
  is_named: bool;
  start_byte: int;
  end_byte: int;
  start_point: point;
  end_point: point;
  children: child list;
  named_children: t list;
}

(** A complete parsed tree with source *)
type tree = {
  root: t;
  source: string;
}

(** {1 Accessors} *)

let node_type t = t.node_type
let is_named t = t.is_named
let start_byte t = t.start_byte
let end_byte t = t.end_byte
let start_point t = t.start_point
let end_point t = t.end_point
let children t = t.children
let named_children t = t.named_children

(** Get text content of a node from source *)
let text source node =
  let start = node.start_byte in
  let len = node.end_byte - start in
  String.sub source start len

(** {1 Child access} *)

let child_count t = List.length t.children
let named_child_count t = List.length t.named_children

(** Get child by index *)
let child t i =
  match List.nth_opt t.children i with
  | Some c -> Some c.node
  | None -> None

(** Get named child by index *)
let named_child t i =
  List.nth_opt t.named_children i

(** Get child by field name *)
let child_by_field_name t name =
  List.find_map (fun c ->
    match c.field_name with
    | Some n when n = name -> Some c.node
    | _ -> None
  ) t.children

(** Alias for child_by_field_name *)
let field = child_by_field_name

(** Get field name for child at index *)
let field_name_for_child t i =
  match List.nth_opt t.children i with
  | Some c -> c.field_name
  | None -> None

(** {1 Traversal} *)

(** Iterate over all children *)
let iter_children f t =
  List.iter (fun c -> f c.node) t.children

(** Iterate over named children *)
let iter_named_children f t =
  List.iter f t.named_children

(** Fold over all children *)
let fold_children f init t =
  List.fold_left (fun acc c -> f acc c.node) init t.children

(** Recursively traverse the tree in pre-order (named nodes only) *)
let rec traverse f node =
  f node;
  iter_named_children (traverse f) node

(** Find all nodes matching a predicate *)
let find_all pred node =
  let results = ref [] in
  traverse (fun n -> if pred n then results := n :: !results) node;
  List.rev !results

(** Find all nodes of a specific type *)
let find_by_type type_name node =
  find_all (fun n -> n.node_type = type_name) node

(** {1 Conversion from tree-sitter nodes} *)

(** Convert a tree-sitter node to our internal representation *)
let rec of_ts_node source (ts_node : Node.t) : t =
  let node_type = Node.node_type ts_node in
  let is_named = Node.is_named ts_node in
  let start_byte = Node.start_byte ts_node in
  let end_byte = Node.end_byte ts_node in
  let ts_start = Node.start_point ts_node in
  let ts_end = Node.end_point ts_node in
  let start_point = { row = ts_start.row; column = ts_start.column } in
  let end_point = { row = ts_end.row; column = ts_end.column } in

  (* Convert all children with field names *)
  let child_count = Node.child_count ts_node in
  let children = List.init child_count (fun i ->
    let ts_child = Node.child ts_node i in
    let field_name = Node.field_name_for_child ts_node i in
    { field_name; node = of_ts_node source ts_child }
  ) in

  (* Extract named children *)
  let named_children = List.filter_map (fun c ->
    if c.node.is_named then Some c.node else None
  ) children in

  { node_type; is_named; start_byte; end_byte; start_point; end_point;
    children; named_children }

(** Convert a tree-sitter tree to our internal representation *)
let of_ts_tree source (ts_tree : Node.tree) : tree =
  let ts_root = Node.root ts_tree in
  let root = of_ts_node source ts_root in
  { root; source }

(** {1 Parsing} *)

(** Parse source code and return our internal tree representation *)
let parse ~language source =
  let lang = Languages.get language in
  let parser = Tree_sitter_bindings.parser_new () in

  if parser = 0n then
    failwith "Failed to create parser";

  if not (Tree_sitter_bindings.parser_set_language parser lang) then begin
    Tree_sitter_bindings.parser_delete parser;
    failwith "Failed to set language"
  end;

  let ts_tree = Node.parse parser source in
  Tree_sitter_bindings.parser_delete parser;
  of_ts_tree source ts_tree

(** Parse a file and return our internal tree representation *)
let parse_file ~language path =
  let source = In_channel.with_open_text path In_channel.input_all in
  parse ~language source
