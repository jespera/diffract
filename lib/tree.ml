(** Internal OCaml tree representation for parsed source code *)

type src = unit
(** Phantom type markers - these have no runtime representation. We use unit as
    the representation since the type is never constructed. *)

type pat = unit
type any = unit

type point = { row : int; column : int }
(** Position in source code *)

type 'kind child = { field_name : string option; node : 'kind t }
(** A child node with optional field name *)

and 'kind t = {
  node_type : string;
  is_named : bool;
  start_byte : int;
  end_byte : int;
  start_point : point;
  end_point : point;
  children : 'kind child list;
  named_children : 'kind t list;
}
(** A tree node with all data copied from tree-sitter *)

type 'kind tree = { root : 'kind t; source : string }
(** A complete parsed tree with source. The 'kind parameter is a phantom type -
    it's never used at runtime. *)

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
  match List.nth_opt t.children i with Some c -> Some c.node | None -> None

(** Get named child by index *)
let named_child t i = List.nth_opt t.named_children i

(** Get child by field name *)
let child_by_field_name t name =
  List.find_map
    (fun c ->
      match c.field_name with Some n when n = name -> Some c.node | _ -> None)
    t.children

(** Alias for child_by_field_name *)
let field = child_by_field_name

(** Get field name for child at index *)
let field_name_for_child t i =
  match List.nth_opt t.children i with Some c -> c.field_name | None -> None

(** Get named children with their field names preserved *)
let named_children_with_fields t =
  List.filter_map
    (fun c -> if c.node.is_named then Some (c.field_name, c.node) else None)
    t.children

(** {1 Traversal} *)

(** Iterate over all children *)
let iter_children f t = List.iter (fun c -> f c.node) t.children

(** Iterate over named children *)
let iter_named_children f t = List.iter f t.named_children

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

(** {1 Parse error detection} *)

(** Check if a node is an ERROR node (parse failure) *)
let is_error node = node.node_type = "ERROR"

(** Check if a tree has any parse errors *)
let has_errors tree =
  let found = ref false in
  traverse (fun n -> if is_error n then found := true) tree.root;
  !found

(** Count the number of ERROR nodes in a tree *)
let error_count tree =
  let count = ref 0 in
  traverse (fun n -> if is_error n then incr count) tree.root;
  !count

(** Get all ERROR nodes with their positions *)
let get_errors tree = find_all is_error tree.root

(** {1 Formatting} *)

(** Escape a string for display (newlines, tabs, etc.) *)
let escape_string s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | c when Char.code c < 32 ->
          Buffer.add_string buf (Printf.sprintf "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(** Format a tree as an indented string with positions and text. Shows node
    types, field names, line:column positions, and text for leaf nodes. ERROR
    nodes are marked for visibility. *)
let format_tree tree =
  let buf = Buffer.create 4096 in
  let source = tree.source in
  let rec format_node indent node =
    let indent_str = String.make indent ' ' in
    let pos =
      Printf.sprintf "[%d:%d-%d:%d]" (node.start_point.row + 1)
        (node.start_point.column + 1)
        (node.end_point.row + 1)
        (node.end_point.column + 1)
    in
    let node_text = text source node in
    let is_leaf = node.named_children = [] in
    let is_err = is_error node in
    (* Format the node line *)
    Buffer.add_string buf indent_str;
    if is_err then Buffer.add_string buf "ERROR ";
    Buffer.add_string buf node.node_type;
    Buffer.add_string buf " ";
    Buffer.add_string buf pos;
    (* Show text for leaf nodes and ERROR nodes *)
    if is_leaf || is_err then begin
      let display_text =
        if String.length node_text > 60 then String.sub node_text 0 57 ^ "..."
        else node_text
      in
      Buffer.add_string buf " \"";
      Buffer.add_string buf (escape_string display_text);
      Buffer.add_char buf '"'
    end;
    Buffer.add_char buf '\n';
    (* Recurse into children, showing field names *)
    List.iter
      (fun child ->
        if child.node.is_named then begin
          match child.field_name with
          | Some name ->
              let child_indent = String.make (indent + 2) ' ' in
              Buffer.add_string buf child_indent;
              Buffer.add_string buf name;
              Buffer.add_string buf ":\n";
              format_node (indent + 4) child.node
          | None -> format_node (indent + 2) child.node
        end)
      node.children
  in
  format_node 0 tree.root;
  Buffer.contents buf

(** {1 Unwrapping} *)

(** Unwrap parser wrapper nodes (program, module, expression_statement, etc.) to
    get to the innermost meaningful content node. Handles PHP's php_tag prefix
    in program nodes. *)
let unwrap_root (node : 'kind t) : 'kind t =
  let rec unwrap node =
    let children = named_children node in
    match (node.node_type, children) with
    | ("program" | "module" | "source_file" | "compilation_unit"), [ child ] ->
        unwrap child
    | "expression_statement", [ child ] -> unwrap child
    | "program", first :: rest when first.node_type = "php_tag" -> (
        match rest with [ child ] -> unwrap child | _ -> node)
    | _ -> node
  in
  unwrap node

(** {1 Phantom type conversion} *)

(** Erase the source/pattern distinction on a node. Safe because phantom types
    have identical runtime representation. We use Obj.magic since all phantom
    types have the same runtime representation. *)
let forget_node : 'a t -> any t = fun node -> Obj.magic node

(** Erase the source/pattern distinction on a tree. *)
let forget : 'a tree -> any tree = fun tree -> Obj.magic tree

(** {1 Conversion from tree-sitter nodes} *)

(** Convert a tree-sitter node to our internal representation (untyped). *)
let rec of_ts_node_internal source (ts_node : Node.t) =
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
  let children =
    List.init child_count (fun i ->
        let ts_child = Node.child ts_node i in
        let field_name = Node.field_name_for_child ts_node i in
        { field_name; node = of_ts_node_internal source ts_child })
  in

  (* Extract named children *)
  let named_children =
    List.filter_map
      (fun c -> if c.node.is_named then Some c.node else None)
      children
  in

  {
    node_type;
    is_named;
    start_byte;
    end_byte;
    start_point;
    end_point;
    children;
    named_children;
  }

(** Convert a tree-sitter node to source node *)
let of_ts_node source ts_node : src t = of_ts_node_internal source ts_node

(** Convert a tree-sitter tree to our internal representation *)
let of_ts_tree source (ts_tree : Node.tree) : src tree =
  let ts_root = Node.root ts_tree in
  let root = of_ts_node source ts_root in
  ignore (Sys.opaque_identity ts_tree);
  { root; source }

(** {1 Parsing} *)

(** Internal parsing function that returns an untyped tree record *)
let parse_internal ~ctx ~language source =
  let lang = Languages.get ctx language in
  let parser = Tree_sitter_bindings.parser_new () in

  if parser = 0n then failwith "Failed to create parser";

  if not (Tree_sitter_bindings.parser_set_language parser lang) then begin
    Tree_sitter_bindings.parser_delete parser;
    failwith "Failed to set language"
  end;

  let ts_tree = Node.parse parser source in
  Tree_sitter_bindings.parser_delete parser;
  let ts_root = Node.root ts_tree in
  let root = of_ts_node_internal source ts_root in
  ignore (Sys.opaque_identity ts_tree);
  { root; source }

(** Parse source code and return our internal tree representation *)
let parse ~ctx ~language source : src tree =
  parse_internal ~ctx ~language source

(** Parse a file and return our internal tree representation *)
let parse_file ~ctx ~language path : src tree =
  let source = In_channel.with_open_text path In_channel.input_all in
  parse ~ctx ~language source

(** Parse pattern code and return a pattern tree. Identical parsing, but typed
    as a pattern tree. *)
let parse_as_pattern ~ctx ~language source : pat tree =
  parse_internal ~ctx ~language source
