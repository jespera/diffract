(** Tree-sitter node traversal API *)

(** Abstract types for tree and node *)
type tree
type t  (* node *)

(** Position in source code *)
type point = { row: int; column: int }

(** {1 Tree operations} *)

external parse : nativeint -> string -> tree = "ts_helper_parse"
external root : tree -> t = "ts_helper_tree_root_node"

(** {1 Node properties} *)

external node_type : t -> string = "ts_helper_node_type"
external to_sexp : t -> string = "ts_helper_node_string"
external is_named : t -> bool = "ts_helper_node_is_named"
external is_null : t -> bool = "ts_helper_node_is_null"

(** {1 Child access} *)

external child_count : t -> int = "ts_helper_node_child_count"
external named_child_count : t -> int = "ts_helper_node_named_child_count"
external child : t -> int -> t = "ts_helper_node_child"
external named_child : t -> int -> t = "ts_helper_node_named_child"
external child_by_field_name : t -> string -> t = "ts_helper_node_child_by_field_name"
external field_name_for_child : t -> int -> string option = "ts_helper_node_field_name_for_child"

(** {1 Tree navigation} *)

external parent : t -> t = "ts_helper_node_parent"
external next_sibling : t -> t = "ts_helper_node_next_sibling"
external prev_sibling : t -> t = "ts_helper_node_prev_sibling"
external next_named_sibling : t -> t = "ts_helper_node_next_named_sibling"
external prev_named_sibling : t -> t = "ts_helper_node_prev_named_sibling"

(** {1 Position information} *)

external start_byte : t -> int = "ts_helper_node_start_byte"
external end_byte : t -> int = "ts_helper_node_end_byte"
external start_point_raw : t -> int * int = "ts_helper_node_start_point"
external end_point_raw : t -> int * int = "ts_helper_node_end_point"

let start_point node =
  let (row, column) = start_point_raw node in
  { row; column }

let end_point node =
  let (row, column) = end_point_raw node in
  { row; column }

(** {1 Convenience functions} *)

(** Get all children as a list *)
let children node =
  let count = child_count node in
  List.init count (fun i -> child node i)

(** Get all named children as a list *)
let named_children node =
  let count = named_child_count node in
  List.init count (fun i -> named_child node i)

(** Get text content of a node from source *)
let text source node =
  let start = start_byte node in
  let len = end_byte node - start in
  String.sub source start len

(** Iterate over all children *)
let iter_children f node =
  for i = 0 to child_count node - 1 do
    f (child node i)
  done

(** Iterate over named children *)
let iter_named_children f node =
  for i = 0 to named_child_count node - 1 do
    f (named_child node i)
  done

(** Fold over all children *)
let fold_children f init node =
  let acc = ref init in
  for i = 0 to child_count node - 1 do
    acc := f !acc (child node i)
  done;
  !acc

(** Recursively traverse the tree in pre-order *)
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
  find_all (fun n -> node_type n = type_name) node

(** Get child by field name, returning option *)
let field name node =
  let child = child_by_field_name node name in
  if is_null child then None else Some child
