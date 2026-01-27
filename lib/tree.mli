(** Internal OCaml tree representation for parsed source code.

    This module provides a pure OCaml data structure for representing
    parsed syntax trees. Unlike the Node module which wraps tree-sitter
    nodes via FFI, this module copies all data into OCaml values,
    simplifying memory management and isolating from tree-sitter API changes. *)

(** Position in source code *)
type point = { row: int; column: int }

(** A child node with optional field name *)
type child = {
  field_name: string option;
  node: t;
}

(** A tree node with all data from the parse *)
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

val node_type : t -> string
val is_named : t -> bool
val start_byte : t -> int
val end_byte : t -> int
val start_point : t -> point
val end_point : t -> point
val children : t -> child list
val named_children : t -> t list

(** [text source node] extracts the source text for a node *)
val text : string -> t -> string

(** {1 Child access} *)

val child_count : t -> int
val named_child_count : t -> int

(** [child t i] returns the i-th child, or None if out of bounds *)
val child : t -> int -> t option

(** [named_child t i] returns the i-th named child, or None if out of bounds *)
val named_child : t -> int -> t option

(** [child_by_field_name t name] returns the child at the given field, or None *)
val child_by_field_name : t -> string -> t option

(** Alias for [child_by_field_name] *)
val field : t -> string -> t option

(** [field_name_for_child t i] returns the field name for child at index i *)
val field_name_for_child : t -> int -> string option

(** {1 Traversal} *)

(** [iter_children f t] applies f to all children *)
val iter_children : (t -> unit) -> t -> unit

(** [iter_named_children f t] applies f to all named children *)
val iter_named_children : (t -> unit) -> t -> unit

(** [fold_children f init t] folds f over all children *)
val fold_children : ('a -> t -> 'a) -> 'a -> t -> 'a

(** [traverse f node] recursively traverses in pre-order, applying f to named nodes *)
val traverse : (t -> unit) -> t -> unit

(** [find_all pred node] returns all nodes matching the predicate *)
val find_all : (t -> bool) -> t -> t list

(** [find_by_type type_name node] returns all nodes of the given type *)
val find_by_type : string -> t -> t list

(** {1 Conversion} *)

(** [of_ts_node source ts_node] converts a tree-sitter node to internal format *)
val of_ts_node : string -> Node.t -> t

(** [of_ts_tree source ts_tree] converts a tree-sitter tree to internal format *)
val of_ts_tree : string -> Node.tree -> tree

(** {1 Parsing} *)

(** [parse ~language source] parses source code and returns the tree *)
val parse : language:string -> string -> tree

(** [parse_file ~language path] parses a file and returns the tree *)
val parse_file : language:string -> string -> tree
