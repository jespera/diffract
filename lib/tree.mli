(** Internal OCaml tree representation for parsed source code.

    This module provides a pure OCaml data structure for representing
    parsed syntax trees. Unlike the Node module which wraps tree-sitter
    nodes via FFI, this module copies all data into OCaml values,
    simplifying memory management and isolating from tree-sitter API changes.

    Trees and nodes are parameterized by a phantom type to distinguish
    source trees from pattern trees at compile time. *)

(** {1 Phantom types}

    These types have no runtime representation but distinguish trees
    and nodes at compile time. *)

type src
(** Phantom type for source code trees/nodes *)

type pat
(** Phantom type for pattern trees/nodes (containing metavariables) *)

type any
(** Phantom type for trees/nodes of unknown kind *)

(** {1 Tree types} *)

(** Position in source code *)
type point = { row: int; column: int }

(** A child node with optional field name.
    The ['kind] parameter matches the parent node's kind. *)
type 'kind child = {
  field_name: string option;
  node: 'kind t;
}

(** A tree node with all data from the parse.
    The ['kind] parameter distinguishes source nodes from pattern nodes. *)
and 'kind t = {
  node_type: string;
  is_named: bool;
  start_byte: int;
  end_byte: int;
  start_point: point;
  end_point: point;
  children: 'kind child list;
  named_children: 'kind t list;
}

(** A complete parsed tree with source text.
    The ['kind] parameter distinguishes source trees from pattern trees. *)
type 'kind tree = {
  root: 'kind t;
  source: string;
}

(** {1 Accessors} *)

val node_type : _ t -> string
val is_named : _ t -> bool
val start_byte : _ t -> int
val end_byte : _ t -> int
val start_point : _ t -> point
val end_point : _ t -> point
val children : 'kind t -> 'kind child list
val named_children : 'kind t -> 'kind t list

(** [text source node] extracts the source text for a node *)
val text : string -> _ t -> string

(** {1 Child access} *)

val child_count : _ t -> int
val named_child_count : _ t -> int

(** [child t i] returns the i-th child, or None if out of bounds *)
val child : 'kind t -> int -> 'kind t option

(** [named_child t i] returns the i-th named child, or None if out of bounds *)
val named_child : 'kind t -> int -> 'kind t option

(** [child_by_field_name t name] returns the child at the given field, or None *)
val child_by_field_name : 'kind t -> string -> 'kind t option

(** Alias for [child_by_field_name] *)
val field : 'kind t -> string -> 'kind t option

(** [field_name_for_child t i] returns the field name for child at index i *)
val field_name_for_child : _ t -> int -> string option

(** [named_children_with_fields t] returns named children paired with their field names *)
val named_children_with_fields : 'kind t -> (string option * 'kind t) list

(** {1 Traversal} *)

(** [iter_children f t] applies f to all children *)
val iter_children : ('kind t -> unit) -> 'kind t -> unit

(** [iter_named_children f t] applies f to all named children *)
val iter_named_children : ('kind t -> unit) -> 'kind t -> unit

(** [fold_children f init t] folds f over all children *)
val fold_children : ('a -> 'kind t -> 'a) -> 'a -> 'kind t -> 'a

(** [traverse f node] recursively traverses in pre-order, applying f to named nodes *)
val traverse : ('kind t -> unit) -> 'kind t -> unit

(** [find_all pred node] returns all nodes matching the predicate *)
val find_all : ('kind t -> bool) -> 'kind t -> 'kind t list

(** [find_by_type type_name node] returns all nodes of the given type *)
val find_by_type : string -> 'kind t -> 'kind t list

(** {1 Parse error detection} *)

(** [is_error node] returns true if the node is an ERROR node (parse failure) *)
val is_error : _ t -> bool

(** [has_errors tree] returns true if the tree contains any ERROR nodes *)
val has_errors : _ tree -> bool

(** [error_count tree] returns the number of ERROR nodes in the tree *)
val error_count : _ tree -> int

(** [get_errors tree] returns all ERROR nodes with their positions *)
val get_errors : 'kind tree -> 'kind t list

(** {1 Formatting} *)

(** [format_tree tree] formats a tree as an indented string showing:
    - Node types with [line:col-line:col] positions
    - Field names for children where applicable
    - Text content for leaf nodes (truncated if long)
    - ERROR nodes marked and showing their text content

    Example output:
    {v
    program [1:1-1:17]
      lexical_declaration [1:1-1:17]
        variable_declarator [1:7-1:16]
          name:
            identifier [1:7-1:8] "x"
          value:
            number [1:11-1:12] "42"
    v} *)
val format_tree : _ tree -> string

(** {1 Phantom type conversion} *)

val forget_node : _ t -> any t
(** [forget_node node] erases the source/pattern distinction on a node. *)

val forget : _ tree -> any tree
(** [forget tree] erases the source/pattern distinction.
    Use when you need to treat trees uniformly (e.g., generic utilities). *)

(** {1 Conversion from tree-sitter} *)

(** [of_ts_node source ts_node] converts a tree-sitter node to internal format. *)
val of_ts_node : string -> Node.t -> src t

(** [of_ts_tree source ts_tree] converts a tree-sitter tree to internal format *)
val of_ts_tree : string -> Node.tree -> src tree

(** {1 Parsing} *)

(** [parse ~language source] parses source code and returns a source tree *)
val parse : language:string -> string -> src tree

(** [parse_file ~language path] parses a file and returns a source tree *)
val parse_file : language:string -> string -> src tree

(** [parse_as_pattern ~language source] parses pattern code and returns a pattern tree.
    The parsing is identical to [parse], but the result is typed as a pattern tree. *)
val parse_as_pattern : language:string -> string -> pat tree
