(** Tree-sitter node traversal API *)

(** Abstract type for a parsed tree (GC-managed) *)
type tree

(** Abstract type for a node in the tree *)
type t

(** Position in source code (0-indexed) *)
type point = { row: int; column: int }

(** {1 Tree operations} *)

val parse : nativeint -> string -> tree
(** [parse parser_ptr source] parses source code and returns a tree.
    The parser must have a language set. *)

val root : tree -> t
(** [root tree] returns the root node of the tree. *)

(** {1 Node properties} *)

val node_type : t -> string
(** [node_type node] returns the grammar type of the node (e.g., "function_declaration"). *)

val to_sexp : t -> string
(** [to_sexp node] returns the S-expression representation of the node and its children. *)

val is_named : t -> bool
(** [is_named node] returns true if this is a named node (vs anonymous punctuation). *)

val is_null : t -> bool
(** [is_null node] returns true if this is a null/missing node. *)

(** {1 Child access} *)

val child_count : t -> int
(** [child_count node] returns the total number of children (including anonymous). *)

val named_child_count : t -> int
(** [named_child_count node] returns the number of named children. *)

val child : t -> int -> t
(** [child node i] returns the i-th child (0-indexed). *)

val named_child : t -> int -> t
(** [named_child node i] returns the i-th named child (0-indexed). *)

val child_by_field_name : t -> string -> t
(** [child_by_field_name node name] returns the child with the given field name.
    Returns a null node if not found (check with [is_null]). *)

val field_name_for_child : t -> int -> string option
(** [field_name_for_child node i] returns the field name for the i-th child, if any. *)

(** {1 Tree navigation} *)

val parent : t -> t
(** [parent node] returns the parent node (null if at root). *)

val next_sibling : t -> t
(** [next_sibling node] returns the next sibling (null if none). *)

val prev_sibling : t -> t
(** [prev_sibling node] returns the previous sibling (null if none). *)

val next_named_sibling : t -> t
(** [next_named_sibling node] returns the next named sibling. *)

val prev_named_sibling : t -> t
(** [prev_named_sibling node] returns the previous named sibling. *)

(** {1 Position information} *)

val start_byte : t -> int
(** [start_byte node] returns the byte offset where this node starts. *)

val end_byte : t -> int
(** [end_byte node] returns the byte offset where this node ends. *)

val start_point : t -> point
(** [start_point node] returns the (row, column) where this node starts. *)

val end_point : t -> point
(** [end_point node] returns the (row, column) where this node ends. *)

(** {1 Convenience functions} *)

val children : t -> t list
(** [children node] returns all children as a list. *)

val named_children : t -> t list
(** [named_children node] returns all named children as a list. *)

val text : string -> t -> string
(** [text source node] extracts the source text for this node. *)

val iter_children : (t -> unit) -> t -> unit
(** [iter_children f node] calls [f] on each child. *)

val iter_named_children : (t -> unit) -> t -> unit
(** [iter_named_children f node] calls [f] on each named child. *)

val fold_children : ('a -> t -> 'a) -> 'a -> t -> 'a
(** [fold_children f init node] folds over all children. *)

val traverse : (t -> unit) -> t -> unit
(** [traverse f node] recursively traverses the tree in pre-order,
    visiting only named nodes. *)

val find_all : (t -> bool) -> t -> t list
(** [find_all pred node] finds all descendant nodes matching the predicate. *)

val find_by_type : string -> t -> t list
(** [find_by_type type_name node] finds all descendants with the given node type. *)

val field : string -> t -> t option
(** [field name node] returns the child with the given field name as an option. *)
