(** Tree-sitter node traversal API *)

(** Abstract type for a parsed tree (GC-managed) *)
type tree

(** Abstract type for a node in the tree *)
type t

(** Position in source code (0-indexed) *)
type point = { row : int; column : int }

(** {1 Tree operations} *)

(** [parse parser_ptr source] parses source code and returns a tree. The parser
    must have a language set. *)
val parse : nativeint -> string -> tree

(** [root tree] returns the root node of the tree. *)
val root : tree -> t

(** {1 Node properties} *)

(** [node_type node] returns the grammar type of the node (e.g.,
    "function_declaration"). *)
val node_type : t -> string

(** [to_sexp node] returns the S-expression representation of the node and its
    children. *)
val to_sexp : t -> string

(** [is_named node] returns true if this is a named node (vs anonymous
    punctuation). *)
val is_named : t -> bool

(** [is_extra node] returns true if the node was generated from one of the
    grammar's [extras] rules — typically comments and (rarely) whitespace
    tokens. Such nodes can appear anywhere between tokens without being part of
    the syntactic structure. *)
val is_extra : t -> bool

(** [is_null node] returns true if this is a null/missing node. *)
val is_null : t -> bool

(** {1 Child access} *)

(** [child_count node] returns the total number of children (including
    anonymous). *)
val child_count : t -> int

(** [named_child_count node] returns the number of named children. *)
val named_child_count : t -> int

(** [child node i] returns the i-th child (0-indexed). *)
val child : t -> int -> t

(** [named_child node i] returns the i-th named child (0-indexed). *)
val named_child : t -> int -> t

(** [child_by_field_name node name] returns the child with the given field name.
    Returns a null node if not found (check with [is_null]). *)
val child_by_field_name : t -> string -> t

(** [field_name_for_child node i] returns the field name for the i-th child, if
    any. *)
val field_name_for_child : t -> int -> string option

(** {1 Tree navigation} *)

(** [parent node] returns the parent node (null if at root). *)
val parent : t -> t

(** [next_sibling node] returns the next sibling (null if none). *)
val next_sibling : t -> t

(** [prev_sibling node] returns the previous sibling (null if none). *)
val prev_sibling : t -> t

(** [next_named_sibling node] returns the next named sibling. *)
val next_named_sibling : t -> t

(** [prev_named_sibling node] returns the previous named sibling. *)
val prev_named_sibling : t -> t

(** {1 Position information} *)

(** [start_byte node] returns the byte offset where this node starts. *)
val start_byte : t -> int

(** [end_byte node] returns the byte offset where this node ends. *)
val end_byte : t -> int

(** [start_point node] returns the (row, column) where this node starts. *)
val start_point : t -> point

(** [end_point node] returns the (row, column) where this node ends. *)
val end_point : t -> point

(** {1 Convenience functions} *)

(** [children node] returns all children as a list. *)
val children : t -> t list

(** [named_children node] returns all named children as a list. *)
val named_children : t -> t list

(** [text source node] extracts the source text for this node. *)
val text : string -> t -> string

(** [iter_children f node] calls [f] on each child. *)
val iter_children : (t -> unit) -> t -> unit

(** [iter_named_children f node] calls [f] on each named child. *)
val iter_named_children : (t -> unit) -> t -> unit

(** [fold_children f init node] folds over all children. *)
val fold_children : ('a -> t -> 'a) -> 'a -> t -> 'a

(** [traverse f node] recursively traverses the tree in pre-order, visiting only
    named nodes. *)
val traverse : (t -> unit) -> t -> unit

(** [find_all pred node] finds all descendant nodes matching the predicate. *)
val find_all : (t -> bool) -> t -> t list

(** [find_by_type type_name node] finds all descendants with the given node
    type. *)
val find_by_type : string -> t -> t list

(** [field name node] returns the child with the given field name as an option.
*)
val field : string -> t -> t option
