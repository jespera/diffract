(** Tree diff computation *)

(** Context describing where a change occurs in the tree *)
type context = {
  parent_type: string option;
  field_name: string option;
  index: int;
}

(** Summary of a node's structure *)
type node_shape = {
  node_type: string;
  children: node_shape list;
}

(** A single change between two trees *)
type change =
  | Added of {
      context: context;
      node: Tree.t;
      shape: node_shape;
    }
  | Removed of {
      context: context;
      node: Tree.t;
      shape: node_shape;
    }
  | Modified of {
      context: context;
      node_type: string;
      before: Tree.t;
      after: Tree.t;
      children_changed: change list;
    }
  | Replaced of {
      context: context;
      before: Tree.t;
      after: Tree.t;
      before_shape: node_shape;
      after_shape: node_shape;
    }

(** Result of diffing two trees *)
type diff_result = {
  changes: change list;
  before_source: string;
  after_source: string;
}

(** {1 Diff computation} *)

val diff :
  before_source:string ->
  before_root:Tree.t ->
  after_source:string ->
  after_root:Tree.t ->
  diff_result
(** [diff ~before_source ~before_root ~after_source ~after_root] computes
    the structural differences between two parsed trees. *)

(** {1 Node comparison} *)

val node_shape : Tree.t -> node_shape
(** [node_shape node] computes the structural shape of a node. *)

val hash_shape : node_shape -> int
(** [hash_shape shape] computes a hash of the node shape. *)

val hash_node : string -> Tree.t -> int
(** [hash_node source node] computes a hash including text content. *)

val nodes_equal : string -> Tree.t -> string -> Tree.t -> bool
(** [nodes_equal src1 n1 src2 n2] checks if two nodes are identical. *)

val same_shape : Tree.t -> Tree.t -> bool
(** [same_shape n1 n2] checks if two nodes have the same structure. *)

(** {1 Change inspection} *)

val change_text_before : diff_result -> change -> string option
(** Get the before-text of a change (None for Added). *)

val change_text_after : diff_result -> change -> string option
(** Get the after-text of a change (None for Removed). *)

val change_context : change -> context
(** Get the context where a change occurs. *)

val change_node_type : change -> string
(** Get the node type of a change. *)

val flatten_changes : change list -> change list
(** Flatten nested Modified changes into a flat list. *)

(** {1 Pretty printing} *)

val pp_context : Format.formatter -> context -> unit
val pp_change : Format.formatter -> diff_result -> change -> unit
val pp : Format.formatter -> diff_result -> unit
val to_string : diff_result -> string
