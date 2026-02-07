(** Tree-level diffing with GumTree-inspired node matching.

    Computes a node-to-node mapping between two parsed trees using hash-based
    identical-subtree detection and anchor-based child alignment. Derives a
    structured diff from the mapping. *)

(** {1 Types} *)

type node_key = string * int * int
(** Key identifying a node within a tree: [(node_type, start_byte, end_byte)].
    Unique within a single parse tree. *)

type node_mapping = {
  forward : (node_key, node_key) Hashtbl.t;
  backward : (node_key, node_key) Hashtbl.t;
}
(** Bidirectional mapping between nodes of two trees. [forward] maps before-node
    keys to after-node keys; [backward] maps the reverse. *)

(** How a node changed between before and after trees *)
type node_change =
  | Unchanged  (** Subtrees are structurally identical *)
  | Modified of { child_changes : child_change list }
      (** Same node type but some children differ *)
  | Replaced  (** Completely different subtrees *)

(** How a child position changed *)
and child_change =
  | Same of { node : Tree.src Tree.t }
      (** Child is unchanged *)
  | Changed of {
      before : Tree.src Tree.t;
      after : Tree.src Tree.t;
      change : node_change;
    }
      (** Child exists in both trees but differs *)
  | Removed of { node : Tree.src Tree.t }
      (** Child exists only in the before tree *)
  | Added of { node : Tree.src Tree.t }
      (** Child exists only in the after tree *)

type diff = {
  before_source : string;
  after_source : string;
  before_root : Tree.src Tree.t;
  after_root : Tree.src Tree.t;
  root_change : node_change;
  mapping : node_mapping;
}
(** Complete diff between two parsed trees, including the node mapping. *)

type change_pair = {
  before_node : Tree.src Tree.t;
  after_node : Tree.src Tree.t;
  before_source : string;
  after_source : string;
}
(** An atomic change: a pair of before/after subtrees at the point where the
    diff recursion bottomed out. *)

(** {1 Mapping} *)

val compute_mapping :
  before_source:string ->
  after_source:string ->
  Tree.src Tree.t ->
  Tree.src Tree.t ->
  node_mapping
(** [compute_mapping ~before_source ~after_source before_root after_root]
    computes a node-to-node mapping between two trees. Uses hash-based
    identical-subtree matching followed by anchor-based child alignment with
    type+similarity fallback. *)

(** {1 Diffing} *)

val diff : before:Tree.src Tree.tree -> after:Tree.src Tree.tree -> diff
(** [diff ~before ~after] computes a structured diff between two parsed trees.
    Internally calls [compute_mapping] and derives the change structure. *)

val change_pairs : diff -> change_pair list
(** [change_pairs d] extracts the leaf-level change pairs from a diff. These are
    the minimal before/after subtree pairs — the [Changed] nodes whose
    [node_change] is [Replaced], plus [Changed] nodes at the deepest level of
    [Modified] chains. *)
