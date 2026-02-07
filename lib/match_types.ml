(** Pattern matching using concrete syntax with metavariables *)

(** Index for fast node lookup by type *)
type ast_index = {
  by_type: (string, Tree.src Tree.t list) Hashtbl.t;
}

(** Match mode for child matching *)
type match_mode =
  | Strict   (** Exact positional matching - no extra children allowed *)
  | Field    (** Match by field name - extras in other fields ignored, ordered within fields *)
  | Partial  (** Unordered subset matching - extras ignored, order doesn't matter *)

(** A correspondence between a pattern child index and a source child index *)
type child_correspondence = {
  pattern_index: int;
  source_index: int;
}

(** A text edit: replace bytes [start_byte, end_byte) with replacement *)
type text_edit = {
  start_byte: int;
  end_byte: int;
  replacement: string;
}

(** Result of applying transforms *)
type transform_result = {
  edits: text_edit list;
  original_source: string;
  transformed_source: string;
}

(** A parsed pattern with metavariable information *)
type pattern = {
  metavars: string list;  (** Original metavar names (e.g., ["$msg"; "$fn"]) *)
  sequence_metavars: string list;  (** Metavars that match sequences (declared with * suffix) *)
  substitutions: (string * string) list;  (** Maps original name -> placeholder *)
  tree: Tree.pat Tree.tree;  (** Parsed pattern tree (phantom-typed as pattern) *)
  source: string;  (** Transformed source (with placeholders) *)
  original_source: string;  (** Original pattern source (after preamble) *)
  match_mode: match_mode;  (** How to match children *)
  on_var: string option;  (** If set, match against the node bound to this var instead of traversing *)
  is_transform: bool;  (** Whether pattern has -/+ lines (semantic patch) *)
  replace_tree: Tree.pat Tree.tree option;  (** Parsed replacement tree, if is_transform *)
  replace_source: string;  (** Transformed replacement source (with placeholders) *)
}

(** A single match result *)
type match_result = {
  node: Tree.src Tree.t;  (** The matched source node *)
  bindings: (string * string) list;  (** Metavar name -> matched text *)
  node_bindings: (string * Tree.src Tree.t) list;  (** Metavar name -> matched node (for 'on $VAR') *)
  sequence_node_bindings: (string * Tree.src Tree.t list) list;  (** Metavar name -> matched nodes for sequences *)
  correspondences: child_correspondence list;  (** Pattern-to-source child index mapping *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** A context match for nested patterns *)
type context_match = {
  context_node: Tree.src Tree.t;
  context_bindings: (string * string) list;
  context_node_bindings: (string * Tree.src Tree.t) list;
  context_sequence_node_bindings: (string * Tree.src Tree.t list) list;
  context_start_point: Tree.point;
  context_end_point: Tree.point;
}

(** Result of nested pattern matching *)
type nested_match_result = {
  node: Tree.src Tree.t;
  bindings: (string * string) list;  (** All bindings (inner + inherited from contexts) *)
  node_bindings: (string * Tree.src Tree.t) list;
  sequence_node_bindings: (string * Tree.src Tree.t list) list;
  contexts: context_match list;  (** outermost first *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** Result of matching with parse information *)
type match_search_result = {
  matches: nested_match_result list;
  parse_error_count: int;  (** Number of ERROR nodes in source *)
}

(** A nested pattern with multiple sections *)
type nested_pattern = {
  patterns: pattern list;  (** All sections; last is target, rest are contexts *)
}

(** Result of parsing a single preamble line *)
type preamble_line =
  | Metavar of string * bool  (** name, is_sequence *)
  | MatchMode of match_mode
  | OnVar of string

(** Parsed preamble result *)
type preamble_result = {
  p_metavars: string list;
  p_sequence_metavars: string list;
  p_match_mode: match_mode option;
  p_on_var: string option;
  p_pattern_body: string;
}

(** Match bindings: text bindings and node bindings (from source) *)
type match_bindings = {
  text_bindings: (string * string) list;
  node_bindings: (string * Tree.src Tree.t) list;
  sequence_node_bindings: (string * Tree.src Tree.t list) list;  (** For sequence metavars *)
  correspondences: child_correspondence list;  (** Pattern-to-source child index mapping *)
}
