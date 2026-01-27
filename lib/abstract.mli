(** Change abstraction - Replace concrete values with placeholders *)

(** Types of placeholders used in abstraction *)
type placeholder_kind =
  | Identifier   (** Named identifiers: variable names, function names, etc. *)
  | StringLit    (** String literals *)
  | NumberLit    (** Number literals *)
  | TypeName     (** Type identifiers *)
  | Other        (** Other leaf nodes *)

(** A placeholder in an abstracted template *)
type placeholder = {
  kind: placeholder_kind;
  index: int;  (** Unique index for this placeholder within its kind *)
  original_text: string;  (** The original concrete value *)
}

(** An abstracted node - either structural or a placeholder *)
type abstract_node =
  | Structural of {
      node_type: string;
      children: abstract_node list;
    }
  | Placeholder of placeholder

(** An abstracted change template *)
type abstract_change = {
  change_kind: [ `Added | `Removed | `Modified | `Replaced ];
  context: Diff.context;
  before: abstract_node option;
  after: abstract_node option;
}

(** {1 Abstraction} *)

val abstract_change : Diff.diff_result -> Diff.change -> abstract_change
(** [abstract_change diff_result change] creates an abstracted version of
    a change where identifiers and literals are replaced with placeholders.
    The same concrete value will get the same placeholder index. *)

(** {1 Templates for comparison} *)

type template
(** A normalized template that can be compared across changes.
    Two changes with the same template are "similar" - they have the same
    structural transformation but may use different concrete values. *)

val to_template : abstract_change -> template
(** [to_template change] creates a template for comparison. *)

val templates_equal : template -> template -> bool
(** [templates_equal t1 t2] checks if two templates are structurally equal. *)

(** {1 Grouping} *)

val group_by_template : abstract_change list -> abstract_change list list
(** [group_by_template changes] groups changes that have the same template.
    Returns groups sorted by size (largest first). *)

(** {1 Change inspection} *)

val change_node_type : abstract_change -> string
(** Get the node type of a change (from the before or after node). *)

val before_to_string : abstract_change -> string option
(** Get the abstracted before-text of a change (None for Added). *)

val after_to_string : abstract_change -> string option
(** Get the abstracted after-text of a change (None for Removed). *)

(** {1 Pretty printing} *)

val pp_placeholder : Format.formatter -> placeholder -> unit
val pp_abstract_node : Format.formatter -> abstract_node -> unit
val pp_abstract_change : Format.formatter -> abstract_change -> unit
val to_string : abstract_change -> string
val node_to_string : abstract_node -> string
