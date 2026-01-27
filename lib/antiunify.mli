(** Anti-unification for finding minimal abstractions of changes *)

(** {1 Single-change representation}

    Annotated nodes show what's same vs different within a single change.
    All content remains CONCRETE - no placeholders at this level.
    The goal is to highlight exactly what changed. *)

type annotated =
  | Same of {
      node_type: string;
      text: string;
      children: annotated list;
    }
  | Diff of {
      node_type: string;
      before: string;  (** Concrete text from before *)
      after: string;   (** Concrete text from after *)
    }
  | Added of {
      node_type: string;
      text: string;  (** Concrete text that was added *)
    }
  | Removed of {
      node_type: string;
      text: string;  (** Concrete text that was removed *)
    }
  | Replaced of {
      before_type: string;
      before_text: string;
      after_type: string;
      after_text: string;
    }
  | Modified of {
      node_type: string;
      children: annotated list;
    }

(** {1 Anti-unification within a change} *)

val antiunify_nodes : string -> Tree.t -> string -> Tree.t -> annotated option
(** [antiunify_nodes src1 node1 src2 node2] compares two nodes and returns
    an annotated representation showing what's same vs different.
    Returns [None] if the nodes are identical. *)

val antiunify_change : Diff.diff_result -> Diff.change -> annotated
(** [antiunify_change diff_result change] produces an annotated representation
    of a single change, showing exactly what differs (concrete values). *)

val pp_annotated : Format.formatter -> annotated -> unit
val to_string : annotated -> string

(** {1 Cross-change patterns}

    When comparing multiple similar changes, we can find common patterns
    by anti-unifying them. Parts that vary across changes become variables. *)

type pattern =
  | PConcrete of string  (** Same across all instances *)
  | PVar of int * string list  (** Varies - index and concrete values *)
  | PTransform of { before: pattern; after: pattern }  (** A transformation *)
  | PNode of string * pattern list  (** Node structure *)

val pattern_of_annotated : annotated -> pattern
(** Convert an annotated change to a pattern (initially all concrete). *)

val antiunify_patterns : int -> pattern -> pattern -> pattern * int
(** [antiunify_patterns var_counter p1 p2] finds the most specific
    generalization of two patterns. Returns the pattern and new var counter. *)

val antiunify_pattern_list : pattern list -> pattern option
(** Anti-unify a list of patterns to find their common structure. *)

val find_common_patterns : annotated list -> pattern option
(** Find the common pattern across multiple annotated changes. *)

val pp_pattern : Format.formatter -> pattern -> unit
val pattern_to_string : pattern -> string

val get_variable_values : int -> pattern -> string list
(** Get all concrete values associated with a variable in a pattern. *)
