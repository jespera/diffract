(** Pattern matching DSL for tree-sitter nodes *)

(** {1 Types} *)

(** A pattern that can match against nodes *)
type t

(** A constraint on a node *)
type constraint_

(** Result of a successful match, with captured nodes *)
type match_result = {
  node: Tree.t;
  captures: (string * Tree.t) list;
}

(** {1 Pattern constructors} *)

val any : t
(** Matches any node. *)

val node : string -> constraint_ list -> t
(** [node type_name constraints] matches a node of the given type
    that satisfies all constraints. *)

val any_node : constraint_ list -> t
(** [any_node constraints] matches any named node satisfying constraints. *)

val ( ||| ) : t -> t -> t
(** [p1 ||| p2] matches if either pattern matches. *)

val not_ : t -> t
(** [not_ p] matches if pattern does not match. *)

(** {1 Constraint constructors} *)

val field : string -> t -> constraint_
(** [field name pattern] requires the child at [name] to match [pattern]. *)

val has_field : string -> constraint_
(** [has_field name] requires a child to exist at [name]. *)

val child : t -> constraint_
(** [child pattern] requires at least one child to match [pattern]. *)

val all_children : t -> constraint_
(** [all_children pattern] requires all children to match [pattern]. *)

val has_text : string -> constraint_
(** [has_text s] requires the node's text to equal [s]. *)

val text_matches : (string -> bool) -> constraint_
(** [text_matches pred] requires the node's text to satisfy [pred]. *)

val capture : string -> constraint_
(** [capture name] captures the current node with the given name. *)

val descendant : t -> constraint_
(** [descendant pattern] requires some descendant to match [pattern]. *)

(** {1 Matching functions} *)

val matches : string -> t -> Tree.t -> bool
(** [matches source pattern node] tests if the node matches the pattern. *)

val match_node : string -> t -> Tree.t -> match_result option
(** [match_node source pattern node] attempts to match, returning captures. *)

val find_all : string -> t -> Tree.t -> match_result list
(** [find_all source pattern root] finds all nodes matching the pattern. *)

val get_capture : string -> match_result -> Tree.t option
(** [get_capture name result] retrieves a captured node by name. *)

val get_captures : string -> match_result -> Tree.t list
(** [get_captures name result] retrieves all nodes captured with [name]. *)

(** {1 Convenience patterns} *)

val capture_node : string -> string -> t
(** [capture_node cap_name type_name] matches node of type and captures it. *)

val capture_any : string -> t
(** [capture_any name] matches any named node and captures it. *)

val node_with_text : string -> string -> t
(** [node_with_text type_name text] matches node with specific text. *)

val identifier : string -> t
(** [identifier name] matches an identifier with the given name. *)

val any_identifier : string -> t
(** [any_identifier cap_name] matches any identifier and captures it. *)
