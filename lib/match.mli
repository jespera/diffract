(** Pattern matching using concrete syntax with metavariables *)

(** Index for efficient multi-pattern matching *)
type ast_index

(** A parsed pattern with metavariable information *)
type pattern

(** A single match result *)
type match_result = {
  node: Tree.t;  (** The matched node *)
  bindings: (string * string) list;  (** Metavar name -> matched text *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** A context match for nested patterns *)
type context_match = {
  context_node: Tree.t;
  context_bindings: (string * string) list;
  context_start_point: Tree.point;
  context_end_point: Tree.point;
}

(** Result of nested pattern matching *)
type nested_match_result = {
  inner_node: Tree.t;
  inner_bindings: (string * string) list;
  all_bindings: (string * string) list;
  contexts: context_match list;  (** outermost first *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** A nested pattern with multiple sections *)
type nested_pattern

(** {1 Pattern parsing} *)

val parse_pattern : language:string -> string -> pattern
(** [parse_pattern ~language pattern_text] parses a pattern file.
    Pattern format:
    {v
    @@
    $var1
    $var2
    @@
    code using $var1 and $var2
    v}

    Metavars declared between @@ markers are replaced with valid identifiers
    before parsing, allowing patterns to work across all languages.
    Raises [Failure] if any metavar in the pattern body is not declared.

    {2 Sequence metavars}

    Metavars declared with a [*] suffix match zero or more sibling nodes:
    {v
    @@
    $class_name
    $body*
    @@
    class $class_name { $body }
    v}

    Here [$body*] will match any number of children inside the class body,
    binding their combined text to [$body]. This allows matching classes
    with multiple methods, statements, etc. *)

val parse_nested_pattern : language:string -> string -> nested_pattern
(** [parse_nested_pattern ~language pattern_text] parses a pattern with
    multiple @@ sections for nested/scoped matching.

    Pattern format for nested matching:
    {v
    @@
    $class_name $body
    @@
    class $class_name { $body }

    @@
    $msg
    @@
    console.log($msg)
    v}

    - Each section declares its metavars in the preamble, then the pattern code
    - If multiple sections exist, each one restricts the search scope for the next
    - The last section is the target pattern (what we're searching for)
    - All preceding sections are contexts (outer to inner)
    - All metavars share the same binding scope across all sections
    - Single section works exactly as [parse_pattern] (backward compatible) *)

(** {1 Matching} *)

val find_matches : language:string -> pattern_text:string -> source_text:string -> match_result list
(** [find_matches ~language ~pattern_text ~source_text] finds all occurrences
    of the pattern in the source code. Returns a list of matches with bindings
    for each metavariable. *)

val find_matches_in_file : language:string -> pattern_text:string -> source_path:string -> match_result list
(** [find_matches_in_file ~language ~pattern_text ~source_path] finds matches in a file. *)

val find_nested_matches : language:string -> pattern_text:string -> source_text:string -> nested_match_result list
(** [find_nested_matches ~language ~pattern_text ~source_text] finds matches
    using nested/scoped pattern matching.

    Auto-detects single vs. multi-section patterns:
    - Single section: behaves like [find_matches]
    - Multiple sections: each context pattern restricts where the next pattern
      can match (nested scoping)

    Returns matches with context chain showing how each match was scoped. *)

(** {1 Output} *)

val format_match : string -> match_result -> string
(** [format_match source_text result] formats a match result for display,
    showing the line number, matched text, and variable bindings. *)

val format_nested_match : string -> nested_match_result -> string
(** [format_nested_match source_text result] formats a nested match result
    for display, showing each context level with indentation:
    {v
    context[0] line N: <preview>
      $var = value
      context[1] line M: <preview>
        $var2 = value2
        => line K: <inner match>
           $inner_var = value
    v} *)

(** {1 Index-based matching} *)

val build_index : Tree.t -> ast_index
(** [build_index root] builds an index from a parsed tree.
    O(n) where n is the number of nodes.
    Use this when matching multiple patterns against the same source. *)

val find_matches_with_index :
  index:ast_index ->
  pattern:pattern ->
  source:string ->
  source_root:Tree.t ->
  match_result list
(** [find_matches_with_index ~index ~pattern ~source ~source_root] finds matches
    using a pre-built index. Queries the index for candidate nodes by type,
    then matches only against those candidates.
    Falls back to full traversal if pattern root is a metavar. *)

val find_matches_multi :
  language:string ->
  patterns:string list ->
  source_text:string ->
  (int * match_result) list
(** [find_matches_multi ~language ~patterns ~source_text] matches multiple
    patterns against source, building the index once.
    Returns list of (pattern_index, match_result) pairs. *)
