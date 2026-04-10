(** Pattern matching using concrete syntax with metavariables *)

include module type of Match_types

(** {1 Pattern parsing} *)

val parse_pattern :
  ctx:Context.t ->
  language:string ->
  ?inherited_metavars:string list ->
  ?inherited_sequences:string list ->
  string ->
  pattern
(** [parse_pattern ~ctx ~language pattern_text] parses a pattern file. Pattern
    format:
    {v
    @@
    match: strict
    metavar $var1: single
    metavar $var2: single
    @@
    code using $var1 and $var2
    v}

    Metavars declared between \@@ markers are replaced with valid identifiers
    before parsing, allowing patterns to work across all languages.

    [match:] is required; choose [strict] (ordered, no extra children),
    [partial] (unordered subset), or [field] (field-name based).

    [?inherited_metavars] and [?inherited_sequences] carry metavar declarations
    from preceding sections; omit them when parsing standalone single-section
    patterns.

    Raises [Failure] if: the pattern does not start and end with \@@ delimiters;
    no [match:] mode is specified; a metavar in the pattern body is not
    declared; or a replacement line references an undeclared metavar.

    {2 Unification}

    Every occurrence of a metavariable in a match pattern must bind to
    structurally identical nodes (unification). Shadowing a metavar declared in
    an inherited scope is an error.

    {2 Sequence metavars}

    Metavars declared with [sequence] type match zero or more sibling nodes:
    {v
    @@
    match: strict
    metavar $class_name: single
    metavar $body: sequence
    @@
    class $class_name { $body }
    v}

    Here [$body] will match any number of children inside the class body,
    binding their combined text to [$body]. This allows matching classes with
    multiple methods, statements, etc.

    {2 Partial matching}

    Use [match: partial] to enable subset matching for children. Each pattern
    child finds any matching source child (unordered), and extra source children
    are ignored:
    {v
    @@
    match: partial
    metavar $X: single
    @@
    { someField: $X }
    v}

    This matches objects containing [someField], regardless of other properties.
    Partial matching applies recursively to all nested structures.

    {2 Direct matching with 'on'}

    Use [on $VAR] to match directly against a previously-bound node instead of
    traversing the subtree. Typically used in inner sections of a nested
    pattern:
    {v
    @@
    match: strict
    metavar $OBJ: single
    @@
    foo($OBJ)

    @@
    match: partial
    on $OBJ
    metavar $X: single
    @@
    { someField: $X }
    v}

    Section 2 matches directly against the node bound to [$OBJ], not its
    subtree. If [$OBJ] is a sequence metavar, the pattern is matched against
    each element in the sequence. *)

val parse_nested_pattern :
  ctx:Context.t -> language:string -> string -> nested_pattern
(** [parse_nested_pattern ~ctx ~language pattern_text] parses a pattern with
    multiple \@@ sections. Two modes are supported:

    {b Outer+inner mode} — one or more non-first sections carry [on $VAR].
    Metavars accumulate across sections (declared in the outer section are in
    scope for inner sections). Used for expansion transforms.

    {b Conjunctive siblings mode} — no section carries [on $VAR]. Each section
    is parsed with its own independent metavar scope. At transform time all
    sections must find at least one match; if any finds nothing, no edits are
    applied. See [transform_nested] and [docs/patterns.md] for examples.

    Mixing [on $VAR] and non-[on $VAR] sections in the same pattern is an
    error. Single-section patterns are always conjunctive (backward compatible). *)

(** {1 Matching} *)

val find_matches :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  match_result list
(** [find_matches ~ctx ~language ~pattern_text ~source_text] finds all
    occurrences of the pattern in the source code. Returns a list of matches
    with bindings for each metavariable. *)

val find_matches_in_file :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_path:string ->
  match_result list
(** [find_matches_in_file ~ctx ~language ~pattern_text ~source_path] finds
    matches in a file. *)

val find_nested_matches :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  nested_match_result list
(** [find_nested_matches ~ctx ~language ~pattern_text ~source_text] finds
    matches using a pattern with one or more [@@] sections.

    Auto-detects mode from section headers:
    - Single section: behaves like [find_matches]
    - Multiple sections with [on $VAR]: outer+inner scoped matching — each
      section searches within the node(s) bound by the previous section
    - Multiple sections without [on $VAR]: conjunctive mode — every section
      must find at least one match or the result is empty; matches from all
      sections are returned as a flat list

    Returns matches with context chain (populated in outer+inner mode). *)

val search :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  match_search_result
(** [search ~ctx ~language ~pattern_text ~source_text] finds matches and returns
    parse information.

    Like [find_nested_matches] but also returns [parse_error_count] indicating
    how many ERROR nodes (parse failures) were found in the source. This is
    useful for detecting when a file couldn't be fully parsed (e.g., using wrong
    grammar for the file type). *)

(** {1 Output} *)

val format_match : string -> match_result -> string
(** [format_match source_text result] formats a match result for display,
    showing the line number, matched text, and variable bindings. *)

val format_nested_match : string -> nested_match_result -> string
(** [format_nested_match source_text result] formats a nested match result for
    display, showing each context level with indentation:
    {v
    context[0] line N: <preview>
      $var = value
      context[1] line M: <preview>
        $var2 = value2
        => line K: <inner match>
           $inner_var = value
    v} *)

(** {1 Index-based matching} *)

val build_index : Tree.src Tree.t -> ast_index
(** [build_index root] builds an index from a parsed source tree. O(n) where n
    is the number of nodes. Use this when matching multiple patterns against the
    same source. *)

val find_matches_with_index :
  index:ast_index ->
  pattern:pattern ->
  source:string ->
  source_root:Tree.src Tree.t ->
  match_result list
(** [find_matches_with_index ~index ~pattern ~source ~source_root] finds matches
    using a pre-built index. Queries the index for candidate nodes by type, then
    matches only against those candidates. Falls back to full traversal if
    pattern root is a metavar. *)

val find_matches_multi :
  ctx:Context.t ->
  language:string ->
  patterns:string list ->
  source_text:string ->
  (int * match_result) list
(** [find_matches_multi ~ctx ~language ~patterns ~source_text] matches multiple
    patterns against source, building the index once. Returns list of
    (pattern_index, match_result) pairs. *)

(** {1 Semantic patch transforms} *)

val transform :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  transform_result
(** [transform ~ctx ~language ~pattern_text ~source_text] applies a semantic
    patch to the source. Patterns with [-]/[+] prefixed lines produce edits;
    patterns without them return the source unchanged.

    All occurrences of the pattern are replaced. When matches overlap in the
    source (as can happen with left-recursive constructs like nested member
    expressions), the transform iterates until no overlapping matches remain. *)

val transform_nested :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  transform_result
(** [transform_nested ~ctx ~language ~pattern_text ~source_text] applies a
    multi-section semantic patch. Behaviour depends on mode:

    {b Outer+inner mode} (sections use [on $VAR]): the first section is the
    outer match/replace pattern. For each expansion slot (separator-prefix
    line), if a subsequent section declares [on $VAR] matching that slot's
    variable, its transform is applied to each element; elements with no
    matching inner section are passed through unchanged.

    {b Conjunctive siblings mode} (no [on $VAR]): every section independently
    searches the source. If all sections find at least one match, all their
    transforms are applied in a single pass. If any section finds no match, the
    source is returned unchanged. A section without [-]/[+] lines acts as a
    guard — it must match but contributes no edits.

    Single-section patterns behave identically to [transform]. See
    [docs/patterns.md] for worked examples of both modes. *)

val transform_file :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_path:string ->
  transform_result
(** [transform_file ~ctx ~language ~pattern_text ~source_path] applies a
    semantic patch to a file. *)

val generate_diff :
  file_path:string -> original:string -> transformed:string -> string
(** [generate_diff ~file_path ~original ~transformed] produces a unified diff
    string between original and transformed text. Returns empty string if equal.
*)

val apply_edits : string -> text_edit list -> string
(** [apply_edits source edits] applies text edits to source, processing
    bottom-to-top to avoid offset invalidation. Filters overlapping edits. *)
