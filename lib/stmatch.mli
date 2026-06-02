(** Sequence-to-tree pattern matching.

    The algorithm walks a flat list of pattern tokens against a tree accessed
    via a {!Cursor.S}. Concrete tokens match leaves on both text and node type;
    subtree wildcards bind to one complete subtree with left-spine backtracking;
    sibling wildcards match zero or more adjacent subtrees.

    Wildcards may carry a name. Named wildcards record bindings (single subtree
    for [Subtree], list of subtrees for [Siblings]). If the same name appears
    more than once, the matcher requires all occurrences to bind
    structurally-equal subtrees / sequences (non-linear pattern matching).
    Anonymous wildcards ([name = None]) match without recording.

    Adapted from the STMatch algorithm by Matute et al., "Syntactic Code Search
    with Sequence-to-Tree Matching" (PLDI 2024). See
    {!docs/universal-tokenizer.md} for the architectural context. *)

(** A token in the pattern stream. *)
type pattern_token =
  | Concrete of { text : string; node_type : string }
      (** A literal token. Matches a source leaf iff both [text] and [node_type]
          agree. *)
  | Subtree of { name : string option }
      (** Subtree wildcard. Matches one complete subtree of any shape.
          [name = None] is anonymous (no binding recorded). [name = Some n]
          records a binding under [n]; subsequent occurrences of the same name
          must bind to a structurally-equal subtree. *)
  | Siblings of { name : string option }
      (** Siblings wildcard. Matches zero or more adjacent subtrees.
          [name = None] is anonymous (no binding recorded). [name = Some n]
          records a binding (the list of consumed subtrees) under [n]. *)

module Make (C : Cursor.S) : sig
  (** A recorded binding for a named wildcard. The stored cursor is positioned
      AT the bound subtree (cloned at the time the wildcard consumed it). *)
  type binding =
    | Single of { name : string; cursor : C.t }
    | Sequence of { name : string; cursors : C.t list }

  val match_at :
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    (C.t * binding list) option
  (** [match_at pattern cursor] tries to match [pattern] starting at [cursor]'s
      current position.

      Returns [Some (end_cursor, bindings)] on success, where [end_cursor] is
      the cursor positioned just after the last consumed subtree and [bindings]
      is the list of recorded bindings (one entry per named wildcard, in the
      order they appeared in the pattern). Anonymous wildcards do not contribute
      entries. Returns [None] on failure.

      Success here means the whole pattern was consumed. The source may have
      content past the matched region — [match_at] does not require it to be
      exhausted.

      Bindings are checkpoint-scoped: failures that trigger backtracking also
      roll back any bindings recorded since the corresponding checkpoint.

      [initial_bindings] (default [\[\]]) seeds the binding state with
      pre-existing bindings. Named wildcards encountered during this match
      that share a name with a seeded binding must bind to a
      structurally-equal subtree; otherwise the match fails. The returned
      bindings include the seed plus any new bindings from this call, in
      pattern order.

      The input cursor is mutated during the attempt; on failure its final
      position is unspecified (use {!Cursor.S.clone} on the input cursor if you
      need to retry from the same position). *)

  val match_prefix :
    ?initial_bindings:binding list ->
    ?ignore_node_type:bool ->
    ?descend:bool ->
    pattern_token list ->
    C.t ->
    (pattern_token list * C.t * binding list) option
  (** [match_prefix pattern cursor] tries to match a prefix of [pattern] against
      [cursor]'s sub-tree, succeeding when the sub-tree is exhausted.

      [ignore_node_type] (default [false]): when [true], [Concrete] tokens
      match source leaves on text alone, ignoring node type. Used by
      field-mode search as a cheap superset probe to decide whether a
      candidate is worth the cost of source-context re-tokenization — the
      precise (node-type-checked) match is run afterwards.

      [descend] (default [false]): when [true], the matched prefix is the one
      that consumes the {b most} pattern while still exhausting the source
      subtree — so a leading [Subtree] wildcard descends into the child
      (matching, say, an object key) instead of greedily swallowing the whole
      child and leaving the rest of the element unmatched. Used by
      {!match_set_at} so a partial element like [$K: $V] maps cleanly to one
      child. Without it (field mode, plain prefix use) the first
      source-exhaustion is returned — a leading wildcard takes the whole
      child.

      Returns [Some (remaining, end_cursor, bindings)] on success, where
      [remaining] is the unconsumed suffix of [pattern]. When the pattern and
      the sub-tree exhaust together, [remaining] is the empty list. Returns
      [None] when [pattern] exhausts before the sub-tree (pattern was too
      short to cover the sub-tree) or when no match path succeeds.

      Intended use: scope a cursor to a single source subtree via
      {!Cursor.S.of_node} (or equivalent in the cursor implementation), then
      call [match_prefix] to consume one element's worth of pattern tokens.
      The remaining-pattern return value is fed into the next call when
      iterating over a sequence of source children.

      [initial_bindings] (default [\[\]]) seeds the binding state — the same
      semantics as in {!match_at}. The partial-mode driver uses this to
      thread bindings across element matches so that cross-element
      non-linearity is enforced.

      Like {!match_at}, the input cursor is mutated; clone first if you need
      to retry from the same position. *)

  val match_set_at :
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    (C.t * binding list) option
  (** [match_set_at pattern cursor] is the low-level set-match primitive:
      it consumes [pattern] by repeated {!match_prefix} calls against the
      source's named children (via {!Cursor.S.named_children}), with set
      semantics — unordered, extras tolerated, separator-aware. It does
      NOT inspect leading/trailing anonymous delimiter runs; callers that
      want container-level partial matching should use {!match_partial_at}.

      Semantics:
      - Set-based matching: pattern elements may align with source named
        children in any order (reordering supported).
      - Extras tolerated: source named children that aren't claimed by any
        pattern element are silently accepted.
      - Source-derived separator stripping between successful element
        matches: a leading separator token (e.g. [","] for object
        literals, [";"] for record types) is stripped from the pattern.
        The separator is inferred from the bytes between the first two
        source named children; an empty inferred separator (e.g. JSX
        attributes) means no stripping.
      - Lenient: if the pattern omits the separator, the matcher proceeds
        without stripping.
      - Backtracking: ambiguous patterns explore alternative
        element-to-child assignments. Worst-case complexity is O(n!) for
        n source children with all-wildcard leading tokens; typical
        patterns with distinct leading concrete tokens per element are
        effectively linear because non-matching children fail
        [match_prefix] on the first token.
      - Cross-element non-linearity: accumulated bindings are threaded
        through each [match_prefix] call as [initial_bindings], so a
        named wildcard appearing in multiple elements must bind to
        structurally-equal subtrees across the match. Inconsistent
        assignments trigger backtracking or overall failure.

      Returns [Some (cursor, bindings)] on success (cursor unchanged at
      the container; bindings accumulated). Returns [None] if any pattern
      element fails to match an unused source child. *)

  val match_partial_at :
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    (C.t * binding list) option
  (** [match_partial_at pattern cursor] is the partial-mode entry point
      for a {b real} set-like container. [pattern] is the {b full} token
      stream — including whatever bracket/punctuation tokens the source's
      grammar produces as the container's anonymous delimiters.

      The algorithm splits the source container's non-extra children into:
      - A leading anonymous run ({!Cursor.S.leading_anonymous_leaves} —
        e.g. [\['{'\]] for an object, [\['<'\]] for a JSX element).
      - The named children ({!Cursor.S.named_children} — the elements).
      - A trailing anonymous run ({!Cursor.S.trailing_anonymous_leaves} —
        e.g. [\['}'\]], [\['/>'\]]).

      The pattern is split in lock-step with that structure: a front of
      [List.length leading] tokens strict-matched against the leading
      run, a back of [List.length trailing] tokens strict-matched against
      the trailing run, and a middle handed to {!match_set_at} for the
      element-level set-match against the named children.

      Structural self-filter: a candidate node must have {b both} a
      non-empty leading run and a non-empty trailing run — that's the
      property of a real bracketed container. Nodes without that shape
      (statements, positional declarations, non-bracketed expressions,
      ...) fail immediately. This rules out the corner case where a
      multi-token pattern would otherwise collapse into a single named
      child as a strict subtree match.

      Strict delimiter matching: front/back tokens must be [Concrete] and
      equal the source leaves on both text and node type. A wildcard in
      a delimiter slot fails.

      Returns [Some (cursor, bindings)] on success (cursor unchanged at
      the container). Returns [None] when the candidate node has no
      structural delimiters, when the delimiter runs don't strict-match,
      when the pattern is shorter than the combined delimiter run, or
      when {!match_set_at} fails on the middle.

      Test fixtures: cursor implementations that don't model the
      named/anonymous distinction (typically hand-built test trees)
      return [\[\]] from the anonymous-leaf helpers; against those,
      [match_partial_at] always returns [None]. Use {!match_set_at}
      directly for fixture-based tests of the set-match logic. *)

  val match_field_at :
    ?initial_bindings:binding list ->
    ?ignore_node_type:bool ->
    pattern_token list ->
    C.t ->
    (C.t * binding list) option
  (** [match_field_at pattern cursor] is the field-mode entry point. It
      aligns the ordered [pattern] token stream to a {b subsequence} of the
      node's named children ({!Cursor.S.named_children}), left to right.

      At each child the matcher either MATCHES it — consuming the prefix of
      the remaining pattern that exactly covers the child's subtree, via
      {!match_prefix} — or SKIPS it, leaving the pattern untouched. The
      choice is explored with backtracking, MATCH first. The pattern must be
      fully consumed; children remaining after the pattern is exhausted are
      skipped (tolerated extras).

      No per-language optionality data is consulted. Required fields are
      syntactically mandatory, so the pattern always contains them and they
      are matched in order; the only children a pattern can leave unmatched
      are the optional ones it omits (decorators, annotations, modifier
      groups, return types). MATCH-first guarantees a child the pattern
      addresses is matched fully — a pattern [f ()] does not match a source
      [f (a, b)] — while SKIP and backtracking give both "ignore what I
      didn't mention" and "match this element among several of its shape"
      (e.g. [@Get] skipping a leading [@Auth]).

      The walk is over {b all} non-extra children
      ({!Cursor.S.all_children}), so a declaration's structural keywords —
      anonymous leaves that are direct children (Kotlin [fun], Scala [def],
      a [function] keyword) — are seen and {b must} match; only {b named}
      children are skippable. Skipping an anonymous keyword the pattern omits
      (TS [async] / [abstract]) is the deferred Tier-3 case and so yields an
      honest no-match. A pattern's own punctuation is matched inside a child
      by [match_prefix]. Returns [None] when the pattern cannot be fully
      consumed. On success the cursor is returned unchanged at the node. *)

  (** A successful match found by the outer-loop search functions. *)
  type match_result = {
    start_byte : int;
    end_byte : int;
    bindings : binding list;
  }

  val find_matches :
    ?overlapping:bool ->
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    match_result list
  (** [find_matches pattern source_cursor] returns matches of [pattern] in the
      tree accessible via [source_cursor], in document order. Walks the source
      tree in pre-order, attempting [match_at]. The caller's cursor is not
      mutated.

      [overlapping] (default [false]) selects the resume behaviour after a
      match:
      - [false]: resume past the matched region — matches are non-overlapping
        and nested matches are skipped. Greedy, left-to-right. Suited to
        transforms.
      - [true]: resume one node into the match, so matches nested inside a
        match are also reported; spans are de-duplicated so each distinct
        [(start_byte, end_byte)] appears once. Suited to search.

      [initial_bindings] (default [\[\]]) seeds the binding state for every
      [match_at] attempt during the walk. Named wildcards encountered during
      a match that share a name with a seeded binding must bind to a
      structurally-equal subtree, otherwise the attempt fails. Used by the
      multi-section driver to enforce cross-section non-linearity. *)

  val find_matches_iter :
    ?overlapping:bool ->
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    match_result Seq.t
  (** Lazy variant of {!find_matches}. Pull match results on demand from the
      sequence; stops walking the tree when consumers stop pulling. *)

  val find_partial_matches :
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    match_result list
  (** [find_partial_matches pattern source_cursor] is the partial-mode
      outer loop: at every source node it attempts {!match_partial_at},
      treating the node as a candidate set-like container.

      [pattern] is the {b full} token stream — including the container's
      bracket/punctuation tokens. The inner matcher reads the actual
      delimiter shape from each candidate source node, so the same pattern
      adapts naturally to whatever the grammar produced. Nodes that don't
      have the right structure (wrong leading/trailing anon, wrong element
      shape) fail cleanly — no explicit container classification is
      needed.

      Spans are de-duplicated and the walk descends into matches, so
      nested containers are found. *)

  val find_partial_matches_iter :
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    match_result Seq.t
  (** Lazy variant of {!find_partial_matches}. *)

  val find_field_matches :
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    match_result list
  (** [find_field_matches pattern source_cursor] is the field-mode outer
      loop: at every source node it attempts {!match_field_at}, treating the
      node as a candidate declaration. Nodes whose named children can't be
      aligned to the pattern fail cleanly — no explicit declaration
      classification is needed. Spans are de-duplicated and the walk descends
      into matches. *)

  val find_field_matches_iter :
    ?initial_bindings:binding list ->
    pattern_token list ->
    C.t ->
    match_result Seq.t
  (** Lazy variant of {!find_field_matches}. *)

  val find_field_matches_with :
    ?initial_bindings:binding list ->
    tokens_for:(C.t -> pattern_token list) ->
    C.t ->
    match_result list
  (** Like {!find_field_matches}, but the pattern is computed per candidate
      node by [tokens_for] rather than fixed. This is the hook for field-mode
      {b source-context tokenization}: the caller re-tokenizes the pattern in
      each candidate's surrounding source so context-sensitive node-types
      (e.g. a class method name as [property_identifier], not [identifier])
      come out right, while the matcher stays agnostic about tokenization. A
      [tokens_for] returning [[]] makes that node a non-match. *)

  val find_field_matches_with_iter :
    ?initial_bindings:binding list ->
    tokens_for:(C.t -> pattern_token list) ->
    C.t ->
    match_result Seq.t
  (** Lazy variant of {!find_field_matches_with}. *)
end
