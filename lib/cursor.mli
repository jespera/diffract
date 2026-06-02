(** Abstract tree cursor for the matcher.

    A [Cursor.S] implementation provides pre-order navigation over a tree-shaped
    data source. The matcher consumes the cursor without knowing the underlying
    tree representation, which allows the matching algorithm to be tested in
    isolation (via [Test_cursor] over hand-built fixtures) and run in production
    against a real tree-sitter parse.

    Leaves carry both text and node type so the matcher can compare on
    [(text, node_type)] rather than text alone — see §3.10 of
    [docs/universal-tokenizer.md] for the disambiguation cases this addresses.
    Navigation operations skip tree-sitter "extras" (whitespace, comments)
    transparently.

    Navigation mutates the cursor in place and returns [bool] for
    success/failure. Use {!clone} to snapshot a cursor for backtracking. *)

module type S = sig
  type t
  (** A position in the tree. Mutable: navigation operations modify [t] in
      place. *)

  type leaf
  (** A leaf node, carrying both its text and its node type. *)

  val move_first_leaf : t -> leaf
  (** [move_first_leaf c] descends [c] to its leftmost leaf and returns the leaf
      data. If [c] is already a leaf, returns the leaf at [c] without moving.
      Always succeeds. *)

  val move_first_child : t -> bool
  (** [move_first_child c] descends one level to the first named child of [c],
      skipping extras (whitespace / comments). Returns [false] if [c] has no
      children to descend into (i.e. is a leaf). *)

  val move_next_subtree : t -> bool
  (** [move_next_subtree c] advances [c] past its current subtree to the next
      subtree in pre-order. Climbs ancestors as needed to find a next sibling.
      Returns [false] if no more subtrees exist in the enclosing root. Skips
      extras. *)

  val move_next_sibling : t -> bool
  (** [move_next_sibling c] advances [c] to its next sibling within its parent.
      Returns [false] if [c] is the last child. Does not climb ancestors. Skips
      extras. *)

  val clone : t -> t
  (** [clone c] returns an independent snapshot of [c]. Used to save cursor
      state before mutating navigation, so that backtracking can restore the
      prior position. Implementations should make this cheap (a shallow copy of
      cursor state, not the underlying tree). *)

  val narrow : t -> t
  (** [narrow c] returns a fresh cursor positioned at the same node as [c] but
      rescoped: navigation operations (especially {!move_next_subtree}) cannot
      climb above the current node. This is the operation used by multi-section
      search to scope a section's [find_matches] walk to the subtree bound by
      [on $VAR], without leaking into the surrounding source. The original
      cursor is unchanged. *)

  val leaf_text : leaf -> string
  (** [leaf_text l] returns the source text of the leaf [l]. *)

  val leaf_node_type : leaf -> string
  (** [leaf_node_type l] returns the tree-sitter node type of the leaf [l] (e.g.
      ["identifier"], ["string_content"], [","]). *)

  val subtree_equal : t -> t -> bool
  (** [subtree_equal c1 c2] returns true iff the subtrees at [c1]'s and [c2]'s
      current positions are structurally equal — same node types, same leaf
      text, same children structure. Used by the matcher for non-linear pattern
      matching (the same named wildcard appearing twice in a pattern must bind
      to identical subtrees).

      Implementations should prefer a hash-based fast rejection when available.
      The comparison is content-equality, not position-equality: two
      structurally identical subtrees at different source positions compare
      equal. *)

  val is_named : t -> bool
  (** [is_named c] is true iff the node at [c]'s current position is a named
      node — not an anonymous token like punctuation (commas, brackets). Used by
      transforms to filter separator leaves out of a sequence binding when
      rendering its elements. Implementations without the named/anonymous
      distinction (hand-built test trees) return [true]. *)

  val byte_range : t -> int * int
  (** [byte_range c] returns the source byte range [(start_byte, end_byte)] of
      the subtree at [c]'s current position. The half-open convention applies:
      the subtree spans bytes [\[start_byte, end_byte\)] in the source.

      For [Tree_sitter_cursor], this maps directly to the parsed node's
      positions. For test cursors over hand-built trees, the range is computed
      synthetically based on the implied rendering of the tree (concatenated
      leaf text in pre-order). *)

  val named_children : t -> t list
  (** [named_children c] returns sub-cursors, one per named non-extra child of
      the node at [c]'s current position. Each returned cursor is independent
      and scoped to its child's subtree — navigation from it does not escape
      past the child.

      Intended for partial-mode matching: the matcher iterates these sub-cursors
      and passes each to {!Stmatch.S.match_prefix} to consume one element's
      worth of pattern tokens.

      For implementations distinguishing named vs anonymous tokens (tree-sitter
      grammars), only named children are returned — punctuation like braces and
      commas are skipped. Implementations over hand-built tree fixtures with no
      named/anonymous distinction return all children. *)

  val all_children : t -> t list
  (** [all_children c] returns sub-cursors for {b every} non-extra child of the
      node at [c] — named children {b and} anonymous leaves (keywords,
      punctuation) — in document order. Like {!named_children} each returned
      cursor is scoped to its child's subtree.

      Intended for field-mode matching ({!Stmatch.S.match_field_at}), which must
      see structural keywords that are direct anonymous children of a
      declaration (e.g. Kotlin [fun], Scala [def], an [extends] clause's
      keyword) — these are invisible to {!named_children} but the pattern
      addresses them. Implementations without the named/anonymous distinction
      return the same list as {!named_children}. *)

  val leading_anonymous_leaves : t -> leaf list
  (** [leading_anonymous_leaves c] returns the leaves contributed by the run of
      anonymous (non-named) non-extra children that precede the first named
      non-extra child of the node at [c]. For a typical bracketed container this
      is the opening delimiter run — e.g. [[<]] for a JSX self-closing element,
      [[{]] for a TS object literal, [[(]] for a parenthesised expression.
      Returns [[]] when the node has no leading anonymous run (every child is
      named) or no children at all.

      Each anonymous child is normally a single leaf; if it has substructure,
      all of its non-extra leaves are flattened into the returned list in
      pre-order.

      Intended for source-driven delimiter handling in partial-mode matching:
      the driver consumes this many tokens from the front of the pattern and
      strict-matches them against these leaves, so the pattern's opening
      delimiter tokens align with whatever the source grammar actually produced.
      Hand-built test fixtures with no named/anonymous distinction should return
      [[]] (no opening delimiter inference). *)

  val trailing_anonymous_leaves : t -> leaf list
  (** [trailing_anonymous_leaves c] is the symmetric counterpart of
      {!leading_anonymous_leaves}: the run of anonymous non-extra children that
      follow the last named non-extra child. For a bracketed container this is
      the closing delimiter run — e.g. [[/>]] for a JSX self-closing element,
      [[}]] for a TS object literal. Returns [[]] if there's no trailing
      anonymous run. *)

  val source_substring : t -> int -> int -> string
  (** [source_substring c start_byte end_byte] returns the source text in the
      half-open byte range [\[start_byte, end_byte\)]. The cursor's current
      position is irrelevant — the slice comes from the underlying source.

      Intended for source-derived separator detection in partial-mode matching:
      given the byte ranges of two adjacent named children, the driver reads the
      bytes between them to determine what separator (if any) the source uses.
  *)
end
