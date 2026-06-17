(** End-to-end entry point for the universal-tokenizer matcher: a full pattern
    string (preamble + body) matched against source text.

    Ties together the sigil-free preamble parse, tokenization ({!Tokenize}),
    source parsing, and the matching outer loop ({!Stmatch}). This is the layer
    that lets a real pattern run through the new matcher without hand-building
    token lists.

    Scope: strict mode is wired end-to-end. Partial mode is wired for patterns
    that are a single bracketed container (e.g. [{a: $x}] or [<Foo a={$x} />]) —
    the matcher searches for source containers whose leading/trailing anonymous
    delimiters and named children satisfy the pattern (subset semantics for the
    children, unordered, extras tolerated). No hardcoded bracket table or
    set-like-container allowlist: the delimiter shape comes from the source
    structure itself, so the same pattern adapts uniformly across grammars. A
    single-section partial pattern must be a single bracketed container (its
    first and last tokens are anonymous delimiters); a non-container form like
    [foo({...})] is rejected at parse/compile time with a [Failure] pointing at
    the multi-section idiom. Composition — "a call whose argument contains X" —
    is expressed by a strict section that binds the container and an
    [on $VAR]-scoped [match: partial] section that subset-matches inside it.

    Field mode is wired: the pattern's leaf stream is aligned to a subsequence
    of a declaration node's children, skipping the optional fields the pattern
    omits (decorators, annotations, modifier groups, return types) while
    matching the ones it addresses, with backtracking (see
    {!Stmatch.S.match_field_at}). No per-language data is used.

    Multi-[\@\@]-section patterns are handled: sections run in declaration
    order, share metavar bindings by name, and may be scoped with [on $VAR] /
    [foreach $VAR] (see {!scope}). Transforms are applied by {!transform}:
    [-]/[+] bodies, single-metavar substitution, sequence splicing with [join]
    separators, per-element [foreach] edits, and element removal (an empty
    replacement) with separator cleanup. *)

module M : module type of Stmatch.Make (Tree_sitter_cursor)

type mode = Strict | Partial | Field

type scope =
  | Global
  | On of string
  | Foreach of string
      (** A section's scope directive. [Global]: match against the whole source.
          [On name]: scope to the single subtree bound by [name] in a prior
          section (run once). [Foreach name]: run once per element of the
          sequence bound by [name]. [On] requires a single metavar and [Foreach]
          a sequence; mismatches are parse errors. [name] is taken verbatim
          (sigil-free or with [$]). *)

type section = {
  mode : mode;
  scope : scope;
  single_metavars : string list;
  sequence_metavars : string list;
  joins : (string * string) list;
      (** [join $VAR by "<sep>"] directives: [(var, separator)]. The separator
          joins the rendered elements when [$VAR] is spliced into a replacement;
          the default (no directive) is the empty string. *)
  body : string;
}
(** One [@@]-delimited section of a pattern file. Multi-section patterns have a
    list of these; single-section patterns have one. *)

type parsed_pattern = { sections : section list }
(** A parsed pattern file. Always non-empty: a file with no [@@] delimiters or
    no sections raises a parse error. *)

type composite_match = { sections : M.match_result list }
(** One conjunctive match across all sections of a multi-section pattern.
    [sections] is in declaration order, one entry per section in the pattern.
    For single-section patterns this is a 1-element list. *)

val parse_pattern : string -> parsed_pattern
(** Parse a pattern's sections. Each section's preamble carries [match: …],
    optional [metavar <name>: <kind>] declarations, and an optional [on <name>]
    directive. Metavar names are arbitrary — no [$] prefix is required or
    assumed (sigil-free).

    Cross-section validation runs at parse time:
    - Same metavar name in multiple sections must use the same kind ([single] vs
      [sequence]).
    - An [on $VAR] directive must reference a metavar declared as [single] in
      some prior section.

    Raises [Failure] on a malformed preamble, missing match mode, kind
    contradictions, or unresolved [on $VAR] references. *)

type spatch_line =
  | Ctx of string
  | Del of string
  | Add of string
      (** A pattern body line with its spatch role: context ([Ctx], kept on both
          sides), a [- ] removal ([Del]), or a [+ ] addition ([Add]). The role
          marker is column 0 (unified-diff style); a context line has at most
          one leading role-indicator space stripped. *)

val classify_spatch : string -> spatch_line list
(** [classify_spatch body] parses a body into its per-line spatch roles. This is
    the structured form from which {!match_side} and {!replace_side} derive, and
    the basis surgical transforms build on (the [- ] lines locate what to
    delete, the [+ ] lines what to insert). *)

val match_side : string -> string
(** [match_side body] extracts the match side of a pattern body following
    diffract's spatch conventions: [- ] lines contribute their content, [+ ]
    lines are dropped, and any other line is context (one leading role-indicator
    space stripped if present). A pure-match body (no [-]/[+] lines) is returned
    with at most a leading space removed per line. Equal to the [Ctx]/[Del]
    content of {!classify_spatch}. *)

val replace_side : string -> string option
(** [replace_side body] extracts the replace side of a pattern body — the mirror
    of {!match_side} with the [-]/[+] roles swapped: [+ ] lines contribute their
    content, [- ] lines are dropped, context lines have a leading role-indicator
    space stripped.

    Returns [None] only for a pure-context body (no [- ]/[+ ] lines) — a
    match-only guard. A body with any [- ] or [+ ] line is a transform; in
    particular a body with [- ] lines and no [+ ] is a removal ([- foo] alone
    replaces the matched span with the empty string), per spatch convention. *)

val debug_tokens :
  ctx:Context.t -> language:string -> pattern_text:string -> string
(** [debug_tokens ~ctx ~language ~pattern_text] returns a human-readable dump of
    how each section's match body tokenizes: the [Concrete]/[Subtree]/
    [Siblings] tokens, plus the declared metavars with an [(ABSENT!)] marker for
    any that produced no token. Intended for the CLI's [--debug-tokens] flag, to
    diagnose why a pattern matches nothing. Unlike {!find}/ {!transform} it does
    {b not} run the declared-but-absent validation, so it works on malformed
    patterns (which is precisely when it is useful). *)

val pattern_warnings : string -> string list
(** [pattern_warnings pattern_text] returns non-fatal warnings from a static
    look at the parsed pattern (no source needed). Transforms are surgical, so a
    [-]/[+] on a sub-part is no longer a footgun; this flags only a partial- or
    field-mode section that marks the {b whole container} (a body with no
    context line, so every matched token is removed and the edit spans the whole
    match). That drops the very content those modes tolerate/ignore (extra
    container elements; optional declaration fields) — legitimate as a wholesale
    rewrite, but an easy mistake, so it warns rather than fails. A sub-part edit
    (context lines preserved) or a [foreach]-scoped edit is exempt. Intended for
    the CLI to print before applying. *)

val find_in_tree :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  Tree.src Tree.tree ->
  composite_match list
(** Like {!find}, but against an already-parsed source tree. Useful when the
    caller also wants parse diagnostics ({!Tree.error_count}) from the same
    parse, avoiding a second parse of the source. *)

val text_only_find_in_tree :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  Tree.src Tree.tree ->
  composite_match list
(** Matches of a {b single-section strict} pattern comparing [Concrete] leaves
    on text alone (ignoring node-type). Backs the [search --explain] hint: when
    a strict search finds nothing, these are the locations whose tokens occur as
    text but in a different syntactic role (why strict rejected them). Returns
    [[]] for anything but a single global strict section — the hint is scoped to
    where that context-sensitivity bites, not partial/field/multi-section. *)

val find :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  composite_match list
(** [find ~ctx ~language ~pattern_text ~source_text] parses the pattern, runs
    each section in declaration order, and returns composite matches: tuples of
    per-section matches where all sections matched successfully with consistent
    bindings.

    For single-section patterns the return value is a list of 1-element
    composites — one per section match. Cross-section non-linearity is enforced
    by threading prior sections' bindings as [initial_bindings] into each
    subsequent section's matcher. A section with an [on $VAR] directive runs
    scoped to the subtree bound by [$VAR] in a prior section.

    Strict, partial, and field sections are all supported. *)

val find_file :
  ctx:Context.t ->
  language:string ->
  pattern_file:string ->
  source_text:string ->
  composite_match list
(** Like {!find}, but reads the pattern text from the file at [pattern_file]. *)

val transform :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  string
(** [transform ~ctx ~language ~pattern_text ~source_text] applies the pattern's
    replacement side to [source_text] and returns the rewritten source. Each
    section's [+] lines form a replace template; single metavar placeholders
    ([$NAME]) are substituted with the source text of their bindings.

    Matching is non-overlapping (each source span is rewritten once, greedily
    left-to-right), in contrast to {!find}'s overlapping search. A match-only
    pattern (no [-]/[+] lines) returns [source_text] unchanged.

    Transforms are {b surgical} in all modes (see
    [docs/surgical-transforms.md]): a [-]/[+] on a sub-part edits only that
    part, leaving the rest of the match — context, partial's tolerated extra
    elements, field's ignored optional fields (decorators, modifiers, return
    types), and the source captured by [...] — byte-for-byte. A pattern whose
    [-]/[+] cover the whole match replaces the whole match (the degenerate
    single-region case). Deletion cleanup is context-aware: strict deletions are
    line-oriented; partial/field deletions absorb an adjacent list separator so
    it doesn't dangle.

    Beyond single-metavar substitution this handles: sequence splicing (a
    [sequence] metavar referenced in a [+] template is rendered and joined by
    its [join] separator), per-element [foreach] transforms (an inner section's
    edit applied to each element of a sequence), and element removal (a
    [foreach] element with an empty replacement is deleted, and one adjacent
    separator is consumed so the list stays well-formed). *)

type edit = { start_byte : int; end_byte : int; replacement : string }
(** One edit {!transform} would apply: replace [source_text]'s bytes
    [\[start_byte, end_byte)] with [replacement]. *)

val transform_edits :
  ctx:Context.t ->
  language:string ->
  pattern_text:string ->
  source_text:string ->
  edit list
(** The edits {!transform} would apply, without applying them — the same
    matching and template instantiation, surfaced as spans. Sorted by
    [start_byte], deduplicated. Used by callers that need to *inspect* a
    transform's effect against other information about the source (e.g. the
    change-summary safety gate comparing edits against a diff's changed regions)
    rather than the rewritten text. *)

val apply_edits : string -> edit list -> string
(** Apply an edit list to a source text.
    [apply_edits src (transform_edits ... src)] equals [transform ... src] —
    callers that already hold the edits (the safety gate inspects them first)
    can apply them without re-running the match. Edits are deduplicated and must
    be non-overlapping; a nested or conflicting pair raises [Failure]. *)
