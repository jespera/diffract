(** Pattern tokenization: pattern body text -> [Stmatch.pattern_token list].

    Parses the pattern body with the source language's tree-sitter parser and
    walks the resulting tree's leaves in document order, classifying each leaf:

    - text equals a declared [single] metavar name -> [Subtree] wildcard
    - text equals a declared [sequence] metavar name -> [Siblings] wildcard
    - an ellipsis [...] -> anonymous [Siblings] wildcard (uniquely named
      [..._0], [..._1], ...)
    - anything else -> [Concrete] literal token (text + observed node type)

    Ellipsis handling: before parsing, each standalone [...] is rewritten to a
    unique placeholder identifier so the body parses; the placeholder leaves are
    recognized and converted back to [Siblings] wildcards. A [...] immediately
    followed by an identifier or [$] (a spread/rest operator such as [...args]
    or [...$rest]) is preserved as a literal [...] token, not treated as
    ellipsis.

    Tree-sitter is used only as a lexer here: the parse tree's hierarchy is
    discarded and only the leaves are kept, so ERROR nodes in the pattern's
    parse are harmless.

    Metavar detection is {b sigil-free}: a leaf is a metavar iff its text
    exactly matches a declared metavar name. No [$] prefix is assumed — the
    preamble's declarations are the sole authority. This treats [obj] and [$obj]
    identically; whichever the preamble declares is what's matched.

    Assumption: a metavar name tokenizes to a single leaf (true for ordinary
    identifier names across the supported languages). Names that parse as
    multiple leaves (e.g. a PHP [$x] that splits into [$] + [x]) are not yet
    handled. *)

val tokenize :
  ctx:Context.t ->
  language:string ->
  single_metavars:string list ->
  sequence_metavars:string list ->
  string ->
  Stmatch.pattern_token list
(** [tokenize ~ctx ~language ~single_metavars ~sequence_metavars body] returns
    the token list for [body]. Extras (comments, whitespace tokens) are skipped.
*)

val tokenize_with_lines :
  ctx:Context.t ->
  language:string ->
  single_metavars:string list ->
  sequence_metavars:string list ->
  string ->
  (Stmatch.pattern_token * int) list
(** Like {!tokenize}, but pairs each token with the 0-based line index of its
    leaf in [body]. Ellipsis preprocessing preserves newlines, so the index
    matches the line in the original [body] — letting a caller recover the
    [-]/context role of each token from the pattern line it came from
    (surgical-transforms groundwork). *)

val tokenize_span :
  ctx:Context.t ->
  language:string ->
  single_metavars:string list ->
  sequence_metavars:string list ->
  lo:int ->
  hi:int ->
  string ->
  Stmatch.pattern_token list
(** [tokenize_span ~ctx ~language ~single_metavars ~sequence_metavars ~lo ~hi
     text] tokenizes [text] but keeps only the leaves whose start byte is in the
    half-open range [\[lo, hi)]. No ellipsis preprocessing is performed, so byte
    offsets in [text] are stable; callers must therefore only use this on
    ellipsis-free text.

    This supports {b contextual tokenization}: a pattern fragment is spliced
    into a larger scaffold (so the parser assigns its leaves the node-types they
    would have in that context — e.g. an object key as [property_identifier]
    rather than the standalone [statement_identifier]), then the fragment's own
    leaves are extracted by their byte span. *)
