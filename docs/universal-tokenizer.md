# Universal tokenizer matcher: design note

> **Status: implemented and shipped — this is the original design note,
> kept for rationale.** The matcher described here is now the project's
> *only* matcher (`lib/tokenize.ml`, `lib/cursor.ml`,
> `lib/tree_sitter_cursor.ml`, `lib/stmatch.ml`, `lib/matcher.ml`). The old
> AST matcher it contrasts against (the `Match_*` modules) and the abandoned
> DEL "hybrid" attempt have both been deleted, along with `hybrid-matcher.md`.
> All three match modes are built — see the as-built notes for what actually
> shipped: [field-mode.md](field-mode.md), [partial-mode.md](partial-mode.md),
> [transforms.md](transforms.md), and [internals.md](internals.md). Sections
> below written in future/planning tense ("deferred", "not yet implemented",
> the implementation-phases plan, references to `Match_*`/`Hybrid_*`/the
> `--hybrid` flag) describe the original plan, not the current tree.

## 1. Motivation

The current production matcher (`Match_*` modules on main) parses
patterns as language fragments and matches AST-to-AST. This
architecture has accumulated per-language workarounds — the
`unwrap_root` whitelist, `try_match_children_directly`, the PHP
`php_tag` carve-out — and can't express patterns in languages with
strict top-level grammars (Java, Rust).

The DEL-based hybrid matcher attempt (`Hybrid_*` modules on the
`hybrid-matcher` branch, `docs/hybrid-matcher.md`) sidesteps
fragment parsing by introducing per-language DEL definitions
(bracket pairs, string boundaries, comment markers) used to lex
patterns and to bracket-aware scan sources during matching. Phase 1
smoke testing surfaced two scanner limitations (§8.1 angle brackets,
§8.2 whitespace-only literals) that have principled fixes but
require additional per-language scaffolding on top. This document
abandons the DEL hybrid in favor of the architecture below; the
`Hybrid_*` modules are deleted as part of the refactor.

The stsearch paper (Matute et al., PLDI 2024, "Syntactic Code Search
with Sequence-to-Tree Matching") describes a different architecture
that resolves both Section 8 issues structurally:

- Patterns are tokenized by reusing the source language's parser in
  error-tolerant mode and extracting leaves. No DEL definition; no
  custom lexer.
- The matcher (STMatch) is sequence-against-tree: a pattern token
  list walked against a source tree via a small cursor interface.
  The source AST is the structural ground truth for whitespace
  handling, bracket depth, and operator-vs-delimiter disambiguation.
- Tree-sitter "extras" (whitespace, comments) are skipped by the
  cursor's navigation methods, so the matcher never thinks about
  them.

This design adopts that architecture, adapts it to diffract's metavar
semantics (named single and sequence holes), and lays out a migration
path. The reference implementation
(`plait-lab/stsearch`, MIT-licensed) is ~88 LOC of Rust for the
matcher core and ~77 LOC for tree-sitter integration; we port the
algorithm and adapt the integration for OCaml and diffract's specific
feature set.

Change-summary (the primary downstream consumer) benefits directly:
`pat_node` patterns no longer need to be rendered to text and
re-parsed to be matched; they convert to a token sequence via a leaf
walk and feed the same matcher. This eliminates the rendering /
re-parsing round-trip currently in the applicability-check path.

## 2. Architecture

### 2.1 Three layers

**Pattern tokenization.** A pattern's body is tokenized by parsing it
with the source language's tree-sitter parser (with error recovery
always on) and walking the resulting tree's leaves in order. Each
leaf's text is classified:

- Matches a preamble-declared `single` metavar name → subtree
  wildcard with that name.
- Matches a preamble-declared `sequence` metavar name → siblings
  wildcard with that name.
- Matches the per-language ellipsis placeholder (e.g. `/**/`) →
  anonymous siblings wildcard with a synthetic name (`..._0`).
- Anything else → concrete literal token.

Tree-sitter is invoked, but only as a lexer: the parse tree's
hierarchical structure is discarded; only the leaves are kept. ERROR
nodes in the pattern's parse don't matter because we never consult
node types or hierarchy on the pattern side.

**Source parsing.** Unchanged. The source file is parsed by
tree-sitter into a full AST (`Tree.src Tree.t`).

**Matching.** A small algorithm (~100 LOC after porting STMatch)
walks the pattern token list against the source AST via a cursor
interface. The cursor's surface is small — five navigation
operations (`move_first_leaf`, `move_first_child`,
`move_next_subtree`, `move_next_sibling`, `clone`) plus
accessors on the returned leaf type. Later phases extend the
interface with `node_type`, `field_name`, `byte_range`,
`named_children` as partial / field modes and named bindings
require them. The matcher has no other dependency on tree-sitter
or on `Tree`. Navigation is mutating per stsearch's reference
(§6.5); `clone` provides snapshots for the checkpoint stack.

Concrete literal tokens carry both their text *and* the node type
the tokenizer observed for that leaf. Leaf equality at match time
compares `(text, node_type)` — not text alone. This is a
deliberate extension over stsearch's text-only comparison and
disambiguates leaves that happen to share text but represent
different constructs (e.g. Kotlin's identifier `hello` vs string
content `"hello"`). See §3.10.

### 2.2 The asymmetry made explicit

The pattern's *tokenization* uses tree-sitter for convenience but
doesn't depend on the parse tree being meaningful. The source's
*AST* is the structural ground truth — bracket depth,
operator-vs-delimiter, whitespace location, field names, child
boundaries all live there.

This is genuinely different from:

- **Comby / DEL hybrid:** text bracket-scanning on both sides, no AST.
- **ast-grep / Semgrep:** AST on both sides, requires parseable
  patterns.
- **Wrap-and-peel:** AST on both sides, with per-language wrapping to
  parse partial patterns.

The asymmetry is what makes diffract's matcher language-agnostic
without per-language wrapping: each language's tree-sitter grammar is
the only language-specific data, and it's already linked.

### 2.3 What survives, what changes, what goes away

**Survives unchanged.**

- `Tree.src Tree.t` and most of the existing `Tree` module.
- `Tree_sitter_bindings` and the C FFI layer.
- `Languages` registry.
- `Grammar_metadata` (retaining all DEL definition accessors for reuse
  in the new tokenizer, and extended with separator extraction from
  grammar.json).
- `change-summary` pipeline up to and including anti-unification,
  the dendrogram, and cluster cutting. Only the applicability-check
  path changes (becomes direct).
- `Match_transform`'s replacement and expansion machinery
  (`instantiate_template`, `apply_expansion_slots`,
  `transform_nested`). These operate on bindings + text, which is
  unchanged.

**Changes substantially.**

- The matcher: replaced by `Matcher` (functor over a cursor
  interface), implementing STMatch and the partial / field
  extensions.
- The pattern parser: replaced by `Tokenizer`, which produces a
  token sequence via tree-sitter's parser-as-lexer.
- The top-level `Match.find_matches` API: input changes from
  `Pattern.t` to `Token.t list`; output structure is similar but
  bindings now carry cursor positions in addition to text.

**Deleted upfront** (abandoned without migration).

- `Hybrid_lexer`, `Hybrid_pattern`, `Hybrid_matcher` — the DEL
  hybrid attempt on the `hybrid-matcher` branch. Not load-bearing
  for production; the only consumer is a `--hybrid` CLI flag added
  for smoke testing. Deleted in Phase 0 before any new code is
  written.
- The `--hybrid` CLI flag and any tests gated on it (the `Grammar_metadata` DEL accessors themselves are kept and repurposed for the new matcher).

**Goes away after migration** (replaced by the new matcher).

- `Match_engine`, `Match_search`, `Match_parse`, `Match_types` —
  the existing production matcher.
- `Tree.unwrap_root` and the wrapper whitelist.
- `Tree.parse_as_pattern` and any code that parses patterns as full
  language fragments.
- `Match_engine.try_match_children_directly` and similar AST
  fallbacks.
- PHP `php_tag` carve-out and related per-language workarounds.
- The substitution-to-placeholder-and-reparse dance for ellipsis
  context detection.

## 3. Per-feature mapping

### 3.1 Strict mode

STMatch directly. Pattern token sequence walked against source AST
via cursor. Wildcards bind to subtrees with left-spine backtracking;
literals match leaves on both text and node type (see §2.1). Both
pattern and cursor must end together for the match to succeed.

### 3.2 Partial mode

Outer-bracket structure of the pattern is identified by walking
tokens with universal-bracket depth tracking (`()[]{}`). The interior
is split by a separator derived from the source AST's parent node
type (via grammar.json: see §6.2). Each pattern child is matched
against some source child via STMatch (recursive call with the
pattern child as input). An injective assignment that covers all
pattern children succeeds; any pattern child without a source
counterpart fails the match.

### 3.3 Field mode

Field mode is the one match mode not yet implemented, and it is
harder than this section originally assumed. The naive design —
walk source children, skip any whose first leaf doesn't match the
pattern — is **wrong**: it skips required content too (a pattern
`f()` would match `f(a, b)` by skipping the parameters). The correct
semantic skips only *optional* fields, keyed on named-child node
types (not field labels — Kotlin has none), with optionality drawn
from the grammar. Even then, TS anonymous-keyword modifiers
(`async`, `abstract`) remain unreachable.

See `docs/field-mode.md` for the full analysis: the use case, why
the naive approach fails, the per-grammar findings, the viable
mechanism, the anonymous-modifier gap, and the overlap with partial
mode. Field mode is deferred until tokenization lands so the design
can be validated against real pattern files.

### 3.4 Single and sequence metavars

The only two kinds diffract has. Map directly to stsearch's
`Wildcard::Subtree` (single) and `Wildcard::Siblings` (sequence).
Both carry a name field for diffract's binding lookup and
replacement needs.

### 3.5 Ellipsis `...`

Anonymous sibling wildcards. The tokenizer rewrites `...` to a
per-language "always-extra" placeholder (e.g., `/**/` for C-family
languages) before parsing the pattern. The placeholder survives
parsing as a leaf with that exact text; the leaf walk converts it
back to `Wildcard::Siblings` with a synthetic name (`..._0`,
`..._1`, ...).

### 3.6 Conjunctive sibling sections and `on $VAR`

Each `@@` section is processed independently with its own
tokenization and matching. The harness above (all-sections-must-
match, inner-pattern-targets-binding) is unchanged. Inner-section
matching uses the same matcher with the bound subtree as the
starting cursor.

### 3.7 Expansion lines

Replacement-side text generation. The expansion slots and join
separators are unchanged structurally; they consume bindings +
source text. May or may not adopt auto-inferred separators from
grammar.json (see §6.3 — independent of the matcher refactor).

### 3.8 Change-summary

`change_summary.ml`'s `pat_node` converts to a token list via a leaf
walk: each `Leaf { value; _ }` becomes a `Concrete value` token;
each `Hole h` becomes a `Wildcard::Subtree { name = "H" ^
string_of_int h }`. The same matcher consumes this token list. No
text rendering, no re-parsing. `render_spatch` becomes display-only.

### 3.9 Partial patches (deferred)

Pattern shapes like `foo(` or `} else {` (unbalanced bracket
sequences targeting structural boundaries) remain out of scope. The
matcher requires patterns where every wildcard can bind to a
complete subtree of the source. Lifting this restriction would
require accepting that a hole's binding range doesn't coincide with
an AST node — a focused future addition.

Note: §6.4's empirical validation showed that *tokenization* of
such partial patterns works fine under raw fragment parsing — the
remaining gap is purely on the matcher side, not the tokenizer.

### 3.10 Per-language tokenization properties

Some tree-sitter grammars structurally collapse or omit tokens
that distinguish syntactic constructs. These are properties of the
grammars themselves, not of our approach. Two consequences for
matching:

**(a) Omitted tokens can't be matched.** A pattern that tries to
target a token the grammar removes will silently fail to find
anything.

**(b) Collapsed tokens create matching ambiguity.** When two
distinct source constructs produce the same leaf sequence, a
pattern matches both indistinguishably if the matcher only compares leaf text. 
However, if the matcher compares **both leaf text and leaf node type** (which the sequence-to-tree matcher natively does), this ambiguity is resolved in most cases.

Concrete cases:

**Kotlin.** The `tree-sitter-kotlin` grammar omits the surrounding
`"` quotes and the leading `$` / `$$$` interpolation markers from
interpolated string literals, but preserves the static string
content as a leaf. A source `val s = "hello $name"` tokenizes to
leaves like `[val, s, =, "hello ", name]`.

Consequence (a): patterns can't target the `$` interpolation
prefix or the quote characters — they're not leaves.

Consequence (b) (Resolved by type-checking): 
- The leaf token `hello` from a string's static content has type `string_content`.
- The leaf token `hello` from an identifier reference has type `simple_identifier`.
By comparing both **type and text** at the leaf level, the matcher distinguishes them:
- `foo(hello)` (where `hello` is parsed as `simple_identifier`) will **not** match `foo("hello")` (where `"hello"` is parsed as `string_content`).
- `foo("hello")` will correctly match only the string literal.

**Scala.** The `tree-sitter-scala` grammar omits static string
contents from the AST entirely for interpolated strings. A source
`s"hello $name"` tokenizes to leaves like `[s, ", $, name]` — the
static `"hello "` is absent.

Consequence (a): patterns can't match static text inside Scala
interpolated strings; that text simply isn't a leaf.

Consequence (b): two interpolated strings with different static
parts but the same interpolated identifier produce the same leaf
sequence. `s"hello $name"` and `s"goodbye $name"` are
indistinguishable at the matcher's leaf level. 

*Mitigation*: If we need to distinguish them, we can add a fallback check when matching an `interpolated_string_expression` node type that compares their raw text range (via `Tree.text`). Since change-summary and standard refactorings rarely target the interiors of Scala interpolated strings, this fallback check is deferred.

(Plain non-interpolated Scala strings have not been separately
empirically verified; behavior may differ.)

**TypeScript / TSX / PHP.** No comparable collapsing was observed
in §6.4's validation. String contents appear as leaves; quotes
appear as leaves; matching distinguishes strings from
identifiers.

**Net implication for diffract.** For change-summary's primary
pipeline and for typical refactoring rules (matching identifiers,
call shapes, field accesses, control flow), these collapses are
not significant — the patterns in question don't usually target
string-literal interiors. For rules that need to match specific
string contents (style rules, security checks like "find calls to
`exec` with a literal first argument"), the current matcher can't
express the distinction precisely on Kotlin and Scala.

Possible future mitigations, not in initial scope:

1. **Pattern tokens with optional node-type constraints.** The
   cursor exposes `node_type`; the matcher could require a literal
   pattern token to match only leaves whose parent node type is
   `string_literal` (or similar). This needs preamble syntax to
   express the constraint and is essentially a poor man's
   type-restricted hole for literals. See §6.7.
2. **Synthetic markers at the tokenizer.** When the pattern's
   tokenizer encounters a string literal, emit a synthetic marker
   leaf alongside the content; require the source to have the same
   marker. Re-creates the abstraction the grammar removed, in our
   token stream. More invasive.

Neither is needed for change-summary; both are deferrable.

## 4. Module structure

Tentative layout (subject to revision during prototyping). Flat,
matching the existing `lib/` and `tests/` convention:

```
lib/
  cursor.ml{,i}             — Cursor module type + helpers
  stmatch.ml{,i}            — Algorithm: STMatch + prefix variant
  matcher.ml{,i}            — Functor: match_strict/partial/field
  tokenizer.ml{,i}          — Pattern body → token sequence
  preamble.ml{,i}           — Pattern preamble parsing
  tree_sitter_cursor.ml{,i} — Cursor for Tree.src Tree.t
  pat_node_cursor.ml{,i}    — Cursor for change-summary's pat_node
  outer_loop.ml{,i}         — Pattern.find_iter equivalent

tests/
  test_cursor.ml            — Test cursor implementation + DSL
  test_stmatch.ml           — Algorithm tests on hand-built trees
  test_tokenizer.ml         — Pattern lexing tests per language
  test_integration.ml       — End-to-end via tree-sitter cursor
  test_raw_vs_wrapped.ml    — Phase 0 invariant (already landed)
```

Naming conflict during the parallel-build phases (1-9): the
existing top-level `lib/match.ml` is reused by the new matcher
once Phase 10 deletes the old `Match_*` modules. Until then, the
new entry point lives as `Matcher` (in `lib/matcher.ml`) and
`Match` continues to point at the old implementation.

The `Hybrid_*` modules are already deleted in Phase 0 (done).
`Match_engine`, `Match_search`, `Match_parse`, `Match_types`,
`Tree.unwrap_root`, and `Tree.parse_as_pattern` are deleted in
Phase 10.

## 5. Implementation plan

Eleven phases. Each phase ends with a state where tests pass and the
codebase compiles. Phase 0 removes the abandoned DEL hybrid before
new code is written. Phases 1-7 build the new matcher in parallel
with the existing `Match_*` matcher; phases 8-10 migrate call sites
and delete the old production code.

### Phase 0: Delete the DEL hybrid attempt

Deliverables:

- Remove `lib/hybrid_lexer.ml{,i}`, `lib/hybrid_pattern.ml{,i}`,
  `lib/hybrid_matcher.ml{,i}`.
- Remove `lib/hybrid_lexer`, `Hybrid_pattern`, `Hybrid_matcher`
  references from `dune` files.
- Keep and repurpose the DEL accessors in `Grammar_metadata.ml{,i}`
  (`del_definition`, `bracket_pairs`, `string_defs`, `line_comments`,
  `block_comments`). These are retained for ellipsis placeholder selection,
  early bracket validation, and robust leaf classification in `Tokenizer`.
- Remove `--hybrid` CLI flag from `bin/main.ml` and any tests gated
  on it (`tests/test_hybrid_lexer.ml`,
  `tests/test_hybrid_matcher.ml`).
- Port the §7.1 / §6.4 raw-vs-wrapped validation into a real
  `dune test` at `tests/test_raw_vs_wrapped.ml`, wired into
  `tests/test_runner.ml` as the "Raw vs Wrapped" group. The test
  enumerates pattern fragments per language with their wrapping
  templates, parses each both ways via tree-sitter (using the
  existing `Tree_sitter_bindings` / `Tree` infrastructure — no
  new matcher code needed), extracts leaf sequences (skipping
  tree-sitter extras), and asserts identity on both leaf text and
  `node_type`. This makes "§7.1-style validation" a first-class
  regression guard: a tree-sitter grammar bump that breaks the
  assumption fails CI; adding a new language means adding rows to
  the `test_cases` table.
- Confirm: existing `Match_*` tests still pass.

This phase exists explicitly so that the new work isn't littered
with comparison code or two-matcher branching. The decision to
abandon the DEL *hybrid matcher* is final; nothing in subsequent
phases references `Hybrid_*` modules. The DEL *metadata*
(`Grammar_metadata` accessors for bracket pairs, string defs,
comment markers) is retained and repurposed — see Phase 3
(ellipsis placeholder), Phase 6 (bracket depth tracking), and
the tokenizer's leaf classification.

### Phase 1: Cursor interface + test cursor + STMatch core

Deliverables:

- `Cursor.S` module type with the Phase 1 minimal surface: four
  navigation operations (`move_first_leaf`, `move_first_child`,
  `move_next_subtree`, `move_next_sibling`), one snapshot
  operation (`clone`) for backtracking, and a `leaf` type with
  `leaf_text` / `leaf_node_type` accessors. Later phases extend
  the module type with `node_type` (Phase 6/7 for partial / field
  modes on internal nodes), `field_name` (Phase 7), `byte_range`
  (Phase 4 for bindings), and `named_children` (Phase 6).
- `Test_cursor : Cursor.S` over an immutable in-memory tree with
  a mutable zipper-style cursor for navigation. Tree-building DSL
  (`leaf "ident" "foo"`, `node "call" [...]`) for fixture
  construction.
- `Stmatch.match_at : pattern_token list -> C.t -> C.t option`
  ported line-for-line from `stsearch/src/stmatch.rs` (iterative
  loop with explicit `checkpoints` stack; mutable cursor
  navigation; two-checkpoint trick for the `Siblings` wildcard).
  Extended so leaf equality compares `(text, node_type)` rather
  than text alone — exactly one site change vs the Rust source
  (the `Item::Concrete` branch). See §2.1 and §3.10.
- Algorithm tests (~30 cases) covering strict-mode literal
  matching (text and type both required — same-text-different-
  type must fail as the Phase 1 regression guard for the
  type-aware extension), subtree wildcards (binding and left-
  spine backtracking), sibling wildcards (zero, one, many; back-
  tracking through split points), mixed patterns, edge cases.
- Cross-validation: port the stsearch reference fixtures from
  `algorithm.py` lines 71–105 (the `assert match(...)` and
  `assert not match(...)` cases) into OCaml. Running them
  against `Stmatch.Make(Test_cursor)` provides independent
  evidence the port is correct — if our cursor and matcher pass
  the same fixtures the reference does, the algorithmic
  semantics are faithful.

No tree-sitter dependency anywhere in this phase. Pure algorithm
code, fast tests. Wildcards are anonymous in Phase 1; named
bindings (a `name : string` field on `Subtree` / `Siblings`)
arrive in Phase 4 as a non-breaking extension.

### Phase 2: Tree-sitter cursor implementation

Deliverables:

- `Tree_sitter_cursor` implementing `Cursor.S` for `Tree.src Tree.t`.
- Extras-skipping in `first_child` and `next_sibling` (replicate
  stsearch's pattern from `code/mod.rs`).
- Smoke test: hand-construct one simple pattern programmatically
  and match it against one parsed source file via STMatch.

### Phase 3: Pattern tokenization

Deliverables:

- `Preamble` parser (extract `match: <mode>`, `metavar X: single`,
  `metavar Y: sequence`, `on $VAR` directives, expansion-line
  preamble entries).
- `Tokenizer.tokenize_body` that parses pattern body via tree-sitter
  in error-tolerant mode (no wrapping — see §6.4), walks leaves,
  classifies each as literal / single-wildcard / sequence-wildcard
  based on the preamble. Literal tokens carry both their text and
  the leaf's `node_type` for type-aware comparison at match time.
- Ellipsis placeholder derivation from `Grammar_metadata`'s block /
  line comment markers (see §6.1). No hardcoded language table.
- Tokenizer tests: feed pattern strings per language, assert
  expected `Token.t list` output (literal text + node_type, hole
  names, ellipsis placeholders). These complement the raw-vs-
  wrapped tree-sitter validation added in Phase 0; this phase's
  tests cover the tokenizer's own classification logic on top of
  the parse, not the parse itself.

### Phase 4: Named bindings and outer loop

Deliverables:

- Extend STMatch to record bindings: each `Wildcard::Subtree {
  name }` match captures `(name, start_cursor, end_cursor)`.
- Sibling wildcards capture the list of consumed sibling subtrees.
- Outer-loop function `Matcher.find_matches` analogous to
  stsearch's `Pattern::find_iter`: walks the source tree trying
  STMatch at each candidate position, yields matches.
- Bindings include byte ranges (computed from cursor positions).
- Round-trip tests: bindings produce correct text from source.

### Phase 5: Anchor search

Deliverables:

- Pick the first concrete literal token as anchor (stsearch's
  default).
- Build an index over source: pre-order walk collecting leaf
  positions whose text matches the anchor.
- Outer loop uses the index instead of brute-force walk.
- Benchmark against brute-force on a representative source file.

### Phase 6: Partial mode

**Status: v1 landed.** Whole-pattern partial works across grammars
with no per-language config. The design is written up in
[`docs/partial-mode.md`](partial-mode.md); the short version is:

- The cursor exposes `leading_anonymous_leaves` and
  `trailing_anonymous_leaves` for each candidate source container.
- `Stmatch.match_partial_at` strict-matches the pattern's front/back
  tokens against those runs and set-matches the middle against the
  source's named children.
- Separators are inferred per-call from the bytes between the source's
  first two named children — no grammar.json walk needed.
- Requiring both anonymous runs non-empty is the structural
  self-filter that distinguishes real bracketed containers from
  positional declarations.

The originally-planned outer-bracket detection, grammar-derived
separator table, and source-children fallback are all subsumed by
the source-driven approach. Composition (a partial container nested
inside a strict outer pattern, like `foo({a: $x})`) is not yet wired
— see [`docs/partial-mode.md`](partial-mode.md) §7 for the design
and [the shared per-language registry](partial-mode.md) it would
share with field mode.

### Phase 7: Field mode

Deferred until after tokenization; the design needs validation against
real `match: field` patterns. The skip-on-mismatch sketch this phase
originally carried is wrong (it skips required content). The revised
mechanism — skip optional named children, keyed on node type with
grammar-derived optionality — and its open questions are written up in
`docs/field-mode.md`. `Stmatch.match_prefix` (the prefix variant) is
already implemented as part of partial mode and will be reused.

### Phase 8: Migrate existing tests

Deliverables:

- Route existing diffract tests through the new matcher via a test
  harness flag.
- Fix failures (expected to be edge cases that previously needed
  workarounds — ellipsis handling, whitespace-bounded holes, the
  `pair` vs `labeled_statement` case).
- All 206 existing tests pass on new matcher.

### Phase 9: Change-summary direct path

Deliverables:

- `Pat_node_to_tokens` converter (leaf walk emitting Concrete /
  Wildcard tokens).
- `Change_summary.find_applicable_instances` uses the new matcher
  directly with a token list instead of rendering and re-parsing.
- Benchmark on a real corpus to confirm performance improvement.
- `render_spatch` retained for display-only output (not consumed by
  the matcher).

### Phase 10: Delete old production matcher

Deliverables:

- Remove `Match_engine`, `Match_search`, `Match_parse`,
  `Match_types` (the existing production matcher; replaced by
  `Matcher`).
- Remove `Tree.unwrap_root`, `Tree.parse_as_pattern`.
- Update `Match.mli` to reflect the new surface (Token-based input,
  cursor-based bindings).
- Update CLAUDE.md to match.

(`Hybrid_*` modules were already removed in Phase 0.)

## 6. Open questions

### 6.1 Per-language ellipsis placeholder

`...` substitution dynamically constructs the ellipsis placeholder using
the block comment (e.g., `/**/` for `/* ... */`) or line comment openers
defined in `Grammar_metadata.del_definition ~language`. This avoids having
to maintain a hardcoded language-to-placeholder table and ensures that new
languages work out-of-the-box.

### 6.2 Separator inference (resolved)

The originally-planned grammar-derived separator table (from
`grammar.json` REPEAT-of-(STRING+SYMBOL) shapes) and the
no-separator fallback turned out to be unnecessary. Partial mode
infers each container's separator from the bytes between the
source's first two named children (trimmed of whitespace) and is
lenient when no separator is present (JSX-style attribute lists).
See [`docs/partial-mode.md`](partial-mode.md) §5.

### 6.3 Auto-inferred expansion-line separators

Optional simplification of the expansion-line syntax: when expanding
a sequence metavar, the separator can be inferred from the source
binding (the text between the bound nodes). Makes the column-0
prefix-character syntax optional rather than mandatory. Independent
of the matcher refactor; can be done before, during, or after.

### 6.4 Tree-sitter parser-as-lexer caveats per language (Validated for current languages)

We empirically validated the parser-as-lexer behavior on raw pattern fragments across all supported languages (TypeScript, TSX, Kotlin, PHP, Scala). We compared raw fragments (relying on tree-sitter error recovery) side-by-side with wrapped fragments (enclosed in a valid dummy function/object context to ensure no parser errors, then stripped of wrapper prefix/suffix tokens). For TSX specifically, a dedicated spike additionally verified that the syntactic ambiguities `<>` introduces (JSX element tags vs. generic type instantiations vs. binary comparisons) are correctly resolved at the raw fragment level with precise leaf-type typing.

In every single case, the leaf token sequence extracted from the raw fragment was **100% identical** to that of the wrapped fragment:

- **TypeScript / TSX**: 
  - Raw fragments like `foo / $x / g` and `} else {` parse identically to wrapped versions. The external scanner successfully disambiguates division `/` vs regex `/` and punctuation brackets in both states.
  - TSX syntactic ambiguities (JSX elements vs. generic type arguments vs. binary comparisons) are correctly resolved under raw fragment parsing. For example, `<Foo>` correctly parses as a `jsx_opening_element` (where `Foo` is typed as an `identifier`), `foo<Bar>` correctly parses as an `instantiation_expression` (where `Bar` is typed as a `type_identifier`), and `a < Foo > b` correctly parses as a nested `binary_expression` (where `Foo` is typed as an `identifier`). The matcher's leaf-type validation prevents incorrect cross-matching.
- **Kotlin**: Templates like `val s = $"hello $name"` and `val s = $$$"foo $$$bar"` parse identically. Both raw and wrapped parses yield the exact same leaf tokens (e.g. `[val, s, =, "hello ", name]` and `[val, s, =, "foo ", bar]`). The leading `$` or `$$$` delimiters and the quotes are abstracted out of the AST child hierarchy by the `tree-sitter-kotlin` grammar itself and are never exposed as child nodes, regardless of whether there are parser errors.
- **PHP**: Raw statements and fragments (e.g. `function $f() { $body`) parse to the exact same leaf tokens as wrapped ones.
- **Scala**: Interpolated strings like `s"hello $name"` parse to the exact same leaf tokens as wrapped ones: `[s, ", $, name]`. (Note: the `tree-sitter-scala` grammar omits static string contents like `"hello "` from the AST child hierarchy entirely; this is a property of the grammar itself, not a parser error byproduct).

**Conclusion**: For the supported languages and a representative
pattern corpus, raw tokenization produces leaf sequences identical
to wrapped tokenization. We will implement a generic, wrapping-free
`Tokenizer`; adding a new language requires re-running this
validation against patterns characteristic of that language's
surface. The simplification natively supports arbitrary partial
patterns (e.g. `} else {` or `foo(`) at the tokenization layer —
the matcher's "wildcard binds to complete subtree" requirement is
the remaining gap for full partial-patch support (see §3.9).


### 6.5 Cursor performance

The cursor interface uses mutable in-place operations (per stsearch)
to avoid per-step allocation. In OCaml, implementable via mutable
record fields or a `ref`-wrapped state. Benchmarking should confirm
the allocation behavior matches expectations; if not, alternative
implementations exist.

### 6.6 Match acceptance under ERROR nodes

If the source has ERROR descendants within a match span, the
oracle's structural answers may be wrong. The matcher should either
flag such matches as "best effort" or refuse them in strict mode.
Decision deferred until empirical evidence on real corpora.

### 6.7 String-literal matching on Kotlin and Scala

§3.10 documents that the Kotlin and Scala grammars collapse or omit tokens in interpolated string literals. 

- **Kotlin**: The ambiguity (where a pattern leaf with text `hello` might match both an identifier and a string content) is **resolved** because the matcher compares both leaf text and leaf node type (`simple_identifier` vs `string_content`).
- **Scala**: The static content in Scala interpolated strings remains unmatched because the grammar does not construct AST child nodes for it.

Neither is a blocker for change-summary's pipeline or typical refactoring rules. If a real-world Scala use case demands distinguishing static parts of interpolated strings in the future, we can add a fallback check when matching an `interpolated_string_expression` node type to compare their raw text range (via `Tree.text`). This is deferred as it is not needed for the initial scope.

### 6.8 Character-grained pattern matching (considered, not adopted)

An alternative to tokenizing the pattern at all — even with the
tree-sitter parser-as-lexer trick in §6.4 — is to treat the pattern
purely as a text string with metavar markers identified, and run a
character-grained matcher against source bytes with the AST
providing extras-skipping and subtree-boundary information for hole
binding. No tokenization of the pattern at any layer.

**Motivation.** The §6.4 reliance on tree-sitter's parser-as-lexer
behavior is empirically validated but not formally guaranteed. A
character-grained matcher would remove this dependency entirely —
the pattern is exactly what the user wrote, byte for byte, and the
matcher's correctness for a new language would depend only on
tree-sitter parsing the *source* correctly, not on it tokenizing
*patterns* correctly.

**Sketch.** Pattern is split at metavar markers into a sequence of
[literal chunk : string | hole : name]. Matching walks source bytes
against the chunks: each non-whitespace pattern char requires an
exact source-byte match at the current alignment; each pattern
whitespace character (or run) matches some semantics involving
source extras (whitespace, comments) — see below. Hole binding
follows STMatch's structure: candidate hole-end positions are
constrained to AST subtree boundaries.

**The whitespace rule is user-visible.** The trade-off the
token-grained approach avoids is forcing the pattern author to
choose, and the matcher to enforce, a rule for what pattern
whitespace means. Three coherent rules exist:

- **A (strict pattern fidelity):** pattern non-whitespace = exact
  source byte at current position; pattern whitespace = 0+ source
  extras bytes. Predictable but strict — pattern `foo(` doesn't
  match source `foo (`.
- **B (boundary-aware):** pattern whitespace = leaf boundary with
  extras tolerance; pattern non-whitespace can implicitly cross a
  leaf boundary only when no extras are present. Pattern `foo(`
  matches source `foo(` (no extras between leaves) but not
  `foo (` (extras between).
- **C (permissive boundary-skipping):** pattern non-whitespace
  implicitly crosses leaf boundary regardless of extras — but
  needs character-class awareness to prevent pattern `ifx` from
  matching source `if x`. Reintroduces tokenization knowledge in
  spirit.

**A** is the most principled and the recommended choice if this
path is ever taken; **B** is the most natural-feeling; **C** has
hidden complications.

**Costs vs. the token-grained current path.**

- User-visible whitespace rule that doesn't exist today. Whichever
  of A/B/C, users would need to learn it. Currently they don't —
  tree-sitter's tokenization handles whitespace correctly under
  the hood.
- More complex matcher core. STMatch (§5 Phase 1, 146 LOC) ports
  cleanly from a tested Rust reference. Character-grained matching
  with AST-bounded holes is novel code: byte-by-byte loop with
  leaf-tracking, new cursor methods (`is_extras_at_byte`,
  `subtree_boundary_at_byte`).
- The (text, node_type) discrimination from §3.10 becomes a
  per-byte AST query rather than a per-token field. Doable but
  messier.
- Modest performance hit (byte-grained vs token-grained loop).

**Benefits.**

- No parser-as-lexer dependency. The §6.4 validation requirement
  for new languages drops to "source-side tree-sitter parsing
  works" only.
- Pattern is exactly what the user wrote, character for character.
  No hidden tokenization step the user has to reason about.

**Why not adopted.** The benefits are largely hypothetical (the
§6.4 risk is empirically near zero for current and likely-future
languages) and partly aesthetic (the "feels off" intuition is real
but not load-bearing). The costs are concrete and ongoing — a
user-visible whitespace rule, a more complex matcher, harder
type-discrimination, modest performance hit. For diffract's
workload (primarily change-summary, which bypasses pattern
parsing entirely; secondarily user-written rules across five
validated languages) the trade-off favors the token-grained path.

**When to revisit.** If §6.4's empirical guarantee breaks for a
newly-added language and no parser-as-lexer mitigation works, the
character-grained variant is a focused refactor enabled by the
existing `Cursor.S` abstraction. The cursor interface gains the
byte-level methods listed above; the matcher core is rewritten;
the pattern token list type goes away in favor of the text-with-
holes representation. Documented here so future work has a
starting point rather than rediscovering it cold.

## 7. Validation before commitment

### 7.1 Smoke test: tokenize representative patterns per language (Completed)

Completed early during the planning review. We ran a side-by-side comparison of raw pattern fragments vs. wrapped pattern fragments across all supported languages (TypeScript, TSX, Kotlin, PHP, Scala). The extracted leaf token sequences matched expectations perfectly and were 100% identical in every case, confirming that raw tokenization under error recovery is fully robust and wrapping is not required. Additionally, a dedicated spike was conducted for TSX to verify that syntactic ambiguities (JSX element tags, generic type instantiations, and binary comparisons) are correctly resolved at the raw fragment level with precise leaf-type typing.


### 7.2 Smoke test: STMatch on hand-built trees

Phase 1 deliverable. Confirms the algorithm port is correct
independently of tree-sitter. Tests are deliberately minimal — a
handful of trees, a handful of patterns, every combination
asserted.

### 7.3 Smoke test: end-to-end on one source file

Phase 2 deliverable. Confirms tree-sitter cursor integration.
Hand-construct one simple pattern, parse one source file, match,
inspect bindings.

If any of 7.1, 7.2, or 7.3 surface unfixable problems, the
prototype gets parked and the existing matcher continues with
whatever incremental fixes it needs.

## 8. Trade-offs

### 8.1 Pros

- Patterns are never structurally parsed; the fragment-parsing
  problem dissolves entirely.
- Source AST is the only structural truth; whitespace, brackets,
  operator-vs-delimiter ambiguities are all handled by tree-sitter's
  existing logic.
- Per-language code in the matcher: effectively zero. Extras-
  skipping is generic; separator extraction is generic; the
  ellipsis placeholder is derived from `Grammar_metadata`'s
  existing comment markers (no new per-language data, see §6.1).
- Matcher core is ~270 LOC ported from stsearch; substantially
  smaller than the current `Match_*` infrastructure.
- Type-aware leaf comparison `(text, node_type)` extends
  stsearch's text-only equality and disambiguates collisions like
  Kotlin's identifier `hello` vs string content `"hello"` without
  any per-language code (§3.10).
- Change-summary integrates directly via leaf walk; eliminates the
  rendering / re-parsing round-trip in the applicability check
  path.
- Algorithm is testable in complete isolation from tree-sitter
  (parametric cursor).
- Validated architecture (stsearch, PLDI 2024) — not unexplored
  territory.

### 8.2 Cons

- Substantial rewrite of the matcher subsystem. Weeks of work, not
  days. Migration plan keeps the tree green throughout, but the
  cost is real.
- Tree-sitter-as-parser-as-lexer for patterns: empirically
  validated on all supported languages in §7.1 (raw-vs-wrapped
  tokenization identical across TypeScript, TSX, Kotlin, PHP,
  Scala on a representative corpus, including a dedicated TSX
  spike for `<>` ambiguity). Not formally proven; adding a new
  language requires re-running the validation per §6.4.
- Source-side AST queries during matching are more grammar-coupled
  than text scanning. Grammar version bumps can change matcher
  behavior; need test corpus + grammar pinning discipline.
- Pattern semantics depend partly on source AST shape (operator
  precedence surprises). Documentation and test corpus needed; this
  is a real semantic shift from text-based matching.
- Loses early "is this pattern syntactically valid?" feedback for
  cases where tokenization produces unexpected tokens. Tokenization
  succeeds (tree-sitter always produces leaves) but matches may
  silently fail or match unexpected things.
- On Kotlin and Scala, the grammars collapse or omit some tokens
  in interpolated string literals (§3.10). Type-aware leaf
  comparison resolves the Kotlin ambiguity; the Scala
  static-content omission remains a real expressiveness gap until
  the deferred mitigation in §6.7 lands.

### 8.3 Alternatives considered

**Wrap-and-peel** (`docs/hybrid-matcher.md` §5.3): parse pattern in
synthetic wrapping context per language and shape, peel back to AST,
match AST-on-AST. Smaller architectural change but requires more
per-language wrapping templates and reintroduces AST-shape
commitment per pattern. Rejected: per-language hardcoding is
exactly what we want to avoid, and shape-commitment brings back
`try_match_children_directly`-style workarounds in a new guise.

**stsearch's approach unchanged** (search-only, anonymous
wildcards): doesn't fit diffract's transform / replacement /
named-binding requirements. We extend stsearch with named bindings,
match modes, and the existing transform machinery — but the core
algorithm is unchanged.

The DEL-based hybrid (currently on the `hybrid-matcher` branch) is
not listed as a live alternative — it was the previous attempt,
abandoned for the reasons in §1, and its main modules are removed in
Phase 0 (while retaining the `Grammar_metadata` accessors for reuse).

## 9. What this note does not commit to

This is a design for an investigation, not a commitment to ship.
The first phases (1-3) are small enough to be a discardable
prototype if the approach proves unworkable in practice.

Outcomes that would justify pivoting:

- A new supported language's §7.1-style validation surfaces
  unfixable tokenization issues. (TypeScript, TSX, Kotlin, PHP,
  Scala have already passed; the risk is for future additions.)
- Match modes (partial, field) turn out to require AST-shape
  inspection on the pattern side after all, defeating the
  asymmetry's advantage.
- Performance characteristics on the change-summary corpus are
  unworkable in practice.

If any of those surface, the prototype gets parked as a reference
artifact and the existing `Match_*` matcher (post Phase 0)
continues to ship while we reconsider. Reconsidering would mean
looking at wrap-and-peel (`docs/hybrid-matcher.md` §5.3) as the
next-best option — not reverting to the DEL hybrid, whose main modules
Phase 0 has already removed.

## 10. References

- Matute, Ni, Barik, Cheung, Chasins. *Syntactic Code Search with
  Sequence-to-Tree Matching: Supporting Syntactic Search with
  Incomplete Code Fragments.* PLDI 2024. Article 230.
- `plait-lab/stsearch` — open-source Rust implementation (MIT).
  Reference for the porting work. The key files are
  `src/stmatch.rs` (matcher core, 88 LOC), `src/lib.rs` (outer
  loop, 101 LOC), and `src/code/mod.rs` (tree-sitter integration,
  77 LOC).
- `docs/hybrid-matcher.md` — the prior design this document
  supersedes. Section 5.3 ("Alternatives considered") and §8
  ("Known scanner limitations") are particularly relevant context.
