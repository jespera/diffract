# Migration coverage (historical record)

**The old AST matcher has been retired** — this note is kept as the record of
the migration that justified removing it. It documented what the
universal-tokenizer matcher (`Matcher`) covered relative to the old matcher's
test corpus (`tests/test_match.ml`, ~177 tests in 13 groups) and what retiring
the old matcher would lose, from a parity audit plus re-authoring the old
expansion patterns in the new surface.

Outcome: all three capability gaps (concrete-key matching, element deletion,
field mode) were implemented — none needed the per-language coupling first
feared — and a verification sweep across the supported languages turned up and
fixed several real bugs (trailing-newline leak, partial wildcard-key, phantom
missing-node, metavar-substring substitution). The intentional non-ported
differences (sigil-free PHP/Kotlin metavars, whole-span partial/field
replacement, stricter operators, `join`/`foreach` replacing the `~`/`,`
expansion lines) are recorded in §5–§6 below. The old matcher and its tests,
examples, and benchmarks are now gone.

## 1. The transform model: whole-span replacement

The most important thing to understand, because it reframes what looks
like the biggest gap: **the new matcher replaces exactly the span its
pattern matched.** A pattern's `-`/`+`/context lines decide what text
forms the match side vs the replace side; they do **not** localize the
edit to the marked lines the way Coccinelle/spatch do. The whole matched
span is replaced by the whole instantiated replace side.

The usage rule that follows: **match exactly what you want to change, not
the surrounding container.**

- To rename a call: match the call. `- foo($x) / + bar($x)`.
- To edit a property *in place* preserving its siblings: match the bare
  property, not the object. `- $K: $V / + $K: wrap($V)` against
  `{ color: "red", size: 10 }` yields `{ color: wrap("red"), size:
  wrap(10) }` — each property edited in place, the object and separators
  untouched.
- To replace a whole object: match the whole object. `- { color: $V } /
  + { colour: $V }` replaces the entire object (dropping any unmentioned
  properties) — because you marked the whole object.

This last case is where the old matcher differed: in `match: partial` it
*preserved* unmentioned properties even though the whole object was on the
`-`/`+` lines — a surgical edit silently inferred from an object-level
pattern. That convenience was also a surprise (the markers said "replace
the object," the behaviour was "edit one property"). The new model is
honest: marking the object replaces the object; to be surgical, match the
property. So this is **not a capability gap** — it is a different, more
predictable model with a usage rule.

For surgical edits that also need a guard ("only in objects shaped like
X"), multi-section covers it: one section matches/guards the container,
a `foreach`/`on`-scoped section edits the elements in place.

## 2. Concrete keys out of context — resolved

Selective in-place edits — "rename the `color` property specifically,
preserve the rest" — were the audit's one real capability gap: a concrete
key like `color`, tokenized on its own, parses as a labeled statement
(`statement_identifier`), not the source pair's `property_identifier`, and
the `(text, node_type)` comparison missed.

This is **now fixed** for the case that matters — a `foreach` element
pattern. `compile_foreach` tokenizes the element pattern *in its container
context*, splicing it into the binding section's scaffold (`foo({ color:
$V })`) so the parser assigns the contextual node-type, then extracting the
element's leaves by byte span. No per-language data enters the code (the
context is the user's own outer pattern) and no precision is relaxed. See
[transforms.md §10](transforms.md) for the mechanism, and the `foreach:
concrete key …` tests in `test_matcher.ml`.

What this leaves:

- **Wildcard keys** (`$K: $V`) — always worked (the wildcard ignores node
  type).
- **Concrete keys in `foreach` elements** — now work (TS object property,
  TSX attribute), preserving siblings. Kotlin/Scala/PHP were never affected
  (single identifier node-type).
- **Bare concrete-key patterns** (a standalone `- color: $V`) — *not*
  addressed, and correctly so: bare `color: $V` is a labeled statement and
  `a={$x}` is an assignment, not the in-container element. The bare form
  honestly means its standalone parse; element-shaped patterns belong in
  container context, where `foreach` now handles them.
- **Ellipsis in the element** — falls back to standalone tokenization
  (offsets would shift), so such a selective rule degrades gracefully
  rather than misfiring.

So the audit's highest-value gap is closed, with no architectural cost.

## 3. Element deletion with separator cleanup — resolved

Removing a list element (deprecated property, unused argument) is **now
implemented**. A `foreach` element with an empty replacement (a `-` line,
no `+`) deletes the element, and the engine swallows one adjacent separator
so the list stays well-formed — the separator *following* the element, or
the *preceding* one if it is last with no trailing comma. A trailing comma
is handled by the same rule (`f(a,)` → `f()`, no dangling). See
[transforms.md §11](transforms.md) for the mechanism, and the `remove: …`
tests in `test_matcher.ml`. This covers the old `separator_deletion` group
(11 tests).

## 4. Field mode — resolved

Field mode (`match: field`) — match a declaration while ignoring
decorators/modifiers/return types — is **now implemented**, and without
the per-language registry the design originally anticipated. A pattern's
leaf stream is aligned to a subsequence of the declaration node's
children: optional fields the pattern omits are skipped, addressed fields
are matched in full, with backtracking (so `@deprecated` can be matched
among several annotations). The once-planned "Tier 3" gap (TS
`async`/`abstract`) dissolved — anonymous keywords the pattern omits are
just skipped, and a body-bearing pattern still won't match a bodyless
signature. See [field-mode.md](field-mode.md) and the `field: …` tests in
`test_matcher.ml`. This covers the old `field` group (6 tests).

Two follow-ups remain, both niche: decorator-subset where the grammar
keeps annotations in a single child (PHP `attribute_list`, Kotlin
`modifiers`) is a nested match, and TS method decorators are class-body
siblings of the declaration (so a field-mode replace leaves them in
place). Neither blocks the primary use case.

## 5. On par or better (no gap)

- **Search and whole-construct transforms**: calls, member calls,
  fragments, ellipsis, partial-search, comments, rename, operand swap,
  multi-section, `on`/`foreach` scoping. Confirmed by the parity harness.
- **Operator precision is better**: the old matcher matched `m * n` with a
  `$a + $b` pattern (ignoring the operator); the new matcher correctly
  matches only `+` expressions.
- **Sequence expansion**: the old column-0 expansion (`~`/`,` prefixes) is
  fully covered by the new `foreach`/`join` surface — verified by
  re-authoring the old `expansion` group's patterns as passing tests
  across TS and Kotlin (the `port:` tests in `test_matcher.ml`).

## 6. Not gaps — convention artifacts

- **PHP / Kotlin metavars**: the old matcher requires `$`-sigil metavars
  and unwraps the `<?php` tag; the new matcher is sigil-free (`$MSG`
  tokenizes to `$` + `MSG` in PHP/Kotlin, where `$` is real syntax) and
  treats tags literally. Old PHP/Kotlin tests can't run verbatim, but the
  capability is intact — the Kotlin expansion ports (sigil-free metavars)
  pass. These tests need re-authoring in new conventions, not new code.

## 7. Two minor expansion edge behaviors

- **Merging two captured sequences into one join**: old `, $BEFORE $AFTER`
  gathered both sequences into a single comma-join; the new model renders
  each referenced sequence independently (its own `join`). Rare.
- **Passthrough vs drop of non-matching elements**: an old
  transform-expansion passed through elements that didn't match the inner
  pattern; a new splice renders only the matched elements and drops the
  rest. Relevant to "transform some elements, keep the others in the
  list"; ties into the open keep/drop question for filtering.

## 8. The decision this frames

All three of the audit's gaps are **now closed**:

| Gap | Footprint | Cost | Note |
| --- | --- | --- | --- |
| ~~Concrete-key matching (§2)~~ | common surgical edits | — | **done** (contextual tokenization) |
| ~~Deletion + separator cleanup (§3)~~ | remove-property/arg | — | **done** (foreach empty `+`) |
| ~~Field mode (§4)~~ | decorated declarations | — | **done** (child alignment; no registry) |

Each closed without the per-language coupling first feared — concrete keys
via contextual tokenization, deletion via the foreach empty-replacement,
field mode via child alignment. Retiring the old matcher (the endgame
"delete old matcher" step) is no longer gated on a capability gap; what
remains is verification breadth and the mechanical work of porting the old
corpus's remaining tests and removing the old engine. The niche field-mode
follow-ups (§4) and the deferred partial-mode sugar
([partial-mode.md §7](partial-mode.md)) are evidence-driven, to be built if
the change-summary work asks for them.
