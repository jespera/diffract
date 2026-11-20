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
replacement, stricter operators, `join` replacing the `~`/`,`
expansion lines) are recorded in §5–§6 below. The old matcher and its tests,
examples, and benchmarks are now gone.

## 1. The transform model: surgical edits

Transforms are **surgical** (Coccinelle-style) in all three modes: an edit is
localized to the `-`/`+` *lines*, and everything else is preserved
byte-for-byte — context, partial's tolerated extras, field's ignored optional
fields, and the source captured by `...` (see `docs/surgical-transforms.md §8`
for the mechanism).

What this means in practice:

- Rename a call: `- foo($x) / + bar($x)`. The whole match is marked, so it is
  replaced whole — the degenerate single-hunk case.
- Remove or edit a *sub-part* while keeping its siblings — mark only that part:
  ```
   <Foo
  -   bar={$x}
   />
  ```
  in partial mode removes only `bar={$x}` and keeps the other attributes. In
  strict mode, bracket the rest with `...` (matched-and-not-edited, never
  emitted literally); in field mode the omitted decorators/modifiers/return
  types stay put.
- Marking the **whole container** (a body with no context line) still replaces
  it whole, dropping anything not restated. In partial/field that drops the
  tolerated extras / ignored fields, so the matcher warns (`pattern_warnings`)
  — it is the honest reading of marking the whole thing, but an easy mistake in
  modes built to tolerate/ignore content.

For a sub-part edit that also needs a guard ("only in objects shaped like X"),
multi-section still works: one section matches/guards the container, a
`on`-scoped section edits the elements.

## 2. Concrete keys out of context — resolved

Selective in-place edits — "rename the `color` property specifically,
preserve the rest" — were the audit's one real capability gap: a concrete
key like `color`, tokenized on its own, parses as a labeled statement
(`statement_identifier`), not the source pair's `property_identifier`, and
the `(text, node_type)` comparison missed.

This is **now fixed**, and not by anything specific to the case: strict
and partial matching compare leaves *lexically* (text, plus whether the
leaf sits inside a string literal) and tolerate a node-type disagreement,
because a pattern body is a fragment whose re-parse assigns roles
unreliably. A bare `color: $V` therefore matches the property, whether
written between context `...` lines or in a section scoped with
`on $PROPS`. See [transforms.md §10](transforms.md), and the
`on sequence: concrete key …` tests in `test_matcher.ml`.

(An earlier fix re-tokenized a `foreach` element pattern inside its
container; it went away with `foreach`.)

So the audit's highest-value gap is closed, with no architectural cost.

## 3. Element deletion with separator cleanup — resolved

Removing a list element (deprecated property, unused argument) is **now
implemented**. A `-` line between context `...` lines deletes the element,
and the engine swallows one adjacent separator so the list stays
well-formed — the separator *following* the element, or the *preceding* one
if it is last with no trailing comma. A trailing comma is handled by the
same rule (`f(a,)` → `f()`, no dangling). See
[transforms.md §11](transforms.md) for the mechanism, and the
`strict remove: …` tests in `test_matcher.ml`. This covers the old
`separator_deletion` group (11 tests).

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
  multi-section, `on` scoping. Confirmed by the parity harness.
- **Operator precision is better**: the old matcher matched `m * n` with a
  `$a + $b` pattern (ignoring the operator); the new matcher correctly
  matches only `+` expressions.
- **Verbatim sequence expansion**: the old column-0 expansion (`~`/`,`
  prefixes) is covered by the `join` directive — verified by re-authoring
  the old `expansion` group's verbatim patterns as passing tests across TS
  and Kotlin (the `port:` tests in `test_matcher.ml`). The old
  *transform* expansion (each element rewritten, then rendered into the
  replacement) is not carried over; see §7.

## 6. Not gaps — convention artifacts

- **PHP / Kotlin metavars**: the old matcher requires `$`-sigil metavars
  and unwraps the `<?php` tag; the new matcher is sigil-free (`$MSG`
  tokenizes to `$` + `MSG` in PHP/Kotlin, where `$` is real syntax) and
  treats tags literally. Old PHP/Kotlin tests can't run verbatim, but the
  capability is intact — the Kotlin expansion ports (sigil-free metavars)
  pass. These tests need re-authoring in new conventions, not new code.

## 7. Expansion differences

- **Merging two captured sequences into one join**: old `, $BEFORE $AFTER`
  gathered both sequences into a single comma-join; the new model renders
  each referenced sequence independently (its own `join`). Rare.
- **Transform expansion is not ported**: the old matcher could rewrite each
  element of a captured sequence and render the results into the
  replacement (`execute(a, b)` → `Pipeline().addStep(a).addStep(b).run()`).
  The new matcher offered this for a time via a `foreach` directive and
  dropped it ([transforms.md §7](transforms.md)): elements are rewritten in
  place with `on`, and a sequence referenced in a `+` template is
  substituted unchanged.

## 8. The decision this frames

All three of the audit's gaps are **now closed**:

| Gap | Footprint | Cost | Note |
| --- | --- | --- | --- |
| ~~Concrete-key matching (§2)~~ | common surgical edits | — | **done** (lexical leaf comparison) |
| ~~Deletion + separator cleanup (§3)~~ | remove-property/arg | — | **done** (`-` line beside `...`) |
| ~~Field mode (§4)~~ | decorated declarations | — | **done** (child alignment; no registry) |

Each closed without the per-language coupling first feared — concrete keys
via lexical leaf comparison, deletion via the `...` run beside the `-` line,
field mode via child alignment. Retiring the old matcher (the endgame
"delete old matcher" step) is no longer gated on a capability gap; what
remains is verification breadth and the mechanical work of porting the old
corpus's remaining tests and removing the old engine. The niche field-mode
follow-ups (§4) and the deferred partial-mode sugar
([partial-mode.md §7](partial-mode.md)) are evidence-driven, to be built if
the change-summary work asks for them.
