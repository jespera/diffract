# Surgical transforms: design note

Status: **implemented for strict, partial, and field modes.** This note argues
for changing the transform model from *whole-span replacement* to *surgical*
(Coccinelle-style) edits, sketches how, and (§8) records what was built. The
§1–§7 discussion is the original design argument; read §8 for the as-built
behaviour.

## 1. The problem

diffract's transforms today use **whole-span replacement**: the engine finds
the span a pattern matched and replaces that entire span with the instantiated
`+`/context template. That is simple to implement (match a span, substitute,
overwrite) but it diverges from the Coccinelle/spatch model the tool is
otherwise modelled on, and three separate user-facing surprises all trace back
to it:

1. **Partial `-` drops siblings.** A pattern that marks one element for removal
   while the container is context —
   ```
     <Foo
   -   bar={$x}
     />
   ```
   matches a `<Foo …>` with other attributes (partial mode tolerates them), but
   the replacement is rebuilt from the pattern's context (`<Foo />`), so the
   other attributes — present in the source, absent from the pattern — vanish.
2. **`...` is inserted literally and the rest is dropped.** The strict variant
   ```
     <Foo
       ...
   -   bar={$x}
       ...
     />
   ```
   matches (the `...` capture surrounding attributes), but `...` is a
   *match-side-only* construct; on the replace side it is literal text, so the
   output contains literal `...` lines and the captured attributes are gone.
3. **Field `-` drops the fields field mode ignores.** A field-mode rewrite of a
   declaration drops the decorators/return types it ignored for matching,
   because the whole declaration span is rebuilt from the pattern.

These are not three bugs; they are one model showing its limits. From a
Coccinelle background all three are backwards: `-` should remove *only what it
marks*, context and `...` should be *preserved*, and you write the parts you
care about while everything else stays put.

## 2. The key principle: scope follows the `-`/`+` marking

The unifying insight (and the reason whole-span isn't simply "wrong"): **the
granularity of the `-`/`+` marking is the scope of the edit.**

- `- { color: $v }` / `+ { colour: $v }` — the *whole construct* is on the
  marked lines. Replacing the whole object, dropping anything not restated, is
  the **expected** result, because you marked the whole thing. Whole-span
  behaviour is correct here.
- `<Foo` … `-   bar={$x}` … `/>` — only an *inner element* is marked; the
  container delimiters are context. Only that element should change; the
  container and its other attributes should stay.

So whole-span replacement is not a separate model — it is the **special case of
surgical editing where the entire matched construct is marked `-`/`+`.** Today's
engine collapses every transform to that special case regardless of where the
markers actually are. The fix is to honour the marking granularity.

## 3. Proposed model: surgical edits

A transform computes edits **at the span granularity of the `-`/`+` regions**,
and leaves everything else in the source byte-for-byte:

- **Context lines** (unprefixed) must match, and their source bytes are
  **untouched**.
- **`-` regions** delete their matched source span (with separator cleanup, as
  element removal already does). A `-` region with a corresponding `+` is
  *replaced* by the instantiated `+` text rather than just deleted.
- **`+`-only regions** insert text at their position relative to the
  surrounding context.
- **Metavars** inside a `-` region: their bound source span is part of what is
  deleted/replaced. Inside a `+` region: substituted from the binding as today.
- **Unmatched source** — partial mode's tolerated extra elements, field mode's
  ignored optional fields, and the content captured by `...` — is covered by no
  edit, so it is **preserved verbatim**, formatting included.

Consequences:

- All three surprises in §1 disappear at the root. Sibling attributes survive;
  `...` needs no "rendering" because it is matched-and-not-edited; ignored
  declaration fields stay.
- **Formatting fidelity** improves: untouched regions keep their exact source
  text (the indentation rewrite users saw under whole-span goes away), because
  we splice small edits into the original source instead of rebuilding the span
  from the pattern.
- Whole-construct rewrites (`- foo($x)` / `+ bar($x)`, or `- { … } / + { … }`)
  behave exactly as now — there, the whole match *is* the marked region.
- `foreach` element removal is revealed as a special case of this model (delete
  one element's span + clean its separator), so the model is a generalization
  of something already in the engine, not a parallel mechanism.

## 4. What changes for existing patterns

- **No change**: whole-construct renames/replacements; any pattern where the
  marked lines cover the entire matched span.
- **Behaviour change (the fix)**: partial/field patterns that mark a *sub*-part
  now preserve the rest instead of dropping it. This is the intended outcome,
  but it *is* a semantic change — worth a note in the changelog.
- **The partial/field whole-span warning** (`pattern_warnings`) becomes mostly
  unnecessary: a partial/field `-` on a sub-part is no longer a footgun. The
  warning should be re-scoped to only the genuinely-destructive case (a `-`/`+`
  that marks the whole container) or removed.
- **`...` in transforms** starts working as expected.
- The guidance in `migration-coverage.md §1` ("match exactly what you want to
  change") is *relaxed*: you mark what changes, context preserves the rest —
  which is the more ergonomic Coccinelle rule.

## 4a. IR impact

The IR's **matching/composition structure is unaffected**; only its
**transform representation** changes.

Unchanged: `StrictSeq` / `PartialContainer` / `FieldContainer` (mode + match
tokens), `Within` (`on`-scoping), `All` (multi-section composition). These say
*where and how to match* — surgical editing doesn't touch any of it.

Changes: the per-leaf transform payload. Today a leaf carries `tokens` (match
side, with context and `-` lines already **flattened together**, no roles) and
`replace : replace_template option` where `replace_template = { text; singles;
sequences }` — a **single whole-span replacement string**. Both bake in
whole-span replacement:

- **Match tokens need a per-token role** (context vs removed). Edit-building
  must know which Phase-1 spans belong to `-` regions (delete) vs context
  (keep); `match_side` currently concatenates them and loses the role.
- **`replace_template` is the wrong shape.** A flattened "context + `+`" string
  forces rebuilding the whole span and can't say "delete here, insert there,
  keep the rest." It must be replaced by a structure that keeps the
  `-`/`+`/context layout distinct, including **where** each `+` anchors relative
  to the matched tokens.

Recommended shape: the leaf carries an ordered segment list —
`Context tok | Removed tok | Added text` — from which the match tokens
(`Context` + `Removed`) are derived for matching and the localized edits are
derived for transforming. This replaces `tokens` + `replace_template`.
`edits_of_composite` then walks the segments + Phase-1 spans: delete `Removed`
runs (with separator cleanup), insert `Added` at its anchor, leave `Context`
untouched. Whole-construct rewrites are the special case where every token is
`Removed`/`Added`, so the localized edit covers the whole span — i.e. today's
behaviour, unchanged. `foreach` (`elem_tokens` + `elem_replace`, already doing
surgical removal via `removal_span`) should become another consumer of the same
segment/span edit-builder rather than a parallel path.

The change is contained: the transform-carrying fields of the leaf
constructors, the body classifier (`match_side`/`replace_side` →  a segment
parser), and `edits_of_composite`/`foreach_edits`. The matching engine and
`Within`/`All` evaluation do not move.

## 5. Implementation sketch

The central new requirement: the matcher must expose, for the matched region,
the **source byte span of each `-`/context region** (not just the overall match
span and metavar bindings, which is all it surfaces today).

Rough shape:

1. **Tag tokens by role.** Tokenization currently folds context+`-` into one
   `match_text`. Instead, carry a per-token role (`context` vs `removed`) into
   the `pattern_token` stream so the engine knows which matched leaves belong
   to a `-` region. (`+` regions don't participate in matching; they are
   positioned relative to the surrounding context/`-` structure.)
2. **Record consumed spans.** During matching, record the source byte range
   each pattern token consumed (the engine already navigates leaves via
   `move_first_leaf`/`byte_range`; this surfaces what it already visits).
3. **Group into edit regions.** A maximal run of `removed` tokens maps to a
   source span `[first.start, last.end)`. With a paired `+`, that span is
   replaced by the instantiated `+`; without, it is deleted (+ separator
   cleanup, reusing the `removal_span` logic). `+`-only regions insert at the
   boundary between their neighbouring context spans.
4. **Splice.** Feed the resulting edits into the existing bottom-up
   `apply_edits` (overlap handling already present).

Most of the work is steps 1–2 (threading roles and spans through
tokenize → stmatch → matcher); steps 3–4 reuse the element-removal machinery.

## 6. Open questions

- **Span tracking cost/complexity.** Recording per-token spans through the
  backtracking engine needs care (a token's span is only final once its match
  path commits). Scope this before committing to the approach.
- **Whitespace at edit boundaries.** Surgical edits should be cleaner than
  whole-span (context whitespace is preserved), but a deleted middle element
  still leaves the separator/space question (the cosmetic residue element
  removal already has). Decide how aggressively to absorb adjacent whitespace.
- **`+`-only insertion position.** Where exactly a `+`-only line lands relative
  to context needs a precise rule (before/after the adjacent context token).
- **Multi-section / `foreach` consistency.** `foreach` is already surgical
  per-element; ensure the generalized model and `foreach` agree (ideally
  `foreach` becomes one more caller of the same edit-building code).
- **Intentional whole-drop.** A user who genuinely wants "replace this object,
  dropping its extras" must now mark the whole object (which is the honest way
  to express it). Confirm nothing relied on the implicit drop.

## 7. Recommendation

Adopt the surgical model. It fixes three real surprises at the root, aligns the
tool with its Coccinelle inspiration, improves formatting fidelity, and
generalizes the element-removal code already present. The main cost is
threading token roles and source spans through the engine; the edit-building
and splicing reuse what exists. Build behind the existing test suite, adding
cases for: partial sub-element removal preserving siblings, strict `...`
preservation, field sub-edit preserving ignored fields, and confirmation that
whole-construct rewrites are unchanged.

## 8. As built

**Strict, partial, and field modes are all surgical.** A `-`/`+` on a sub-part
edits only that part; context, partial's tolerated extras, field's ignored
optional fields, and `...`-captured source all survive byte-for-byte.

The pieces, bottom-up:

- **Per-token spans** (`Stmatch`). `match_result` gains a `spans` array — per
  pattern-token source byte range. Strict records it in `drive`
  (`match_at_spans`); `find_matches` populates it on the non-overlapping
  (transform) path by re-driving the committed leftmost match. Partial and
  field thread a global `spans` array (with a running offset) through
  `match_set_at` / `match_field_at`: each element's `match_prefix` records its
  tokens' spans, blitted into the global array at the element's offset, and
  only on the committed path so backtracking trials don't pollute it. Search
  (`overlapping`) leaves `spans = [||]`.

- **Hunks** (`Matcher`). A transform body compiles to a list of `hunk`s
  (`compile_hunks`): a maximal run of `-`/`+` lines between context lines. Each
  hunk records the token indices of its `-` lines (`del_idxs`, parallel to
  `spans`), the instantiated `+` text (`add`), and the bracketing context
  tokens (`prev_tok`/`next_tok`) for anchoring a `+`-only insertion. The leaf
  carries `hunks` instead of a per-token role list — the role structure lives
  in the hunk boundaries.

- **Surgical edits** (`surgical_edits`). When a match has `spans`, each hunk
  becomes one localized edit: delete/replace `[min start, max end)` over its
  `-` tokens' spans, insert a `+`-only hunk at the preceding context token's
  end. Everything else is covered by no edit and survives.

- **Whole-span fallback.** When a match has no `spans` (search only, never the
  transform path now), or as the natural degenerate case of a whole-construct
  rewrite (one hunk, every token `-`, so the edit spans the whole match),
  behaviour equals the former whole-span model. The existing transform suite is
  the behaviour-preservation check; only one case changed (`removal within
  context` now keeps the surrounding function instead of rebuilding it).

**Whitespace at edit boundaries.** Deletion cleanup depends on context.
*Strict* deletions are line-oriented (`line_cleanup`): a whole-line deletion
absorbs its indentation and trailing newline. *Partial/field* deletions are
element-oriented (`element_cleanup`): they absorb an adjacent list separator
(`,`/`;`) so it doesn't dangle (`{ , size }`), or close a whitespace gap, then
fall back to line cleanup. The split matters because in strict code a `;` is a
statement terminator (handled by line cleanup), not a list separator. A
*replacement* is in-place: the `-` span is the bare token run, so the `+`
content should omit structural indentation — the source's indentation stays put
and prefixes the new text.

**What this fixes.** All three §1 surprises are gone:

- §1.2 — strict `...` no longer emits literal `...`; only the `-` line is
  touched, captured siblings preserved.
- §1.1 — a partial `-` on one element removes just it (with its separator),
  keeping the tolerated extras.
- §1.3 — a field `-`/`+` rewrites the part it marks and keeps the
  decorators/annotations/modifiers/return types it ignored for matching.

```
 <Foo
-   bar={$x}
 />
```

in partial mode removes only `bar={$x}` from a `<Foo bar={x} baz={y} />`,
keeping `baz`.

**The warning, re-scoped.** `pattern_warnings` no longer fires for a
partial/field sub-part edit (it is surgical now). It fires only when a
partial/field section marks the **whole container** — a body with no context
line, so every matched token is removed and the edit spans the whole match,
dropping the tolerated extras / ignored fields. That is the honest reading of
"replace the whole thing", but an easy mistake in modes built to tolerate/
ignore content, so it warns rather than errors.
