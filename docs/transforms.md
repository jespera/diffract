# Transforms: design note

Transforms are the replacement (`+`) side of a pattern: once a pattern
matches, the bound metavars are substituted into a replacement template
and the matched span is rewritten. This note records the design of
transforms on the universal-tokenizer matcher — in particular how
sequence metavars are rendered into a replacement, which is where the
interesting decisions are.

Status: **design settled, implementation staged**. Single-binding
transforms, multi-section transforms, in-place per-element transforms
(map), and sequence splicing with the two knobs below are planned on the
IR (`StrictSeq`/`PartialContainer`/`Within`/`All` + `eval_ir`). The
general accumulator fold (§7) is deferred.

## 1. The use case

A pattern's `-` lines describe what to match; its `+` lines describe what
replaces it. Bindings carry across:

```
@@
match: strict
metavar $x: single
@@
- foo($x)
+ bar($x)
```

`foo(1)` → `bar(1)`. The match side binds `$x`; the replace side
substitutes the bound subtree's source bytes. For single metavars this is
straightforward textual substitution.

The design questions all concern **sequence metavars** — a metavar that
binds a variable number of elements (object properties, call arguments,
JSX attributes). Rendering a sequence into a replacement is not one
obvious operation, and the rest of this note is about pinning it down.

## 2. Formatting is not diffract's job

A decision that simplifies everything downstream: **diffract produces
semantically-correct output and leaves formatting to a formatter.** A
codemod is conventionally paired with a formatter (prettier, ktlint, ...)
in the same pipeline or CI; nobody hand-formats codemod output. So
diffract does not produce pretty output — it produces output that parses
to the intended tree, and a formatter prettifies it.

The corollary: diffract never needs to *invent cosmetic layout* (newlines,
indentation) for a new structure. That is the formatter's job. What
diffract must still get right is **semantic** punctuation — a `,` between
new call arguments, a `&&` between new operands — because a formatter will
not insert those and the output would not parse without them. The
distinction between cosmetic layout (punted) and semantic glue (diffract's
responsibility) runs through the whole design below.

## 3. Map vs splice: what span is replaced

Transforms over a sequence come in two shapes, distinguished by **what
gets replaced**:

**Map** replaces *elements*. Renaming every property, retyping each,
filtering: the edits are local to each element, applied where it sits. The
container and its separators are untouched — renaming `x` to `y` in
`{x: 1, z: 2}` never moves the comma, because nothing is gathered and
re-joined. There is no "join" in a map; each element is edited in place.

**Splice** replaces the *whole enclosing construct*. Rewriting
`matchExhaustive($TAG, {...})` to `match($TAG).with(...).exhaustive()`:
the entire call is gone, and the sequence's elements are *consumed as an
ingredient* in building a new structure. The elements are gathered,
rendered, and placed into the new template.

The two are told apart by the replacement itself (§5): if a `+` template
restructures and references the sequence metavar, it is a splice; if the
container is preserved and only elements are edited, it is a map.

## 4. Splicing a sequence: two orthogonal knobs

When a sequence is spliced into a replacement, rendering it has exactly
two independent dials, both declared in the preamble, both with sensible
defaults:

- **`foreach $VAR`** — the per-element *transform*. A section whose `-`
  matches one element and whose `+` is that element's rendered output.
  Default: identity (elements rendered unchanged).
- **`join $VAR by "<sep>"`** — the *glue* placed between rendered
  elements (intercalation: n−1 separators for n elements). Default:
  empty.

The base operation is: **intercalate the `foreach`-transformed elements
with the `join` separator.** Everything else is a choice of the two
defaults. The cases:

| Use case | `foreach` | `join` |
| --- | --- | --- |
| `[a, b, c]` → `a && b && c` | identity | `" && "` |
| object → `match(t).with(...).exhaustive()` | `key: val` → `.with(...)` | empty |
| same, dot as glue | `key: val` → `with(...)` | `"."` |
| rename each property (map, §3) | `key: val` → `key: wrap(val)` | n/a (in place) |

Both knobs live in the preamble — the structured directive zone — so the
replacement *body* stays pure: literal text plus metavar placeholders,
with no embedded meta-syntax. This is the same boundary the match side
holds (real source tokens, no sigil ceremony); neither body carries
directives.

### Why semantic glue cannot be avoided, and why the default is empty

The `join` separator is not always recoverable or punt-able:

- A **method chain** can use the empty default. Each fragment carries its
  own leading `.` (`foreach` emits `.with(...)`), and the chain attaches
  to a receiver base (`match($TAG)`). Prefix glue works because there is
  always something to its left, so no separator between fragments is
  needed.
- A **conjunction** `a && b && c` cannot. There is no base — `a` is itself
  the first operand — so `&&` can only go *between* elements (infix). A
  prefix `&& a` would dangle at the front. The `&&` is semantic (a
  formatter will not add it) and new (not present in the source array, so
  not inferable). It must be specified: `join $ELEMS by " && "`.

So the empty default covers the prefix-gluable cases (method chains) for
free, and `join` is the opt-in for infix semantic glue. The same dot in
the method chain can be expressed either way — baked into each fragment
(`foreach` emits `.with`, no `join`) or as the separator (`foreach` emits
`with`, `join … by "."`); both produce identical output. It is a style
choice about where the boundary glue sits.

## 5. `on` and `foreach`; map vs splice in code

A section can be scoped to a metavar bound by an earlier section. The
directive splits on whether the target is a single subtree or a sequence:

- **`on $VAR`** — `$VAR` is **single**. Scope this section to the subtree
  bound by `$VAR`; run once.
- **`foreach $VAR`** — `$VAR` is **sequence**. Run this section once per
  element.

The kind and the directive must agree: `on` over a sequence is a parse
error (a sequence is many subtrees, so "the subtree bound by it" is
ill-defined), and `foreach` over a single is a parse error (iterating one
element is a degenerate no-op). The two are not redundant — they
de-overload a single scoping concept whose behavior would otherwise depend
silently on the target's declared kind. With two names the behavior is
legible at the use site: `foreach` announces a loop body, `on` announces
single-scope.

Whether a `foreach`'s outputs are spliced or applied in place follows from
the replacement, not a separate flag:

**Map (in place)** — the outer section preserves the container and does not
reference the sequence in a restructuring `+`; the per-element edits land
where the elements are:

```
@@
match: strict
metavar $PROPS: sequence
@@
{ $PROPS }
@@
foreach $PROPS
match: field
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ $KEY: wrap($VAL)
```

`{a: 1, b: 2}` → `{a: wrap(1), b: wrap(2)}`. Commas never move.

**Splice** — a restructuring `+` references the sequence metavar; the
rendered elements (§4) replace the whole construct at that reference:

```
@@
match: strict
metavar $TAG: single
metavar $PROPS: sequence
@@
- matchExhaustive($TAG, {
-   $PROPS
- });
+ match($TAG)$PROPS.exhaustive();
@@
foreach $PROPS
match: field
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ .with("$KEY", $VAL)
```

`matchExhaustive(tag, {one: "1", two: "2"})` →
`match(tag).with("one","1").with("two","2").exhaustive()`. Empty `join`
default; the dot lives in each fragment.

A sequence metavar in a replacement always denotes the *rendered
elements*; you either splice them into something new or, by not
restructuring, leave them where the originals were.

## 6. Multiple references and multiple sequences

A sequence metavar in a `+` denotes a **value** (the rendered, intercalated
elements), not an iteration point — the `+` line never loops; iteration is
confined to `foreach`. So multiple references compose as plain
substitutions:

- The **same** sequence referenced twice → the same rendered value
  substituted at both sites.
- **Two different** sequences in one `+` → each rendered independently
  (its own `foreach`/`join`) and spliced at its own site.

Because the line is a template with value-holes rather than a loop body,
there is no "iterate over which?" ambiguity and no cross-product. Two
different sequences are rendered **independently, not zipped**; parallel
iteration (a zip) is a separate capability this design does not include.

## 7. The deferred case: accumulator fold

The two knobs cover every case where each element renders **independently**
of the others. They do not cover the case where an element's rendering
**depends on the result so far** — a genuine fold with an accumulator. The
canonical example is nesting: `pipe(f, g, h)(x)` → `h(g(f(x)))`, where
`g`'s output contains `f`'s output.

The test: does the rendering *append* (each element produces a
self-contained fragment, glued by `join`) or *wrap* (each element encloses
the accumulated result)? Append is the two-knob model. Wrap needs the
three ingredients of the functional `fold` — the sequence, an initial
accumulator, and a step combining the accumulator with each element —
which means naming the accumulator and giving an init. That is irreducible
ceremony; there is no WYSIWYG form for "this element wraps the result so
far." A possible surface:

```
@@
match: strict
metavar $FNS: sequence
metavar $X: single
@@
- pipe($FNS)($X)
+ $NEST
@@
fold: $FNS
into: $NEST
init: $X
metavar $F: single
@@
- $F
+ $F($NEST)
```

`$NEST` threads the accumulator: `x` → `f(x)` → `g(f(x))` → `h(g(f(x)))`.

This surface is **deferred** until a real wrapping case appears — the same
evidence-driven posture as field mode. Note that matchExhaustive is *not*
such a case: written as a fold, its step (`$ACC.with(...)`) appends, so the
accumulator does no work and the two-knob model suffices. Trying to write
the cases we have as accumulator folds is itself the argument for not
exposing the syntax yet.

## 8. Internal model: two mechanisms

Internally there are two mechanisms:

- **Map** — per-element in-place edits, one edit per matched element at its
  own span; container and separators untouched. Used when a sequence's
  `foreach` is *not* referenced in a restructuring template.
- **Splice** — render the sequence into one value placed in the
  replacement: match each named element against the per-element pattern
  (or, with no `foreach`, take the element's source text), drop
  non-matching elements and separator leaves, and intercalate the rest with
  the `join` separator (default empty). The whole construct is replaced.
  Used when a sequence is referenced in a restructuring template.

Both are *map-then-intercalate* over the elements; neither threads an
accumulator. That is deliberate: every level-1 and level-2 case (§6) renders
each element **independently** of the others, so an accumulator would be
inert. The accumulator is only meaningful for the deferred level-3 fold
(§7), where an element's rendering *wraps* the accumulated result. So the
accumulator-threading engine is deferred together with the level-3 surface
— building it now would be untested-by-surface machinery for a feature that
doesn't exist yet. When level 3 is taken up, the splice mechanism
generalises from map-then-intercalate to a left fold (`append` becoming the
degenerate step); that is an engine change scoped to that work, accompanied
by its surface and tests.

## 9. Output validity: textual construction, and its limits

Transforms produce **text**, not an AST. A `foreach` fragment like
`.with(foo)` is not a valid AST node on its own — it is a sub-syntactic
piece of a chain — and the rendered output is assembled by substitution
and intercalation, then spliced into the source as bytes. Diffract does
not build a result AST.

This is deliberate, and it is the **dual of fragment matching on the match
side**. The match side matches sub-syntactic leaf streams (`} else {`,
`eval(... x,`) that are not valid AST constructs either; that is the
universal tokenizer's headline capability. Treating code as structured
text rather than a mandatory well-formed AST is the architecture's premise
on *both* sides. Requiring the transform side to construct valid ASTs
would make output stricter than input — an asymmetry that contradicts the
premise. So `.with(foo)` being sub-syntactic is weird in exactly the way
`} else {` is weird, and that weirdness is the point.

Validity is therefore a property of the **assembled whole**, not of
individual fragments, and it is addressed at that level:

**Structural breakage** — forgetting a `join` separator (`f(ab)`), or a
malformed template — produces output that does not parse. Because this is
a whole-output property independent of how the output was built, the check
is a **post-hoc re-parse**: after applying edits, re-parse the transformed
region and compare `Tree.error_count` against the original; new ERROR
nodes mean the transform broke the syntax. This reuses the source
diagnostic we already compute, is construction-agnostic, and is the reason
AST-faithful construction (e.g. forcing every splice through the
accumulator fold) is not worth it — the final result is still text, so it
would not *guarantee* validity anyway, while the re-parse check catches the
failure directly. This check is a possible safety feature, not yet built.

**Precedence** — folding `[x || y, z]` into `x || y && z` parses but means
the wrong thing, because `&&` binds tighter than `||`. Diffract has no
precedence model and will not insert parentheses. But correct precedence is
**expressible**, not impossible: the author parenthesizes defensively —
per element via `foreach` (`+ ($E)` → `(x || y) && (z)`, each operand
isolated) and, if the whole result needs isolating at its splice site, in
the outer template (`+ foo(($ELEMS))`). The parentheses are literal
template text, no meta-syntax. Redundant parens on atomic operands (`(z)`)
are harmless and a formatter strips them. So the caveat is "no *automatic*
precedence safety — you parenthesize yourself," not "precedence cannot be
made correct." Because the output is author-controlled text, the escape
hatch is two characters; an AST-constructing model that auto-parenthesized
would need a correct per-language precedence table to deliver what the
author already expresses trivially.

**Associativity** is the one thing parens do not cover, and it is the other
axis already carved out: flat text is left-associative (`a - b - c` =
`(a - b) - c`, correct for `-`); right-association (`a - (b - c)`) is
nesting — the deferred accumulator fold (§7), not a precedence concern.

These limitations — no automatic precedence safety, post-hoc-only validity
— are shared by all textual structural-rewriting tools (Coccinelle, spatch,
Semgrep autofix). They are the accepted cost of not constructing a result
AST, and the cost buys the symmetry with fragment matching.

## 10. Concrete keys in foreach elements: contextual tokenization

A `foreach` element pattern with a **concrete key** — e.g. `color: $V` to
transform only the `color` property — needs care, because the key's
node-type is context-sensitive. The matcher compares leaves on
`(text, node_type)` (§2.1 of the universal-tokenizer note — the precision
that distinguishes an `identifier` from a `type_identifier`), and
tree-sitter assigns the node-type from the grammatical context it infers:

| Text tokenized | tree-sitter parse | node-type of `color` |
| -------------- | ----------------- | -------------------- |
| `{ color: $V }` (with braces) | object literal | `property_identifier` |
| `color: $V` (bare) | labeled statement | `statement_identifier` |

A `foreach` element pattern is written *bare* (just `color: $V`), so on its
own `color` parses as `statement_identifier` and would not match the source
pair's `property_identifier`. (This is TS/TSX-specific: tree-sitter-
typescript aliases `identifier` into `property_identifier` /
`statement_identifier` / `type_identifier` by context; Kotlin, Scala, and
PHP use a single identifier node-type regardless of context, so their
concrete keys already match.)

**The fix: tokenize the element pattern in its container context.** The
`foreach`'s binding section already supplies a valid container — `foo({
$PROPS })`, `<Foo $ATTRS />`, `[$ELEMS]`. The element pattern is spliced
into that scaffold at the sequence-metavar reference (`foo({ color: $V })`,
`<Foo a={$x} />`), the whole thing is tokenized, and the element's own
leaves are extracted by the byte span where they were spliced. The parser
then assigns `color` the `property_identifier` it has in that context, and
`a` the attribute's identifier — correctly, including the JSX tag, because
the scaffold carries it.

Two properties make this fit the architecture:

- **No per-language data in the code.** The context comes from the user's
  own outer pattern, not a hardcoded "objects use braces" table. Extraction
  is by byte span, language-agnostic.
- **No precision lost.** It corrects the node-type by *parsing in context*
  rather than *relaxing* the comparison — so global `(text, node_type)`
  precision (including the TS/TSX type-vs-value disambiguation of §6.4) is
  untouched. This is the only safe automatic use of the grammar's alias
  data: let the parser, which embodies the grammar, assign the right alias
  for the context.

It falls back to standalone tokenization whenever context can't be
established — no binding section, the reference can't be located, or
ellipsis is present (ellipsis preprocessing would shift the byte offsets
the extraction relies on). So it is a pure improvement, never worse than
standalone.

**Bare concrete-key patterns are intentionally not addressed**, and this is
correct rather than a gap: a bare `color: $V` *is* a labeled statement and
`a={$x}` *is* an assignment — a `jsx_attribute` can't even exist outside a
JSX element. So the bare forms honestly mean their standalone parse; an
element-shaped pattern is only legitimately written inside a container
(where `foreach` provides the context). The one mild asymmetry: a bare
*wildcard* `$K: $V` still matches object properties (the wildcard is
node-type-blind), so it over-matches relative to a concrete key — benign,
and inherent to wildcards.

## 11. Element removal and separator cleanup

Removing a list element — a deprecated property, an unused argument — is
expressed by giving the element an **empty replacement**: a `-` line with
no matching `+`. This follows the spatch convention directly: `- legacy: $v`
with nothing on the `+` side replaces the matched span with the empty
string. (`replace_side` returns `None` only for a pure-context guard; a body
with any `-`/`+` line is a transform, and a `-`-only body is the
empty-replacement removal.)

The mechanism is **`foreach`**, not a bare fragment. A list element is
element-shaped (`legacy: $v`, `a = $v`) and only parses correctly in its
container context — exactly the case §10 handles — so removal rides on the
same `foreach` element matching: a binding section captures the sequence
(`f($ARGS)`), and a `foreach $ARGS` section matches the element to delete
and gives it an empty replacement.

The wrinkle is the separator. Deleting just the element's byte span leaves a
dangling separator — `f(a, , c)`, `{ , size }` — a parse error a formatter
cannot fix (§2 punts cosmetics, but not validity). So the engine extends the
deletion to swallow one adjacent separator. The rule (`removal_span`),
computed over the full sequence binding (named elements interleaved with the
separator leaves between them):

- Take the separator **following** the element.
- If there is none — the element is last with no trailing comma — take the
  **preceding** separator instead.
- Absorb whitespace on the far side of the consumed separator so the
  survivors close up.

A **trailing comma** is just a following separator, so it needs no special
case: removing the only element of `f(a,)` consumes the trailing comma →
`f()` (no dangling `f(,)`), and removing the last element of a multi-element
trailing-comma list keeps the idiom — `f(a = 1, b = 2,)` → `f(a = 1, )`,
where the between-comma survives as the new trailing comma. Residual cosmetic
whitespace (the empty-container `foo({  })`, the space before `)`) is
formatter territory; only parse-validity is the engine's concern.

This is genuinely *match-and-delete*: no sequence rendering, `join`, or
filtering predicate. A non-empty replacement on the same `foreach` element is
the in-place edit of §5 (rename/retype); the empty replacement is the only
difference.

## 12. What is not in this design

- **The column-0 separator-prefix syntax** (`~ $VAR`, `, $VAR`, ...) — an
  earlier design for joining sequence expansions. Replaced by the `join`
  preamble directive (§4), which can express multi-character separators
  (`" && "`), keeps meta-syntax out of the body, and defaults to empty so
  the common method-chain case needs no directive at all.
- **The accumulator fold** (§7) — both surface and engine deferred; the
  splice mechanism (§8) is map-then-intercalate and would generalise to a
  left fold when level 3 is built.
- **Sequence filtering** — dropping elements of a *captured sequence*
  (`$PROPS`) by a per-element keep/drop predicate, re-rendering the
  survivors via `join`. This is distinct from element removal (§11): a
  splice over a whole captured sequence rather than a match-and-delete of
  one element.
  Its keep/drop semantics are unsettled (does an element that fails the
  per-element pattern get kept or dropped; is a drop an empty `+` or a
  separate predicate?), and — unlike deletion — it has no concrete driver:
  the property-removal use case that might have justified it is actually
  match-and-delete. Speculative; quite possibly never needed.
- **Parallel iteration / zip** over two sequences (§6) — not included.
- **`foreach` in match-only (search) sections** — per-element constraints
  during search, the search-side dual of the transform-side `foreach`. A
  natural extension; not part of this design.
