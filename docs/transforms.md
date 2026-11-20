# Transforms: design note

Transforms are the replacement (`+`) side of a pattern: once a pattern
matches, the bound metavars are substituted into a replacement template
and the matched span is rewritten. This note records the design of
transforms on the universal-tokenizer matcher — in particular how
sequence metavars are rendered into a replacement, which is where the
interesting decisions are.

Status: **implemented**. Single-binding transforms, multi-section
transforms, rewrites scoped to a sequence's elements (`on`), and sequence
splicing with a `join` separator all run on the IR
(`StrictSeq`/`PartialContainer`/`Within`/`All` + `eval_ir`). A separate
`foreach` directive, and with it rendering *rewritten* elements into a new
construct, was implemented and then removed (§7).

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

## 3. In place vs splice: what span is replaced

Transforms over a sequence come in two shapes, distinguished by **what
gets replaced**:

**In place** replaces *parts of elements*. Renaming a property, wrapping
each value: the edits are local, applied where the matched text sits. The
container and its separators are untouched — renaming `x` to `y` in
`{x: 1, z: 2}` never moves the comma, because nothing is gathered and
re-joined.

**Splice** replaces the *whole enclosing construct*. Rewriting
`all([a, b, c])` to `(a && b && c)`: the entire call is gone, and the
sequence's elements are carried, unchanged, into a new structure with a new
separator between them.

The two are separate operations with separate surface syntax: in-place
edits are an `on` section scoped to the sequence (§5), a splice is a
reference to the sequence metavar in a `+` template (§4). A sequence is
used one way or the other in a rule, never both (§7).

## 4. Splicing a sequence: the `join` separator

When a sequence metavar is referenced in a `+` template, its elements'
source text is substituted there. The one dial is what goes between them:

- **`join $VAR by "<sep>"`** — the *glue* placed between elements
  (intercalation: n−1 separators for n elements). Default: empty.

| Use case | `join` |
| --- | --- |
| `[a, b, c]` → `a && b && c` | `" && "` |
| `listOf(a, b, c)` → one per line in a builder block | `"\n"` |
| `combine(a, b, c)` → `joined(a, b, c)` | `", "` |

The directive lives in the preamble — the structured directive zone — so
the replacement *body* stays pure: literal text plus metavar placeholders,
with no embedded meta-syntax. This is the same boundary the match side
holds (real source tokens, no sigil ceremony); neither body carries
directives.

The original separators are not reused. The binding holds the separator
leaves alongside the elements; a splice drops them and intercalates the
`join` string, because the new construct generally needs different glue
than the old one (a `,` between array elements becoming `&&` between
operands). The separator is semantic — a formatter will not add `&&` — and
new, so it cannot be inferred; it must be stated. An empty default means a
missing `join` produces visibly glued output (`f(ab)`) rather than a guess.

## 5. `on`: scoping a section to a binding

A section can be scoped to a metavar bound by an earlier section:

- **`on $VAR`** — search this section only inside what `$VAR` bound: the
  one subtree of a **single** metavar, or each element of a **sequence**.

"Inside" means the bound node itself or anything nested in it. For a
sequence each element is searched separately, so a match must lie within
one element.

```
@@
match: strict
metavar $PROPS: sequence
@@
foo({ $PROPS })
@@
match: strict
on $PROPS
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ $KEY: wrap($VAL)
```

`foo({a: 1, b: 2})` → `foo({a: wrap(1), b: wrap(2)})`. Commas never move.

There is one directive and it means one thing; the metavar's declared kind
only decides how many subtrees are searched. Three consequences follow
from machinery that already exists, rather than from rules of their own:

- **Every match is rewritten.** A scoped section yields one composite per
  match, and each contributes its edit. "Apply to every element" needs no
  loop construct.
- **Outermost wins.** A transform takes non-overlapping matches, leftmost
  and outermost first, so an element that itself matches shadows what is
  nested in it: `foo({a: 1, b: {c: 2}})` → `foo({a: wrap(1), b: wrap({c:
  2})})`. When a top-level element does *not* match, a match nested in it
  is found — the one behavioural difference from testing each element as a
  whole.
- **The rule needs a match.** Sections are a conjunction (`All`), so a
  scoped section that matches nowhere means the rule does not fire.

The scope need not be list elements. In
`{` / `PRE` / `- val x = E` / `POST` / `}` the sequence `POST` is the
statements after a declaration, up to the closing brace, and `on POST` is a
rewrite confined to the rest of that block — lexical scope read off the
tree, with no name resolution.

## 6. Multiple references and multiple sequences

A sequence metavar in a `+` denotes a **value** (its elements' source text,
intercalated), not an iteration point — the `+` line never loops. So
multiple references compose as plain substitutions:

- The **same** sequence referenced twice → the same value substituted at
  both sites.
- **Two different** sequences in one `+` → each rendered independently
  (its own `join`) and spliced at its own site.

Because the line is a template with value-holes rather than a loop body,
there is no "iterate over which?" ambiguity and no cross-product. Two
different sequences are rendered **independently, not zipped**; parallel
iteration (a zip) is a separate capability this design does not include.

## 7. Removed: rendering rewritten elements into a template

An earlier version had a second scope directive, `foreach $SEQ`, which
tested each element of a sequence against the section's pattern as a whole.
It had two meanings. With the sequence only on context lines it edited the
matching elements in place. With the sequence also referenced in a `+`
template it instead *rendered* each element, and the rendered fragments
were joined and spliced. The one driver for the second was

```
- matchExhaustive($TAG, { $CASES });
+ match($TAG)$CASES.exhaustive();
@@
foreach $CASES
@@
- $KEY: $VAL
+ .with("$KEY", $VAL)
```

turning `matchExhaustive(tag, {a: f, b: g})` into
`match(tag).with("a", f).with("b", g).exhaustive()`. The rendering form was
removed first, for these reasons:

- **One directive, two meanings, chosen implicitly.** Whether `foreach`
  edited in place or rendered depended on whether some other section's `+`
  line happened to mention the sequence.
- **Silent loss.** Rendering kept only the elements the per-element pattern
  matched. An element it did not match — a spread, a shorthand method —
  was dropped from the output without notice. In place, the same
  non-match is harmless: the element just stays.
- **The metavar changed type between lines.** On the `-` line `$CASES` is a
  list of object properties; on the `+` line it is a string of call
  suffixes. `.with("a", f)` is not an expression, and
  `match($TAG)$CASES.exhaustive()` is not code that can be read or checked
  without first working out what the other section produces.
- **It is a fold written as a concatenation.** A method chain is nested —
  `(match(tag).with("a", f)).with("b", g)` — so each case wraps the result
  so far. Expressed as a fold, every line is well-formed and there is no
  separator to choose:

  ```
  - matchExhaustive($TAG, { $CASES });
  + $CHAIN.exhaustive();
  @@
  fold $CASES into $CHAIN from match($TAG)
  @@
  - $KEY: $VAL
  + $CHAIN.with("$KEY", $VAL)
  ```

  The same shape covers genuine nesting, `pipe(f, g, h)(x)` →
  `h(g(f(x)))`, which concatenation cannot express at all.

A fold is the principled form of this capability, and it is **not built**:
one motivating rule does not justify the surface or the engine. This
section records where the design would resume if a second driver appears.

With rendering gone, the in-place form of `foreach` had nothing left that
other constructs did not do, and it was removed too:

- **Rewriting elements** is `on $SEQ` (§5). `on` used to reject a sequence
  only because `foreach` existed to take it.
- **Deleting an element** is a `-` line between context `...` lines, with
  the separator absorbed from the `...` run beside it (§11). That cleanup
  used to exist only for `foreach`.

What is no longer expressible is "every top-level element that matches, and
nothing nested inside the ones that do not". `foreach $VAR` is rejected
with a message naming `on $VAR`.

## 8. Internal model

- **Scoped sections** are the `Within (name, sub)` IR node: `eval_ir`
  evaluates `sub` under a cursor narrowed to each subtree `name` bound —
  one for a single, each element for a sequence — and concatenates the
  results. Their edits are ordinary surgical edits; nothing downstream
  knows the section was scoped.
- **Splice** (`render_identity`) — the named elements' source text,
  separator leaves dropped, intercalated with the `join` separator
  (default empty), substituted wherever a template references the
  sequence. The whole construct is replaced.

A sequence that is both spliced and edited under `on` produces an edit
nested inside a replaced span, which `apply_edits` rejects like any other
overlapping rewrite.

## 9. Output validity: textual construction, and its limits

Transforms produce **text**, not an AST. A `+` line may be a sub-syntactic
fragment, and the output is assembled by substitution and intercalation,
then spliced into the source as bytes. Diffract does not build a result
AST.

This is deliberate, and it is the **dual of fragment matching on the match
side**. The match side matches sub-syntactic leaf streams (`} else {`,
`eval(... x,`) that are not valid AST constructs either; that is the
universal tokenizer's headline capability. Treating code as structured
text rather than a mandatory well-formed AST is the architecture's premise
on *both* sides. Requiring the transform side to construct valid ASTs
would make output stricter than input — an asymmetry that contradicts the
premise.

What a `+` line may not do is change what a metavar *denotes*. A fragment
is fine; a sequence metavar that means "these properties" on the `-` line
and "these call suffixes" on the `+` line is not, and that is the line §7
draws.

Validity is therefore a property of the **assembled whole**, not of
individual fragments, and it is addressed at that level:

**Structural breakage** — forgetting a `join` separator (`f(ab)`), or a
malformed template — produces output that does not parse. Because this is
a whole-output property independent of how the output was built, the check
is a **post-hoc re-parse**: after applying edits, re-parse the transformed
region and compare `Tree.error_count` against the original; new ERROR
nodes mean the transform broke the syntax. This reuses the source
diagnostic we already compute, is construction-agnostic, and is the reason
AST-faithful construction is not worth it — the final result is still
text, so it would not *guarantee* validity anyway, while the re-parse check
catches the failure directly. This check is a possible safety feature, not yet built.

**Precedence** — folding `[x || y, z]` into `x || y && z` parses but means
the wrong thing, because `&&` binds tighter than `||`. Diffract has no
precedence model and will not insert parentheses. But correct precedence is
only partly **expressible**: the author can isolate the whole result at
its splice site in the outer template (`+ foo(($ELEMS))`), as literal
template text, but cannot parenthesize each operand — that would be a
per-element rendering (§7). A rule whose elements may contain a
lower-precedence operator than the `join` glue is therefore not safe to
write as a splice.

**Associativity**: flat text is left-associative (`a - b - c` =
`(a - b) - c`, correct for `-`); right-association (`a - (b - c)`) is
nesting — a fold (§7), not a precedence concern.

These limitations — no precedence safety, post-hoc-only validity
— are shared by all textual structural-rewriting tools (Coccinelle, spatch,
Semgrep autofix). They are the accepted cost of not constructing a result
AST, and the cost buys the symmetry with fragment matching.

## 10. Concrete keys in element patterns

An element pattern with a **concrete key** — `color: $V`, to rewrite only
the `color` property — is written bare, and a bare `color: $V` parses as a
labeled statement, so its `color` leaf is a `statement_identifier` where
the source property's is a `property_identifier`. (TS/TSX-specific:
tree-sitter-typescript aliases identifiers by context; Kotlin, Scala and
PHP use one identifier node-type.)

This needs no special handling. Strict matching compares leaves
*lexically* — text, plus whether the leaf sits inside a string literal —
and tolerates a disagreement in node-type, precisely because a pattern
body is a fragment whose re-parse assigns roles unreliably (see
`universal-tokenizer.md`). So `color: $V` under `on $PROPS` matches the
property as written.

`foreach` predated lexical matching and carried its own fix: it re-tokenized
the element pattern spliced into the binding section's container
(`foo({ color: $V })`) so the parser assigned the contextual node-type.
That mechanism went with the directive.

## 11. Element removal and separator cleanup

Removing a list element — a deprecated property, an unused argument — is a
`-` line with no matching `+`, written in the section that matches the
list, between context `...` lines:

```
  f(
  ...
- gone
  ...
  )
```

The wrinkle is the separator. Deleting just the element's byte span leaves
a dangling separator — `f(a, , c)`, `{ , size }` — a parse error a
formatter cannot fix (§2 punts cosmetics, but not validity). So the
deletion is extended to swallow one adjacent separator
(`strict_separator_cleanup`). The `...` on either side of the `-` line
bound the runs of siblings next to the element, separator leaves included,
and the rule reads them:

- Take the separator **following** the element: the first cursor of the
  following run, if it is an unnamed leaf.
- If there is none — the element is last with no trailing comma — take the
  **preceding** separator instead: the last cursor of the preceding run.
- Absorb whitespace on the far side of the consumed separator so the
  survivors close up.

A **trailing comma** is just a following separator, so it needs no special
case: removing the only element of `f(a,)` consumes the trailing comma →
`f()` (no dangling `f(,)`), and removing the last element of a
multi-element trailing-comma list keeps the idiom — `f(a, b,)` → `f(a, )`,
where the between-comma survives as the new trailing comma. Residual
cosmetic whitespace (the empty-container `foo({  })`, the space before
`)`) is formatter territory; only parse-validity is the engine's concern.

No separator table is involved. Brackets are concrete pattern tokens and
never part of a run. A statement's own terminator is inside the statement
node, so it is never a sibling leaf. A neighbour written as a concrete
context token is kept as written — the pattern says to keep it — and a
separator the author already put on a `-` line is not doubled.

Partial mode has its own cleanup (`element_cleanup`), which scans the
source text for `,`/`;` beside the deleted span. A `-`-only section under
`on` has neither a container nor a `...` beside its match, and does not
clean up separators.

## 12. What is not in this design

- **The column-0 separator-prefix syntax** (`~ $VAR`, `, $VAR`, ...) — an
  earlier design for joining sequence expansions. Replaced by the `join`
  preamble directive (§4), which can express multi-character separators
  (`" && "`) and keeps meta-syntax out of the body.
- **Per-element rendering into a template, and the fold that would
  replace it** (§7) — removed and not built, respectively.
- **Sequence filtering** — dropping elements of a *captured sequence* by a
  per-element keep/drop predicate and re-rendering the survivors. Not
  offered: the only deletion a rule can express is the explicit
  match-and-delete of an element (§11).
- **Parallel iteration / zip** over two sequences (§6) — not included.
- **Whole-element anchoring** — restricting a scoped section to the bound
  nodes themselves, excluding what is nested in them (§7). No driver.
