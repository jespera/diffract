# Field mode: design note

Field mode (`match: field`) lets a pattern match a structured
declaration while ignoring parts of it the pattern doesn't mention.
This note works through why it is subtler than strict or partial mode
under the tokenizer architecture, what the real grammars force on the
design, and the mechanism that resulted.

Status: **implemented**, with no per-language configuration. The
analysis below (§1–§4) is what the design had to satisfy; §5 is the
as-built mechanism. The headline result is that the per-language
optionality registry the early analysis assumed would be needed turned
out to be unnecessary: the pattern itself, plus the grammar's
required-field structure, carries enough information.

## 1. The use case

A pattern author wants to match a declaration without enumerating
every part of it. The canonical case is a decorated method:

```
@Get("/users")
getUsers() { return this.users; }
```

The author writes `getUsers() { return this.users; }` (or with
metavars) and wants it to match regardless of which decorators,
visibility modifiers, return-type annotations, etc. the source
method carries. Strict mode fails here: the decorator changes the
AST, so the pattern's leaves don't line up with the source's leaves.

The fields a pattern wants to ignore are always the _optional_ ones:
decorators, annotations, attributes, modifiers, return-type
annotations, type parameters. The _required_ structural parts — the
name, the parameter list, the body — the author always writes.

## 2. Why "skip any mismatched child" is wrong

The tempting cheap implementation: walk pattern leaves against source
leaves like strict mode, and on a mismatch, if the source is at the
start of a named child, skip that child and continue. Call this
option A.

Option A is **semantically wrong**, and the parameter list is the
proof:

```
Pattern: function f() {}
Source:  function f(a, b) {}
```

These should _not_ match — an empty-parameter-list pattern shouldn't
match a function with parameters. But option A walks `function`,
`f`, `(`, then sees the pattern wants `)` while the source has `a`
(the first parameter, a named child) — and skips it. It would skip
`a` and `b` and report a match. That is not what the author meant by
writing `f()`.

The distinction option A misses: a field the pattern _omits entirely_
(decorators) should be ignored, but a field the pattern _specifies_
(parameters, even as empty `()`) must match. Option A can't tell the
two apart because it operates on leaves and has no notion of which
fields the pattern populates.

## 3. The correct semantic: only optional fields are skippable

The key observation: **the set of fields a pattern can meaningfully
omit is exactly the set of optional fields.** This is forced by the
grammar. Required fields are syntactically mandatory — you cannot
write a function pattern without a parameter list, because
`function f {}` is not valid source. So the pattern always contains
the required fields, and they must match. Optional fields are the
only ones you can leave out, and leaving one out is the signal "I
don't care about this field."

The early reading of this was: we need to know which fields are
_optional in the grammar_, and skip those — on the source side — when
the pattern doesn't address them. That would mean sourcing optionality
data (a `node-types.json` loader or an allowlist).

The implemented mechanism (§5) reaches the same semantic **without**
that data, by turning the observation around. Since the pattern always
contains the required fields, a matcher that walks the declaration's
children and skips any child the pattern doesn't match — matching the
ones it does — skips _exactly_ the optional fields: the required ones
can't be skipped, because their pattern segment would then have no
child to bind to and the match would fail. The pattern's presence or
absence of a field is the optionality signal; the grammar's required
structure makes relying on it safe. So no optionality lookup is needed.

The parameter problem stays fixed: a pattern `f()` addresses the
parameter list, so it is matched (not skipped), and an empty `()`
cannot match `(a, b)`. The pattern stays a flat leaf stream — no
pattern-fragment parsing, no wrap-and-peel.

## 4. What the grammars actually force

The clean story above assumes "optional field" is a uniform,
grammar-readable concept. Inspecting the five supported grammars
shows it is not. The skippable content (decorators/annotations/
attributes and modifiers) and the core fields manifest in markedly
different ways:

| Language | Decorator / annotation / attribute                                     | Modifiers                                                                                                                                                                                                                           | Core (name / params / body)                            |
| -------- | ---------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------ |
| TS / TSX | `decorator:` — **field-labeled** child                                 | `async` is an **anonymous keyword leaf**; `abstract` changes the **node type** (`abstract_class_declaration`); `export` is an **outer wrapper** (`export_statement`); `accessibility_modifier` is a **named child, no field label** | field-labeled                                          |
| PHP      | `attributes:` — **field-labeled**                                      | `visibility_modifier`, `static_modifier` — **named children, no field labels**                                                                                                                                                      | field-labeled                                          |
| Kotlin   | `annotation` nested in a `modifiers` — **named child, no field label** | `visibility_modifier`, `function_modifier` also in `modifiers`                                                                                                                                                                      | **no field labels at all** — positional named children |
| Scala    | `annotation` — **named child, no field label**                         | `modifiers` — **named child, no field label**                                                                                                                                                                                       | field-labeled                                          |

Three consequences:

**4.1 "Skip optional _fields_" does not generalize — Kotlin has no
fields.** A Kotlin `function_declaration`'s children are positional
named nodes (`modifiers`, `simple_identifier`,
`function_value_parameters`, `function_body`) with zero field labels.
Any rule keyed on field labels fails outright here. The rule must be
keyed on **named-child node types**, not field labels.

**4.2 The skippable content is sometimes not a named child at all.**
In TS, `async function foo()` and `function foo()` produce _identical_
`function_declaration` subtrees — `async` is an anonymous keyword
leaf, invisible as a child. `abstract` changes the node type;
`export` wraps the whole declaration. None of these can be skipped by
a rule that operates on named children or fields.

**4.3 The decorator-subset case is partial-list matching, not
field-skipping.** If the pattern has `@Get` and the source has
`@Auth @Get @Cache`, the author wants to match one decorator out of a
list, with the others ignored. That is partial-mode semantics over
the decorator list, not "skip an optional field." A naive
skip-on-mismatch even mishandles it: the pattern's leading `@`
matches `@Auth`'s `@` and then fails on the identifier.

## 5. The implemented mechanism: child alignment with backtracking

Field mode aligns the pattern's flat leaf-stream to a **subsequence of
the declaration node's children**, left to right. At each child the
matcher has two choices, explored with backtracking, MATCH first:

- **MATCH** — consume the prefix of the remaining pattern that exactly
  covers this child's subtree (the existing `match_prefix` primitive),
  then advance to the next child with the leftover pattern.
- **SKIP** — leave the pattern untouched and advance to the next child.

The pattern must be fully consumed for a match; children left over once
the pattern is exhausted are tolerated extras.

No optionality data is consulted (§3): the required fields are in the
pattern and get matched in order, so the only children left unmatched
are the optional ones the pattern omitted. MATCH-first means a child the
pattern addresses is matched in full — `f()` does not match `f(a, b)`,
because the parameter list is matched, not skipped. SKIP plus
backtracking give two behaviours at once:

- **Ignore what I didn't mention.** Unaddressed optional children
  (decorators, annotations, the `modifiers` group, return types) are
  skipped.
- **Match this element among several of its shape.** A concrete
  `@deprecated` skips a leading `@inline` to reach the annotation it
  matches — the decorator-subset case (§4.3) falls out of backtracking
  for free, wherever those decorators are separate children.

Worked example (`@deprecated def $m() = { l }` against
`@inline @deprecated def getUsers() = { l }`): `@inline` doesn't match
the pattern's `@deprecated` segment, so it is skipped; `@deprecated`
matches; `def` matches; `$m` binds `getUsers`; `()` matches; the body
matches. A return-type mismatch still fails correctly — `$m(): number`
against `$m(): string` matches `:` into the return-type child and then
mismatches its content, with no later home for the pattern's `number`.

### 5.1 Walk all children, not just named ones

The walk is over **all** non-extra children (a new `Cursor.all_children`),
because a declaration interleaves named children with anonymous leaves
the pattern addresses or omits:

- The structural keyword (`fun`, `def`, `function`) is an anonymous
  leaf. The pattern contains it, so it is matched.
- Kotlin and Scala place the optional return type's introducing `:` as
  an anonymous child. The pattern omits it, so it must be skippable.

Both follow from the same uniform rule — skip any child the pattern
doesn't match. A required keyword the pattern *does* contain can't be
silently dropped: its pattern token would find no later child to bind
to, and the alignment fails.

### 5.2 The "Tier 3" gap dissolved

An earlier draft flagged TS anonymous-keyword modifiers (`async`,
`abstract`, `export`) as an unreachable "Tier 3":

- `export` wraps the declaration in `export_statement` — already handled
  for free, since the search loop tries the inner declaration node.
- `async` is an anonymous keyword leaf; `abstract` changes the node
  type.

Under §5.1's uniform rule these need no special handling. `async` is an
anonymous child the pattern omits, so it is skipped like any other
unaddressed child — `$m() { $b }` matches `async getUsers() {...}`.
`abstract` produces a different node type that the search loop visits
directly. And the design does **not** over-match: a pattern with a body
(`{ $b }`) cannot match a bodyless `abstract foo(): void;` signature,
because there is no `statement_block` child for the body segment to
bind. So the gap the registry was meant to paper over does not exist;
no allowlist of skippable keywords is needed.

### 5.3 Concrete names: source-context tokenization

A field pattern with a concrete name — `getUsers() { $b }` — hits the
same node-type aliasing as concrete object keys. Tokenized on its own,
`getUsers` parses as an `identifier`; the source method's name is a
`property_identifier`; field mode's strict `(text, node_type)` comparison
(kept deliberately — strict/partial modes compare lexically, see
`docs/universal-tokenizer.md` §2.1, but field mode's contextual tokens make
strictness both correct and cheap) misses and the
pattern finds nothing. Unlike the `foreach` concrete-key case (docs
[transforms.md §10]), a field pattern has no surrounding container to
borrow context from — it *is* the top-level construct.

The fix is to tokenize the pattern **in the context of the source it's
being matched against**. For each candidate node the matcher builds a
context string — the whole source with that node's byte span replaced by
the pattern's match text — tokenizes it, and keeps the pattern's own
leaves by span (the existing `tokenize_span` peel). `getUsers` then gets
the `property_identifier` it has inside the real class. Crucially the
*alignment* still runs on the bare declaration node, so decorators and
the return type are still skipped and the span is still the method.

Why the whole source and not a cheaper ancestor subtree: an ancestor in
isolation is not reliably parsable into the same roles. A class body's
`{ … }` reparsed alone is an *object literal* — it agrees on
`property_identifier` only by coincidence, and class-only constructs
would diverge or ERROR. Splicing into the real source puts the pattern in
its true ancestral context by construction. It degrades to standalone
tokenization on the same fallbacks as `foreach` (ellipsis, or a splice
that doesn't parse) — never worse than before.

Each reparse is whole-source, so two cost controls keep it from being a
per-node tax:

- **Gate.** If the pattern has no concrete identifier-family token (only
  metavars and punctuation, e.g. `$m() { $b }`), context can't change any
  node-type, so source-context is skipped entirely — that search runs at
  plain standalone speed with zero reparses.
- **Relaxed probe.** Otherwise, each candidate is first tested with a
  node-type-*ignoring* alignment (a cheap superset of the precise match).
  A candidate that fails the probe can't match precisely either, so it is
  skipped without a reparse; only candidates that pass — essentially the
  one construct kind the pattern targets — trigger a reparse, memoized by
  node-type. In practice this is ~1 reparse per search.

Measured: on a class of 500 decorated methods, a concrete-name field
search dropped from ~970ms (one reparse per distinct node-type) to ~35ms
(~2.5× a raw parse) with the probe in place; a metavar-only pattern pays
no reparse at all.

This is the same `tokenize_span` primitive the `foreach` fix uses; the
only difference is the context string is built from the source rather
than from the user's pattern. It is the general "wrap-and-peel" tool that
partial-mode composition (if ever built single-section) would reuse.

## 6. What this covers, and the one structural caveat

Covered across all five languages, with no per-language data:

- **Decorators / annotations / attributes** — skipped when unmentioned;
  matched (subset, via backtracking) when the pattern names one and they
  are separate children.
- **Modifiers** — PHP `visibility_modifier`/`static_modifier`, the
  Kotlin and Scala `modifiers` group, TS `async`/`abstract`.
- **Return types and other optional trailing fields** — skipped when
  the pattern omits them, content-checked when it includes them.
- **Concrete declaration names** — `getUsers() { $b }`, not just a
  metavar `$m() { $b }`. This needs care: tokenized standalone,
  `getUsers` is an `identifier`, but a class method's name is a
  `property_identifier`, so the `(text, node_type)` comparison would
  miss. See §5.3.

The one caveat is about **what the matched span is**, which matters for
transforms. The span is the declaration node, so:

- In PHP, Kotlin, and Scala the annotation/modifier children are
  *inside* the declaration node — the match span includes them, and a
  field-mode replace rewrites them.
- In TS, method decorators are *siblings* of `method_definition` in the
  class body — the span excludes them, and a replace leaves them in
  place.

This is a property of where each grammar puts decorators, surfaced
rather than hidden. The decorator-subset variants that are *not* plain
child-level backtracking — PHP/Kotlin keep multiple annotations inside a
single `attribute_list`/`modifiers` child (a nested match), and TS
methods keep them as siblings — are natural follow-ups, not covered by
the first cut.

## 7. Note: no shared per-language registry after all

An earlier draft anticipated a per-language registry shared between
field mode and partial-mode composition. Field mode turned out not to
need it (§3, §5), and partial-mode composition is covered by
multi-section without one either (see
[partial-mode.md §6](partial-mode.md)). The registry idea now survives
only as possible future support for two deferred, niche features — the
single-section composition *syntax* and Kotlin named-argument partial
([partial-mode.md §7](partial-mode.md)) — not as infrastructure either
implemented mode depends on.
