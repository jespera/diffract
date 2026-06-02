# Partial mode: design note

Partial mode (`match: partial`) lets a pattern match a key-bearing
container — an object literal, a JSX self-closing element, a record
type, ... — while tolerating extra elements the pattern doesn't mention
and ignoring the source's element order. This note records why the
v1 design works without any per-language configuration and where the
boundary of v1 lies.

Status: **implemented**. Whole-pattern partial works for any grammar
where the container is structurally `[anon...] named...named [anon...]`
— TS objects, TSX JSX elements, record types, and so on. Composition (a
partial container nested inside a larger strict pattern, like
`foo({a: $x})`) is **covered via multi-section** — a strict section binds
the container and an `on $VAR`-scoped partial section matches inside it
(§6); the single-section `foo({a: $x})` *syntax* is rejected at compile
time (it can't be a container) and remains deferred sugar — see §7.

## 1. The use case

A pattern author wants to match a container by some of its keys,
ignoring the rest. The canonical object case:

```
@@
match: partial
metavar $x: single
@@
{a: $x}
```

This should match `{a: 1, b: 2, c: 3}` and bind `$x = 1`. The
canonical JSX case is the same idea over attributes:

```
@@
match: partial
metavar $p1: single
metavar $p2: single
@@
<Foo a={$p1} b={$p2} />
```

This should match `<Foo b={2} a={1} c={3} />` — reordered, with an
extra `c` — and bind `$p1 = 1`, `$p2 = 2`.

Strict mode fails for both: source order or count disagrees with the
pattern. Partial mode says "treat the container's elements as a set;
the pattern names a subset."

## 2. Naive approaches and why they were rejected

**A. Hardcoded bracket table.** "Strip `{` and `}` from the pattern,
set-match the rest." Works for objects, but `/>` is a single combined
token in TSX, `<...>` (opening) differs from `<.../>` (self-closing),
and future grammars may bracket differently. Hardcoding bakes in
grammar facts that belong on the source side.

**B. Pattern-side AST.** Parse the pattern, find its bracket-pair
regions, drive matching from the pattern's structure. Defeats the
universal-tokenizer's central asymmetry — the pattern side is meant
to be language-agnostic, with all per-language knowledge living on
the source side via tree-sitter.

**C. Skip-on-mismatch.** Walk pattern leaves against source leaves;
when mismatched at a child boundary, skip the child. This is the same
shape as field mode's option A (see
[field-mode.md §2](field-mode.md)), and it fails the same way: the
matcher can't tell content the pattern *meant* to ignore from content
it just didn't address, so it over-matches.

## 3. The v1 design: source-driven delimiters

The matcher reads two structural facts from each candidate source
container:

- The **leading anonymous-leaf run** — the run of non-named non-extra
  children before the first named child. For a TS object this is
  `['{']`; for a JSX self-closing element it's `['<']`; for a
  parenthesised expression `['(']`.
- The **trailing anonymous-leaf run** — symmetrically, after the last
  named child. `['}']`, `['/>']`, `[')']`.

The pattern is then split into three slices in lock-step with that
structure:

- A **front** of `n_lead` tokens, strict-matched leaf-by-leaf against
  the leading run.
- A **middle**, set-matched against the source's named children.
- A **back** of `n_tail` tokens, strict-matched against the trailing
  run.

The matcher never decides on its own which tokens are "delimiters".
The decision is whatever the source grammar said about the
container's anonymous children. The same matcher code adapts
uniformly across `{}`, `<>`, `/>`, `()`, `[]`, and any future
grammar's brackets — without a single hardcoded character.

## 4. The structural self-filter

A real bracketed container is the only kind of node that has both a
leading and a trailing anonymous run. Requiring `n_lead > 0 && n_tail
> 0` is the structural self-filter:

| Source node                                  | `n_lead` | `n_tail` | Outcome           |
|----------------------------------------------|----------|----------|-------------------|
| TS `object` `{a: 1}`                         | 1 (`{`)  | 1 (`}`)  | Candidate         |
| TSX `jsx_self_closing_element` `<Foo />`     | 1 (`<`)  | 1 (`/>`) | Candidate         |
| TS `array` `[1, 2]`                          | 1 (`[`)  | 1 (`]`)  | Candidate         |
| TS `parenthesized_expression` `(x)`          | 1 (`(`)  | 1 (`)`)  | Candidate         |
| TS `variable_declarator` `o = {a: 1}`        | 0        | 0        | Rejected early    |
| TS `expression_statement` `doIt();`          | 0        | 1 (`;`)  | Rejected early    |
| TS `program` (top level)                     | 0        | 0        | Rejected early    |

For each candidate, the front/back strict-match then rejects nodes
whose actual delimiters disagree with the pattern's (e.g. a `[`
pattern won't match an object's `{`).

Why both runs must be non-empty: without that rule, a node with no
anonymous children but one named child that *happens* to match the
entire pattern as a strict subtree would over-match. The
`variable_declarator` `o = {a: 1}` is the proof — its named children
are `[o, object]`; the pattern `{a: $x}` (tokens `[{, a, :, $x, }]`)
would set-match the object child as a single element and report a
"partial match" whose actual extent is just the object, but whose
reported span is the whole declarator. That's a strict subtree match
collapsed into a partial set-match. Requiring both runs non-empty
rules this out at the structural level, with no need to inspect the
pattern.

## 5. Set-matching the middle

Once front and back are validated, the middle pattern tokens are
handed to the set-match driver (`match_set_at`), which:

- Tries to consume each pattern element by calling `match_prefix`
  against each unused source named child in turn. The first child
  whose `match_prefix` succeeds is locked in for this element.
- Backtracks if a later element fails after a particular lock-in.
- Tolerates source named children that aren't claimed by any pattern
  element — they remain unused and are silently accepted.
- Strips inter-element separator tokens (`,`, `;`, ...) inferred from
  the bytes between the source's first two named children. JSX
  containers contribute an empty separator and no stripping occurs.
- Threads accumulated bindings across element matches so cross-element
  non-linearity (the same named wildcard appearing in two elements)
  is enforced.

`match_set_at` is exposed separately from `match_partial_at` so
fixture-based tests can exercise this logic against hand-built trees
whose cursors don't model the named-vs-anonymous distinction.
Production callers go through `match_partial_at`, which composes the
strict delimiter handling on top.

## 6. What v1 covers and what it doesn't

**Covers** — uniformly across grammars, no per-language config:

- Whole-pattern partial over any container whose grammar exposes
  `[anon...] named...named [anon...]` shape. In practice that's TS
  `object`, `object_pattern`, `object_type`, TSX
  `jsx_self_closing_element` and `jsx_opening_element`, and the
  equivalents in PHP and Scala.
- Reordering, extras, and source-derived separators — automatically,
  from the source structure.
- Self-filter cleanly rejects non-container nodes; no false positives
  observed.

**The single-section composition form is rejected, not silent.** A
single-section partial pattern must *be* a bracketed container — its
first and last tokens must be anonymous delimiters. A named-wrapper form
like `foo({a: $x})` begins with an `identifier` token that can never match
a container delimiter, so it could only ever yield zero matches. Rather
than fail silently, the matcher rejects it at compile time with a `Failure`
that points at the multi-section idiom below. (Earlier it yielded zero
matches; the raise is strictly more informative.)

**Composition is covered — via multi-section, no registry needed.**
"A call whose object argument contains `a`" is expressed with two
sections: a strict section binds the argument, and an `on $VAR`-scoped
partial section subset-matches inside it.

```
@@
match: strict
metavar $ARG: single
@@
foo($ARG)
@@
match: partial
on $ARG
metavar $x: single
@@
{a: $x}
```

This matches `foo({a: 1, b: 2})` and not `foo({c: 3})` or `bar({a: 9})`.
The outer/inner mode switch the matcher would otherwise need (§7) is
replaced by section composition: each section runs in its own mode, the
`on` directive scopes the inner partial to the subtree the outer section
bound. This is the recommended way to compose modes today.

**Does not cover** — by deliberate scope:

- **Single-section composition syntax.** The ergonomic `foo({a: $x})`
  form (one pattern, mode-switch mid-walk) is not built — multi-section
  delivers the capability, so this would be sugar. See §7.
- **Kotlin (and similar) named-argument partial.** Pattern `Foo(a =
  $x)` matching `Foo(a = 1, b = 2)` would require detecting Kotlin's
  `value_argument` whose first child is a `simple_identifier` as a
  name-bearing element. Kotlin has no field labels, so this needs a
  per-language structural rule — see §7. (This is a distinct need from
  composition: it is about recognising the container at all, not nesting.)

## 7. The per-language registry (and single-section composition sugar)

Two things remain beyond v1, and both want the same per-language data.

The first is the **single-section composition syntax** — letting one
pattern `foo({a: $x})` mode-switch mid-walk instead of writing two
sections. The capability is already delivered by multi-section (§6), so
this is purely ergonomic sugar; it is noted here because it shares the
machinery below. It needs the matcher to walk strictly until it reaches a
set-like inner container, switch to partial for that subtree, then resume
strict. The §4 structural self-filter is not sufficient for that decision:
a TS `statement_block` (function body) *also* has `{` and `}` as anonymous
runs, so the matcher can't tell "this is a set-like object" from "this is a
positional statement block" by structure alone.

The second, and the one with real capability behind it, is **Kotlin-style
named-argument partial** (`Foo(a = $x)` over `Foo(a = 1, b = 2)`) — there
the issue is recognising `value_arguments` as a set-like container at all,
which structure alone also can't decide.

That's the moment where an explicit, declarative per-language
registry earns its keep. An entry would describe how to recognise
the container, how to identify its elements, and how to extract each
element's name and value:

```
type set_like_entry = {
  container_type    : string;    (* "object", "value_arguments", ... *)
  element_type      : string;    (* "pair", "value_argument", ... *)
  name_at           : name_extractor;
  element_predicate : predicate;
  separator         : separator_source;
}

and name_extractor =
  | Field of string         (* TS pair: name at field "key" *)
  | FirstNamedChild         (* JSX attribute, Kotlin named arg *)
  | None_                   (* positional element, no name *)
```

For the languages we support, the entries are small:

| Container                              | Element             | Name                | Predicate            |
|----------------------------------------|---------------------|---------------------|----------------------|
| TS `object`                            | `pair`              | `Field "key"`       | always               |
| TS `object_pattern`                    | pair-shaped         | `FirstNamedChild`   | always               |
| TS `object_type`                       | `property_signature`| `Field "name"`      | always               |
| TSX `jsx_self_closing_element`         | `jsx_attribute`     | `FirstNamedChild`   | always               |
| TSX `jsx_opening_element`              | `jsx_attribute`     | `FirstNamedChild`   | always               |
| Kotlin `value_arguments`               | `value_argument`    | `FirstNamedChild`   | "has 2 named kids"   |

The `Predicate` is what makes Kotlin work: a `value_argument` with one
named child is positional; with two, named. The container is set-like
only when its children pass the predicate.

**What the registry unlocks:**

1. **Single-section composition sugar.** `foo({a: $x})` against
   `foo({a: 1, b: 2})`: the matcher walks the call strictly, sees the
   inner `{...}` is a registered TS `object`, and partial-matches it —
   the one-pattern form of what multi-section (§6) already expresses.
2. **Kotlin named-argument partial.** Pattern `Foo(a = $x)` matches
   Kotlin `Foo(a = 1, b = 2)` once `value_arguments` with all-named
   children is registered as set-like. This is genuinely new capability,
   not sugar.

(An earlier draft expected field mode to share this registry. It does
not — field mode shipped without any per-language data, by aligning the
pattern to the declaration's children, see
[field-mode.md §5](field-mode.md). So the registry is now justified, if
ever, only by the two partial-mode features above.)

**Why still deferred.** The cases v1 covers don't need the registry —
the structural self-filter does the work — and composition itself is now
covered by multi-section (§6), which needs no registry at all. What the
registry would add is the *single-section* mode-switching form (sugar
over multi-section) and Kotlin named-argument partial (genuine, but
niche). Both require the matcher to become aware of source node
boundaries during a strict walk, recognise a registered container, switch
modes, and resume — a real matcher extension. With the capability already
reachable via multi-section, the pressure to build it is low; the registry
+ mode-switching matcher arrive together if the single-section ergonomics
or Kotlin named-arg partial become concretely wanted.

## 8. Scope decisions

- **Composition: covered by multi-section.** "A call whose argument
  contains X" is expressed with a strict section that binds the argument
  and an `on $VAR`-scoped partial section (§6). No registry, no
  mode-switching matcher — section composition does the job today.
- **Single-section composition syntax: deferred until evidence.** The
  one-pattern `foo({a: $x})` sugar is the only part still unbuilt, and
  it is genuinely optional given multi-section: build it when the
  ergonomics are concretely wanted.
- **The single-section form is rejected, not silent.** A partial pattern
  that is not a bracketed container raises at compile time with a message
  pointing at the multi-section idiom — more informative than the earlier
  zero-matches behaviour, and it can't be mistaken for "matched nothing in
  this source."
- **Kotlin named-argument partial: out of scope for now.** Major
  tools (Semgrep, ast-grep, comby) don't expose this either; the
  idiomatic answer to "call with at least these args" is the ellipsis
  (`foo(...)`), already supported as a sequence wildcard in strict
  mode. If the registry is later built, Kotlin named-arg partial comes
  with it cheaply.

## 9. Connection to field mode

Field mode ([field-mode.md](field-mode.md)) and partial mode were
designed expecting to share a per-language registry. In the end neither
implemented mode uses one: partial mode derives container delimiters from
the source structure (§3), and field mode aligns the pattern to the
declaration's children without optionality data
([field-mode.md §5](field-mode.md)). The "skip a child the pattern
doesn't address" idea behind field mode and the "set-match the middle"
idea behind partial mode are close cousins — both consume a node's
children against pattern segments — but each got there structurally
rather than from a registry.

The registry survives only as hypothetical support for the two deferred
partial-mode features in §7 (single-section composition sugar, Kotlin
named-argument partial). If one of those is ever built, §7 records the
entries it would need.
