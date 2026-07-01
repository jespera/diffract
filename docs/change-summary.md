# Change summaries: the `summarize` subcommand

`summarize` answers the question a reviewer of a large, systematic changeset
actually has: *what was done here, as a rule — and what else happened?*
Given a before/after pair of directory trees, it infers the spatch rules
behind the changeset:

```
$ diffract summarize -l typescript -i '*.ts' before/ after/
```

The output is a set of **rules** (diffract patterns, each with the files it
applies to) plus **residuals** (per-file diffs of whatever the rules don't
explain). Instead of a thousand-line diff repeating the same edit, a reviewer
reads one rule once, checks the mechanical part is right, and then inspects
only the residuals — the places where something *else* went on.

Two properties make the output trustworthy rather than approximate:

- **Reconstruction.** Applying each file's claiming rules (in rule-id order)
  and then its residual reproduces the after-state exactly. Nothing is lost;
  the summary is the diff, factored.
- **Never mis-state (the safety property).** A rule only claims a file when
  applying it there moves the source *toward* the after-state without doing
  anything that would have to be undone — formally, the rule's output stays
  on a shortest edit path between before and after (the "geodesic", design
  doc §2.3). A site where the change merely *looks* similar but differs in
  content is left to its residual rather than claimed wrongly. The verified
  per-file scope is the point: a rule's `sites` list is the set of files
  where applying it is safe, not everywhere its pattern happens to match.

## Invocation

```
diffract summarize -l LANG -i GLOB [-e DIR]... [-v] BEFORE_DIR AFTER_DIR
```

`-l`/`--language` and `-i`/`--include` are both **required**: `summarize` walks
directories, so it must be told which grammar to use and which files to scan
rather than silently parsing every walked file with a default grammar. Run it
once per language/extension set (e.g. `-l kotlin -i '*.kt'`, then
`-l tsx -i '*.tsx'`).

| Flag | Meaning |
|------|---------|
| `-l`, `--language` | **Required.** Grammar for files whose extension isn't auto-detected; `.ts`/`.tsx` are always auto-detected by extension |
| `-i`, `--include` | **Required.** Glob for files to scan (e.g. `'*.kt'`) |
| `-e`, `--exclude` | Directory names to skip (repeatable; sensible defaults) |
| `-v` | Progress and phase timing on stderr |
| `--ignore-formatting` | Treat formatting as invisible in the residuals (see below) |

## Output format

```
# rule R1  support=53  language=kotlin          ← rule header
@@
match: strict
metavar _H0: single                              ← inferred metavariables
@@
- oldName(_H0)                                   ← a normal diffract pattern
+ newName(_H0)
# sites R1                                       ← files where R1 is safe
src/a.kt
src/b.kt

# rule R2  support=2  language=kotlin  after=R1  ← tier-2 rule: apply after R1
...

# residual  rule=R1                              ← what R1 leaves at one site
--- a/src/a.kt
+++ b/src/a.kt
@@ ... @@
-...
+...

# residual                                       ← change no rule claims
...
```

- `support` is the number of times the rule fires across its sites — counted
  in the *applied chain*, not against the original source: if an earlier rule
  consumes a later rule's matches at some file, that file is not listed in
  the later rule's sites and contributes nothing to its support.
- Metavariables are rendered `_H0, _H1, …` (valid identifiers in every
  supported grammar; diffract metavars are sigil-free).
- `after=R1` marks a **tiered** rule: its pattern matches the intermediate
  state produced by applying R1, so application order is rule-id order.
  When a tiered rule follows different rules at different sites, the
  annotation moves onto the individual site lines.
- A residual's `rule=` list names the rules applied before the gap was
  measured; a residual with no `rule=` is a pure one-off change (or a
  file-level add/delete, shown against `/dev/null`).
- Layout-only changes never appear: a residual hunk is emitted only when
  it touches a change the *parse tree* can see, so re-indentation,
  spacing tweaks (`{ }` vs `{}`), and line splits are dropped — a file
  whose entire leftover is layout emits no residual at all. (The summary's
  reconstruction guarantee is modulo layout throughout.)

### `--ignore-formatting`

The layout filter above only drops changes the parse tree can't see —
pure whitespace. A formatter (ktlint, prettier, gofmt) does more than
re-indent, though: when it re-wraps a list it adds a **trailing separator**
(a trailing comma, a redundant semicolon), and that *is* a real node, so
the reflow survives as a noisy residual even when a rule already explains
the semantic change. `--ignore-formatting` extends the filter to treat
those trailing separators as trivia too: a residual hunk that is only
re-indentation plus a trailing separator is dropped, so the residuals show
just what changed *semantically*.

It is deliberately conservative and sound: it drops a whole-node
replacement only when its two sides are equal **modulo separators** as
*trees* (not as text), so a newline-sensitive change like `return\nx` vs
`return x` — which differs in statement structure — is still reported, and
a genuinely structural change such as an inserted brace block
(`if (c) g()` → `if (c) { g() }`) is kept. It affects the residuals only;
the rules are unchanged. Off by default. Useful when the after-state was
run through a formatter and you want the residuals to reflect intent rather
than reformatting.

## Worked examples

All of these are test fixtures under `tests/change_summary_cases/` — the
outputs below are the pinned expected results.

### A systematic edit with look-alikes left alone

Three files rename a call and drop its second argument; each file *also*
contains a different call to the same function that was not changed:

```
a.ts  before:  const ok = foo(alpha, beta);    const z = foo(p, q, r);
      after:   const ok = bar(alpha);          const z = foo(p, q, r);
b.ts  before:  const ok = foo(gamma, delta);   const z = foo(p, q, r);
      after:   const ok = bar(gamma);          const z = foo(p, q, r);
c.ts  …
```

```
# rule R1  support=3  language=typescript
@@
match: strict
metavar _H0: single
metavar _H1: single
@@
- foo(_H0, _H1)
+ bar(_H0)
# sites R1
a.ts
b.ts
c.ts
```

The two-argument shape distinguishes the changed calls, so the rule states
the change exactly and the untouched `foo(p, q, r)` calls don't trip it. No
residuals: the rule explains the whole changeset.
(Fixture: `ts_arg_drop_confounded`.)

### A site that did a bit more: rules + residuals

Same rule, but one site's first argument was simplified at the same time
(`x + 1` became `x`):

```
a.ts  before:  const ok = foo(x + 1, a);     after:  const ok = bar(x);
b.ts  before:  const ok = foo(p, q);         after:  const ok = bar(p);
c.ts  before:  const ok = foo(m, n);         after:  const ok = bar(m);
```

```
# rule R1  support=3  language=typescript
@@
match: strict
metavar _H0: single
metavar _H1: single
@@
- foo(_H0, _H1)
+ bar(_H0)
# sites R1
a.ts
b.ts
c.ts

# residual  rule=R1
--- a/a.ts
+++ b/a.ts
@@ ... @@
-const ok = bar(x + 1);
+const ok = bar(x);
```

`a.ts` is still claimed — applying R1 there is safe progress — and the
extra simplification is stated honestly as a residual *against the
intermediate* (`bar(x + 1)`, i.e. after R1 has been applied). This is the
"decomposable site" case: the gap must be a pure insertion or deletion on
top of what the rule wrote (checked by ordered tree inclusion), and smaller
than what the rule explains — otherwise the site is not claimed.
(Fixture: `ts_arg_drop_residual`.)

### Refusing to over-claim

Three files unwrap `box(...).get()` down to the wrapped value — but in one
of them the value was *also renamed*:

```
a.ts  before:  const r = box(old).get();     after:  const r = new1;
b.ts  before:  const r = box(p).get();       after:  const r = p;
c.ts  before:  const r = box(m).get();       after:  const r = m;
```

```
# rule R1  support=2  language=typescript
@@
match: strict
metavar _H0: single
@@
- box(_H0).get()
+ _H0
# sites R1
b.ts
c.ts

# residual
--- a/a.ts
+++ b/a.ts
@@ ... @@
-const r = box(old).get();
+const r = new1;
```

The unwrap rule claims only the sites where the value is genuinely
preserved. At `a.ts`, applying it would write `old` where the change wrote
`new1` — work that would have to be undone — so the site is refused and its
whole change is reported as a residual. The summary never asserts a rule
did something it didn't. (Fixture: `ts_unwrap_rename_confound`.)

### Tiered rules: factoring the change

When the leftover gaps are themselves systematic, they are re-clustered
into second-tier rules (recursively, until nothing systematic remains):

```
a.ts  before:  const r = f(x + 1, p);    after:  const r = g(x);
b.ts  before:  const r = f(m, q);        after:  const r = g(m);
c.ts  before:  const r = f(n, w);        after:  const r = g(n);
d.ts  before:  const r = f(y + 1, s);    after:  const r = g(y);
```

```
# rule R1  support=4  language=typescript
@@
match: strict
metavar _H0: single
metavar _H1: single
@@
- f(_H0, _H1)
+ g(_H0)
# sites R1
a.ts
b.ts
c.ts
d.ts

# rule R2  support=2  language=typescript  after=R1
@@
match: strict
metavar _H0: single
@@
- (_H0 + 1)
+ (_H0)
# sites R2
a.ts
d.ts
```

The primary rule covers all four files; the `+ 1`-dropping that two of them
additionally did becomes its own rule, applied **after** R1 (its pattern
matches the intermediate `g(x + 1)`). Each site's change factors as
primary ∘ secondary ∘ residual, every tier individually safe — here with no
residual at all. (Fixture: `ts_arg_drop_tiered`.)

## How it works, briefly

For every changed file, `summarize` computes an AST-level diff
(`Tree_diff`), extracts change pairs at multiple granularities, and
anti-unifies them across files into candidate patterns (hierarchical
clustering). Candidates are then **evaluated**: each is applied to every
changed file and kept only where a per-site safety gate verifies it — its
edits land in changed regions, its output parses, and it reproduces the
change it claims (or leaves a pure, smaller, insert-or-delete gap), never
writing content that would need to be reverted. A greedy set-cover
**selects** the final rule set; residuals are measured against what the
selected rules actually produce, which is what makes the reconstruction
property hold by construction. The full design — the safety property, the
propose/evaluate/select pipeline, the geodesic gate, coarsening, tiers — is
in [change-summary-design.md](change-summary-design.md), and the papers the
machinery draws on are catalogued in [references.md](references.md).
