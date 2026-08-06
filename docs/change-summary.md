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
diffract summarize -l LANG [-i GLOB] [-v] --pairs MANIFEST
```

`-l`/`--language` names the grammar and is always **required**; every file the
run sees is parsed with it, so run once per language (e.g. `-l kotlin -i
'*.kt'`, then `-l tsx -i '*.tsx'`).

With two directories, `-i`/`--include` is also **required** — `summarize` walks
them, so it must be told which files to scan rather than silently parsing
everything it finds. With `--pairs` the manifest already enumerates the
changeset, so `-i` becomes an optional filter (handy for pointing several
per-language runs at one checkout).

Prefer `--pairs` when the change involves **renames**: see
[Renamed and moved files](#renamed-and-moved-files).

| Flag | Meaning |
|------|---------|
| `-l`, `--language` | **Required.** Grammar used for every file `--include` selects |
| `-i`, `--include` | **Required** with two directories; optional filter with `--pairs`. Glob for files to scan (e.g. `'*.kt'`) |
| `--pairs` | Read the changeset from a change-pair manifest instead of pairing two directories by path — the input that can express a rename |
| `-e`, `--exclude` | Directory names to skip (repeatable; sensible defaults) |
| `-v` | Progress and phase timing on stderr |
| `--ignore-formatting` | Treat formatting as invisible in the residuals (see below) |
| `--format text\|text-minimal\|json` | Output format (default `text`). `text-minimal` is a reading mode: rule site lists collapse to a file count, and residuals are digested into a rename table plus grouped repeated edits. `json` emits one object for filtering with `jq` (see [JSON output](#json-output)) |

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
@@ -12,3 +12,2 @@
-...
+...

# residual                                       ← change no rule claims
...

# residual  rule=R1  unparsed=27-43               ← gap in a region that didn't parse
...

# parse-errors  files=62  residuals-affected=39  ← footer, only when there are any
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

### Parse errors

A rule matches structure, so it cannot match a region the grammar failed to
parse — the structure it would align to isn't there. A change inside such a
region falls to a residual no matter how systematic it is, which looks
identical to a factoring failure unless the summary says otherwise. So it
does:

- `unparsed=27-43` on a residual header means that file has regions the
  grammar could not read, with the line ranges (1-based, inclusive, in the
  same coordinates as the hunk headers below it). If the hunk sits in one of
  those ranges, that — not rule discovery — is why it is a residual. Long
  lists abbreviate (`,+59 more`).
- The `# parse-errors` footer counts the affected files and how many
  residuals they account for. It counts files whose changes rules explain
  completely too: if a fifth of a corpus does not parse, that is worth
  knowing even where it cost nothing.

Both are omitted entirely when everything parses, so a clean corpus's
summary is unchanged. Deliberately terse: one footer line and a suffix on
headers that already exist. `--format json` carries the full per-file
ranges. The line ranges are *regions*, not raw `ERROR` nodes — tree-sitter
nests many error nodes inside one garbled span, so a count of those would
read as catastrophe where one declaration is unreadable.

`apply` and `search` report the same thing as a single count line (with
`--verbose` to list the files), and `parse` as a region summary above its
per-node error list.

Grammar coverage is the limit here, not summarize: the fix for a corpus with
many unparsed regions is a better grammar for that language. What the report
buys is knowing that is the situation.

### Renamed and moved files

Two directory trees cannot say that a before-file corresponds to a
*differently-named* after-file. Left to path equality, a renamed file arrives as
an unrelated deletion plus an unrelated addition, and the systematic edits
inside it produce no rules at all.

The effect is total rather than marginal when a codemod moves everything it
touches. On one module of Apache Pekko's `akka` → `org.apache.pekko` rename
(131 Scala files, all of them moved — `evaluation/pekko.sh`), path pairing
yields **zero** rules and 262 whole-file `/dev/null` residuals; the same
changeset through a manifest yields 3 rules covering 1,029 edits, and the
residuals shrink to the 131 real ones.

So the pairing is supplied as input. `scripts/diffract-checkout.sh` writes a
manifest next to the trees it extracts:

```
scripts/diffract-checkout.sh -M 40 HEAD~1 HEAD /tmp/cs -- '*.kt'
diffract summarize --pairs /tmp/cs/pairs.tsv -l kotlin
```

The manifest is tab-separated, one record per changed file. A leading keyword
gives each record's arity, so a reader never has to infer how many paths
follow; paths are logical, with field 1 read from `before/` and field 2 from
`after/`, and `#` lines are comments:

```
pair	src/old/Thing.kt	src/new/Thing.kt    renamed — the paths simply differ
pair	src/Widget.kt	src/Widget.kt           modified in place
add	src/New.kt
del	src/Gone.kt
```

There is no separate "rename" marker: a `pair` whose two paths differ *is* a
rename. Anything unrecognised is an error rather than a skipped line, since a
manifest that quietly dropped records would read as a codemod with fewer sites
rather than as a broken input.

**Renames appear in the residuals, not in `sites`.** A rule explains a content
transformation matched against a syntax tree; a move is a file-level operation
that no pattern can express, so — exactly like additions and deletions, which
have always surfaced as `/dev/null` residuals rather than rules — it belongs to
the residual. `sites` names the *before* path, which is the one present in the
tree you apply to.

A residual for a moved file carries git's extended header:

```
# residual  rule=R1
diff --git a/src/old/Thing.kt b/src/new/Thing.kt
rename from src/old/Thing.kt
rename to src/new/Thing.kt
--- a/src/old/Thing.kt
+++ b/src/new/Thing.kt
@@ -3,1 +3,1 @@
-...
```

which is what lets `git apply` perform the **move as well as** the content
change. A file that moved without being edited still gets a residual — a
`similarity index 100%` header with no hunks — because otherwise the move never
reaches the output and applying the summary would leave the file where it was.

Two bare directories still handle one case unaided: a move with *identical*
content is paired by exact content match, since identical bytes on both sides
are proof enough and need no threshold. An **edited** rename is beyond what two
directories can express and wants `--pairs`.

The manifest is also the place to correct a mispairing. Detection is git's, and
git compares content, so a thoroughly-renamed file scores *lower* — the more
systematic a codemod, the likelier its renames fall below the default 50%.
Pekko's rename is mostly path-shaped and survives it (all 131 found at
`-M50%`), but on a corpus whose renames also rewrote the file contents, `-M50%`
found 13 of 15 and `-M40%` found all 15. When rules come out thin and
`/dev/null` residuals appear in pairs, lower the threshold before suspecting
the pipeline.

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

### Minimal text output

`--format text-minimal` is the `.summary` text with each rule's site list
collapsed to a one-line count:

```
# rule R1  support=8  language=kotlin
@@
...
# sites R1  8 file(s)
```

Use it when reading the rules is the point and the per-file scope would be
noise; re-run with the default format (or use `--format json` and `jq`) to
drill into where a rule applies. Mixed per-site `after=` annotations are
elided with the file lines (a uniform `after=` still shows in the rule
header), so the full `text` format remains the canonical, lossless one.

It does the same for the other half of the output, which is usually the
larger one — on a rename-heavy corpus the residuals are over 99% of the bytes,
and most of *those* are the git/path header repeating a long path four to six
times per file. So residuals are digested three ways:

- **Moved files** collapse into a `# renames` section keyed by the *path* edit,
  so a systematic move states itself once.
- **Repeated hunks** are grouped by their edit signature under
  `# residual-groups`, each with a count, a file count and one real example.
- **Everything a group doesn't cover** is printed in full, under a one-line
  header instead of six.

```
# renames  131 file(s)  1 path edit(s)
  x131  akka/ -> org/apache/pekko/
      e.g.  akka-cluster/src/main/scala/akka/cluster/Cluster.scala
         -> akka-cluster/src/main/scala/org/apache/pekko/cluster/Cluster.scala

# residual-groups  95 of 97 hunk(s), 5 group(s)
  x58  35 file(s)  akka. -> org.apache.pekko.
  -    if (settings.DowningProviderClassName == "akka.cluster.AutoDowning" ||
  +    if (settings.DowningProviderClassName == "org.apache.pekko.cluster.AutoDowning" ||
  x24  9 file(s)  akka -> pekko
  - * Each cluster [[Member]] is identified by its [[akka.actor.Address]], and
  + * Each cluster [[Member]] is identified by its [[pekko.actor.Address]], and
```

Every hunk appears exactly once — inside a group or printed in full — so the
counts add up and nothing is silently dropped. Two edits group when their
*word-level* difference matches, ignoring the code around them.

Words are runs of identifier characters, and are **not** split at case humps or
underscores. So a rename *inside* an identifier (`FooLibraryService` →
`FooService`) is reported literally and does not group with the same conceptual
rename in another name (`BarLibraryModule` → `BarModule`). Splitting there
would group them, but only by encoding naming conventions the grammar does not
know — camel, acronym tails, snake, kebab — with no natural stopping point.

The exemplar is not decoration. The same rename often has several renderings —
pekko writes `akka` as `org/apache/pekko` in paths, `org.apache.pekko` in
quoted class names and `pekko` in doc comments — and the edits alone read as
contradictions until you see a line of each.

This is a **description, not a rule**: a group says an edit recurred, not that
it is safe to apply. Groups routinely cover changes no rule could — comments
are tree-sitter extras the matcher never visits, and pure insertions have no
anchor to match on. The canonical `text` format is unaffected and stays
byte-identical, which is what `git apply` and the round-trip test consume.

### JSON output

`--format json` emits the same data as the `.summary` text — one compact
JSON object — so any projection of it is a `jq` filter away. The `.summary`
text remains the default and the canonical format; JSON is for tooling and
selective reading. The shape:

```json
{
  "rules": [
    { "id": "R1", "support": 8, "language": "kotlin",
      "pattern": "@@\nmatch: strict\n@@\n- android\n+ androidx\n",
      "sites": [ { "file": "c.kt" },
                 { "file": "a.kt", "after": ["R2"] } ] }
  ],
  "residuals": [
    { "file": "z.kt", "rules": ["R1"], "diff": "--- a/z.kt\n..." },
    { "file": "old/y.kt", "moved_to": "new/y.kt", "rules": [],
      "diff": "diff --git a/old/y.kt b/new/y.kt\n..." }
  ]
}
```

A residual's optional `moved_to` names the after-side path when the file was
renamed; `file` is always the before-side path (see
[Renamed and moved files](#renamed-and-moved-files)).

A site's optional `after` array is the per-site tier attribution (the
earlier rule ids whose output this rule's pattern matched there — the
`after=` annotations of the text format). Useful projections:

```bash
# Rules only, without the site lists:
diffract summarize ... --format json |
  jq -r '.rules[] | "# rule \(.id)  support=\(.support)  files=\(.sites|length)\n\(.pattern)"'

# Residuals only (what no rule explains):
diffract summarize ... --format json | jq -r '.residuals[].diff'

# Everything touching one file:
diffract summarize ... --format json |
  jq --arg f "src/App.kt" '.rules[] | select(.sites[].file == $f)'
```

The enum leaves room for further formats later (e.g. SARIF for
code-review integration).

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
@@ -1,1 +1,1 @@
-const ok = bar(x + 1);
+const ok = bar(x);
```

`a.ts` is still claimed — applying R1 there is safe progress — and the
extra simplification is stated honestly as a residual *against the
intermediate* (`bar(x + 1)`, i.e. after R1 has been applied). This is the
"decomposable site" case: the rule's output must lie on a shortest edit
path between before and after (the geodesic safety property, measured on
the token stream), and the gap it leaves must be smaller than what the
rule explains — otherwise the site is not claimed.
(Fixture: `ts_arg_drop_residual`.)

### An honest partial step inside one change

Three files unwrap `box(...).get()` down to the wrapped value — but in one
of them the value was *also renamed*:

```
a.ts  before:  const r = box(old).get();     after:  const r = new1;
b.ts  before:  const r = box(p).get();       after:  const r = p;
c.ts  before:  const r = box(m).get();       after:  const r = m;
```

```
# rule R1  support=3  language=typescript
@@
match: strict
metavar _H0: single
@@
- box(_H0).get()
+ _H0
# sites R1
a.ts
b.ts
c.ts

# residual  rule=R1
--- a/a.ts
+++ b/a.ts
@@ -1,1 +1,1 @@
-const r = old;
+const r = new1;
```

At `a.ts` the change composes two steps: the unwrap, which R1 states, and
a rename `old → new1` that no other file shares. Applying R1 there writes
`old` — an intermediate strictly *between* the before- and after-states
(nothing about it must be undone; the rename still remains). So the site
is claimed, and the rename is stated honestly as an attributed residual
against the intermediate. The gate refuses a site instead when the rule's
output strays off the path — writing content that appears in neither
endpoint — or when the gap it leaves is no smaller than the change it
claims to explain; such a site's whole change falls to an unattributed
residual. (Fixture: `ts_unwrap_rename_confound`.)

### Overlapping variants: the specific rule wins its sites

When one systematic change is a more elaborate variant of another, both
are emitted as flat rules with disjoint sites — the more specific pattern
applies first (rule ids are application order, assigned
specificity-first), so it takes the sites it fully explains and the
general rule keeps the rest:

```
a.ts  before:  const r = f(x + 1, p);    after:  const r = g(x);
b.ts  before:  const r = f(m, q);        after:  const r = g(m);
c.ts  before:  const r = f(n, w);        after:  const r = g(n);
d.ts  before:  const r = f(y + 1, s);    after:  const r = g(y);
```

```
# rule R1  support=2  language=typescript
@@
match: strict
metavar _H0: single
metavar _H1: single
@@
- f(_H0 + 1, _H1)
+ g(_H0)
# sites R1
a.ts
d.ts

# rule R2  support=2  language=typescript
@@
match: strict
metavar _H0: single
metavar _H1: single
@@
- f(_H0, _H1)
+ g(_H0)
# sites R2
b.ts
c.ts
```

No residual, no ordering subtlety a reviewer has to track: each file's
change is one rule. (Fixture: `ts_arg_drop_tiered`. Broad-rule-first
ordering would instead have R2 fire everywhere and re-derive the `+ 1`
handling as a second-tier rule against its own intermediate.)

When the leftover gaps are systematic but **not** expressible as a flat
rule on the before-state — the secondary change only makes sense against
the intermediate an earlier rule produces — they re-cluster into
second-tier rules (recursively, until nothing systematic remains). A
tier-2 rule carries `after=`: its pattern matched the intermediate the
listed rules produce, so rule-id order is application order, and each
site's change factors as primary ∘ secondary ∘ residual, every tier
individually safe. (Fixture: `tsx_memo_tiered_deps`.)

### A rewrite that changes the node type

A Kotlin codemod turns annotated, parameterless test-suite classes into
objects. This flips the declaration's tree-sitter node type outright
(`class_declaration` → `object_declaration`) — the same shape as TS
`var` → `let` or `interface` → `type` — and the whole class body rides
along unchanged:

```
a.kt  @Suite class AlphaSuite { … }         → object     converted
b.kt  @Suite class BetaSuite { … }          → object     converted
      @Suite(includes = [BetaSuite::class])
      class BetaNightlySuite { … }          → unchanged  argumented annotation
c.kt  @Suite class GammaSuite { … }         → object     converted — but see the residual
      @Suite class KeeperSuite {
          companion object { … } … }        → unchanged  objects can't declare companions
```

```
# rule R1  support=2  language=kotlin
@@
match: strict
metavar _H0: single
@@
  @Suite
- class _H0 {
+ object _H0 {
  ...
  }
# sites R1
a.kt
b.kt

# residual
--- a/c.kt
+++ b/c.kt
@@ -6,1 +6,1 @@
-class GammaSuite {
+object GammaSuite {
```

Three things worth noticing. The `{` sitting directly after the name is
the rule's *precondition*, not decoration: constructor parameters and
supertype calls would appear between the name and the brace, and both
make class→object invalid Kotlin — so two lines of concrete syntax state
"parameterless, no supertype" exactly. Second, `BetaNightlySuite` is
untouched without any special handling: the pattern addresses the
annotation, so `@Suite(includes = [...])` — structurally a different
annotation — simply doesn't match. Third, `c.kt` falls to a residual
rather than being claimed, even though `GammaSuite`'s change is exactly
rule-shaped: a rule applies wherever it matches, and `KeeperSuite`
matches too — but an `object` cannot declare a `companion object`, so
the original change correctly skipped it, and applying the rule there
would be wrong. The gate refuses the file rather than claim a site it
can only reach by also doing damage; the residual is precisely the
reviewer's "this file deviates from the pattern" queue. (Fixture:
`kotlin_class_to_object`.)

The same rule can be hand-written as an unbalanced prefix — the
tokenizer matches leaf runs, so the pattern does not need to be a
complete syntactic construct:

```
@@
match: strict
metavar C: single
@@
  @Suite
- class C {
+ object C {
```

And had the codemod also converted the `@Suite(includes = [...])`
classes, that variant is one more rule away — an inline `...` in the
annotation's argument list binds the arguments as context:

```
@@
match: strict
metavar C: single
@@
  @Suite(...)
- class C {
+ object C {
  ...
  }
```

The two rules partition the cases (a bare annotation and a parenthesised
one are structurally distinct, so neither rule fires on the other's
sites), and both keep the brace-adjacency precondition.

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
