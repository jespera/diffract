# Change Summary: Design Document

Status: design, pre-implementation. Supersedes the rough notes in
`change-summary.md` for the purposes of guiding the build. Keep this document
up to date as decisions change.

## 1. Motivation

A changeset (a pull request, merge request, or patch series) often contains
many source-level changes that share a systematic structure — an API rename, a
library swap, a data-shape migration. When such a change spans dozens or
hundreds of sites, the standard line-oriented diff forces a reviewer to
re-verify the same mechanical rewrite over and over. Anything *non*-mechanical
hiding in the same PR (a bug fix, a subtle local tweak) gets lost in the noise.

Diffract already has the pieces to do better:

- A GumTree-inspired structural diff (`Tree_diff`) that maps nodes between
  before/after trees per file.
- A spatch-style pattern DSL with metavariables, expansion lines, and
  conjunctive multi-section rules (`Match`).
- A working prototype at `examples/change_summary.ml` implementing Getafix-
  style agglomerative clustering of anti-unified edit patterns.

The feature described here turns that pipeline into a first-class library
surface and CLI subcommand that, given a changeset, emits a **change summary**:
a small set of abstract rewrite rules describing the systematic part of the
change, plus a residual set describing everything else. The output is itself a
diffract artefact — the rules re-parse as patterns and re-apply via
`Match.transform_nested`, so the summary is both a human review aid and a
mechanical reproduction of the refactor.

## 2. Specification

### 2.1 Inputs

A **changeset** is a set of file operations:

- **Modified**: `(path, before_source, after_source)`.
- **Added**: `(path, after_source)`.
- **Deleted**: `(path, before_source)`.

Each source is parsed with a language-appropriate tree-sitter grammar into a
`Tree.src tree`.

### 2.2 Outputs

A **change summary** is the triple `(rules, file_ops, residuals)`:

- `rules` — a list of `rule` records, each carrying:
  - `pattern`: a diffract pattern (possibly multi-section / conjunctive).
  - `occurrences`: the list of `(file, location)` sites the rule fires at.
  - `residuals`: per-site residual changes not covered by this rule (see §4.4).
  - `children`: sub-rules (from hierarchy, see §4.1) — each itself a rule.
- `file_ops` — new and deleted files, reported by name.
- `residuals` — change pairs covered by no rule, each with its before/after
  text and a reference to the originating file/line.

### 2.3 Desiderata (adapted from spdiff)

Borrowed from Andersen & Lawall's *Generic Patch Inference* (§IV), adapted to
be practical rather than optimal:

- **Covering**. Every change pair extracted from the changeset is accounted
  for, either by a rule occurrence or in `residuals`.
- **Safe by decomposition**. For every `(site, rule)` association, the pair
  `(rule, residual_at_site)` reproduces the site's after-source *exactly*.
  Safety is structural, not probabilistic. See §4.4.
- **Concrete**. Every emitted rule contains at least one named AST node that
  is not a metavariable. Pure `$X ⤳ $Y` rules are useless and excluded.
- **Coherent**. For each rule the fraction of holes in the pattern is below a
  tunable threshold (default 0.35, measured on the pattern shell, not inside
  held-out sub-patterns). See §4.1.
- **Supported**. Each rule fires at `≥ m` sites (default 2). Singletons go to
  `residuals`.

We **do not** promise the spdiff LCP(C) — finding the literal largest common
safe patch is computationally brittle on real code. Instead we commit to a
Getafix-style hierarchical output derived by greedy clustering, with a
principled merge order, and we supplement it with three additions that handle
cases pure anti-unification cannot: cross-side alignment, residual extraction,
and conjunctive rule fusion.

## 3. Architecture: the pipeline

```
changeset
   │
   ├──▶ file operations  ──────────────────────────────────────┐
   │                                                            │
   └──▶ per-file Tree_diff ──▶ change-pair extraction ──┐       │
                               (Changed + Added/Removed) │      │
                                                         ▼      │
                               singleton clusters (one per pair)│
                                                         │      │
                                                         ▼      │
                 anti-unification clustering (Getafix) ──┤      │
                 with greedy hole-fraction-min merges    │      │
                                                         ▼      │
                               dendrogram of rules ──────┤      │
                                                         │      │
                                                         ▼      │
                 cross-side hole alignment via GumTree ──┤      │
                               (§4.3)                    │      │
                                                         ▼      │
                 residual extraction + recursive ────────┤      │
                 clustering on residuals (§4.4)          │      │
                                                         ▼      │
                 cross-cluster co-occurrence merging ────┤      │
                 into conjunctive rules (§4.2)           │      │
                                                         ▼      │
                                                      rules     │
                                                         │      │
                                                         ▼      ▼
                                                       change summary
```

Each stage is a pure function from the previous stage's output. This matters
for testing: each stage can be exercised in isolation with fixed inputs.

### 3.1 Multi-level change-pair extraction with site covering

The change-pair extractor emits a change pair at **every** `Modified`
ancestor along each change chain (plus the terminating `Replaced` leaf),
not just one chosen by a grammar-shape heuristic. Clustering proceeds on
this expanded candidate set; a greedy site-covering pass then picks which
level of ancestor wins for each site. Key properties:

- **Grammar-agnostic.** No depth constants, no per-language node-type
  whitelists. The same emission rule handles TypeScript's
  `call_expression → arguments` and Kotlin's longer
  `call_expression → call_suffix → value_arguments → value_argument` chain
  uniformly: every wrapper level is a candidate, and the covering pass
  chooses by evidence (support, shape, concreteness) rather than by
  hard-coded depth.
- **Pre-filter by change density.** An ancestor is emitted only when the
  fraction of its named children that are non-`Same` exceeds a small
  threshold. This drops ancestors that are mostly unchanged (a whole
  function body when only one statement changed) without a depth bound.
- **Covering by byte-range overlap.** Clusters are ranked by support (desc),
  then by asymmetric-shape-first (patterns with arg-count or structural
  diffs between `-` and `+` carry strictly more information than same-shape
  renames), then by concrete-node-count (asc — a small well-scoped leaf
  rename beats its enclosing call level when both are symmetric), then by
  hole fraction, then by pattern text. The winning cluster claims the byte
  ranges of its sites; subsequent clusters whose instances overlap a claim
  in the same file are dropped. Disjoint sites at the same call
  (e.g. independent receiver and method renames) do not overlap and can
  each win their own rule, preserving the two-leaf-rule output for cases
  like [api_swap].
- **Coherence gate.** A cluster survives only if its pattern contains at
  least one concrete named leaf on each side and has at least one concrete
  edit (either differing multisets of leaf values or differing structural
  shape modulo holes). All-hole scaffolding like `$.$($)→$.$($)` is
  rejected here before it can compete for coverage. The gate also
  rejects clusters whose `+`-side has a metavariable not present on the
  `-` side (M1.8a).
- **Leaf-on-delimiter conversion.** During `of_src`, a named node is
  treated as a leaf (its full text kept verbatim, no recursion) when
  its byte range contains non-whitespace bytes that no child covers
  (silently-consumed delimiters such as Kotlin's `string_literal`
  quotes) or when an unnamed child's text is a string-quote character
  (TypeScript's `string` exposes `"` as unnamed children). Anti-
  unification then holes the whole literal when its content varies,
  rather than holing inside the delimiters and rendering an
  unapplicable placeholder embedded in a string token. Slash is
  excluded from the quote-char set so `binary_expression` with `/` is
  not misclassified.

## 4. Key design decisions

### 4.1 Hierarchical clustering (Getafix)

Following Bader et al., *Getafix* (§4.2): start with one singleton cluster per
change pair, greedily merge the pair of clusters whose anti-unification
introduces the fewest new holes (measured as hole fraction relative to pattern
size), and record the merge tree. The result is a dendrogram whose leaves are
concrete edits and whose interior nodes are increasingly abstract patterns.

**Cut policy.** By default, emit the coarsest-still-coherent node from each
subtree: the rule is still informative (hole fraction below threshold, has
concrete structure) *and* covers as many sites as possible. Expose the full
dendrogram in the summary under a `children` field so that a reviewer can
drill from a general rule into its specializations.

**Coherence on shell, not whole tree.** If a pattern contains a held-out
sub-pattern `$BODY` whose value is an arbitrary function body, measuring hole
fraction over the entire tree penalises the cluster for the body's necessary
variability. Measure coherence on the *shell* — the part of the tree outside
the metavariables — so that rules with large held-out bodies remain coherent.

**Merge criterion.** Greedy minimum hole fraction is the workable default. A
followup that mirrors Getafix's tiebreaker cascade (prefer merges that keep
bound-holes bound, preserve more label mappings, preserve error context) is
possible but not required for the first cut. Order of merges affects which
dendrogram shape emerges but not which sites end up covered, because we apply
cross-side alignment and residual extraction afterwards.

### 4.2 Conjunctive rules for correlated changes

The pattern DSL already supports conjunctive sibling sections (see
`docs/patterns.md` and `useAppSelector.pat`): a multi-section pattern where
every section must match somewhere in the file for any transform to apply.
This is the natural emit shape for refactors like "remove redux" where an
import change and a call-site rewrite are semantically one operation.

**Mechanism.** After independent clustering produces clusters
`{A: pattern_A, files_A; B: pattern_B, files_B; ...}`, compute Jaccard overlap
between every pair's file sets: `J(A, B) = |files_A ∩ files_B| / |files_A ∪ files_B|`.
Cluster pairs with `J > τ` (default ≈ 0.7) are candidates for fusion.

**Fusion covers three cases.** All three use the same Jaccard criterion;
they differ in shape of inputs and output:

1. *One-sided + one-sided → single two-sided section.* A Removed cluster
   `C_R` (match-only) and an Added cluster `C_A` (replace-only) fuse into
   a single section `- r_body / + a_body`. Generalises to N removals + M
   additions sharing the same file set → one section with N `-` lines and
   M `+` lines (handles "one import becomes two"). This is how raw
   tree-diff Removed/Added entries become swap rules.
2. *One-sided + two-sided → two-sided conjunctive rule.* A one-sided
   candidate fuses with a two-sided cluster whose file set coincides; the
   two-sided cluster supplies the match anchor for the fused rule, while
   the one-sided candidate contributes a section whose body is widened
   to two-sided as in case 1 (if it can be paired with another one-sided
   candidate from the same file set) or, failing that, is not included.
3. *Two-sided + two-sided → conjunctive multi-section rule.* The original
   story: two independent rewrite rules that consistently co-occur are
   emitted as a single multi-`@@` rule (`useAppSelector.pat` style).

The fused rule's occurrence set is the intersection of the component
clusters' file sets; sites that had one but not all of the components get
routed to residuals via whichever component matched them.

**Safety.** The fused rule is safe iff every section fires in every file in
the intersection and the combined application reproduces the per-file after-
source. This is exactly what `Match.transform_nested` already does; the
summariser just needs to check it.

### 4.3 Cross-side hole alignment via GumTree

Anti-unification memoizes holes on pairs of concrete subtrees. Before- and
after-sides are anti-unified independently; cross-side correspondence emerges
only when the same concrete-pair appears on both sides. This breaks when the
after's subtree is a *part of* (or a reshaping of) one of the before's
subtrees — which is pervasive in realistic refactors (see the synthetic
example in §5.2 and the `const user = useAppSelector(s => s.users.user)` case
in §5.4).

**Fix.** After anti-unification produces before-holes `{$h_i}` and after-holes
`{$h'_j}`:

1. For each file pair, use the GumTree mapping from `Tree_diff.compute_mapping`
   to determine which after-subtrees correspond to which before-subtrees.
2. For each after-hole `$h'_j` with values `{v_j^k}` across sites, find the
   before-hole `$h_i` such that for every site `k`, `v_j^k` is mapped (possibly
   through a sub-position) to a subterm of `$h_i`'s value at site `k`.
3. If a unique such `$h_i` exists, rename `$h'_j := $h_i`.
4. If no such `$h_i` exists, the rule has an orphan metavariable on the `+`
   side — the spatch engine rejects it. Drop the cluster (or, if the
   containing dendrogram node has a coherent parent, fall back to it).

This is pure post-processing on the anti-unified pattern. It reuses the
`Tree_diff` mapping already computed for change-pair extraction.

### 4.4 Residual extraction and recursive decomposition

Cross-side alignment does not guarantee the rule reproduces the after-source
exactly. When it doesn't, the gap is a **residual**: the additional local
change that was applied at that site on top of the common rule.

**Mechanism.** For each site `(t_i, t_i')` in a cluster with rule `p ⤳ p'`:

1. Apply the rule: `intermediate_i = Match.transform(p⤳p', t_i)`.
2. If `intermediate_i = t_i'`, the pair is fully explained.
3. Otherwise, the residual is `Tree_diff.diff(intermediate_i, t_i')`. Attach
   it to the site.

**Recursive clustering on residuals.** Residuals are themselves change pairs.
Running the clustering pipeline on the accumulated residuals produces
secondary rules — common small changes that appear across a subset of the
sites. This process terminates when every remaining residual is a singleton.

This is exactly spdiff's sequential-patch composition `gp_1; gp_2`
constructively computed instead of searched. It turns "safety" from a
yes/no filter into a decomposition property: every site's change equals
(common rule) ∘ (residual), both explicit in the summary.

**Why this is valuable, not a workaround.** A reviewer of a large refactor
has two questions: "Is the mechanical part correct?" (answered by inspecting
the primary rule once) and "Is anything else going on?" (answered by the
residuals). Traditional diffs conflate these. Residual extraction separates
them, and when residuals cluster, surfaces hidden secondary patterns — for
instance a small bug fix riding along with an API migration.

## 5. Synthetic examples

These examples serve both as illustration and as seeds for end-to-end tests.
Each pairs a minimal input with the expected summary output.

### 5.1 Baseline — identical mechanical change

**Input (3 file pairs).**

```
file1.ts:  x = foo(1, 2);    →    x = bar(1);
file2.ts:  y = foo(a, b);    →    y = bar(a);
file3.ts:  z = foo(p(), q);  →    z = bar(p());
```

**Expected `summary` (§9 format).**

```
# rule R1  support=3  language=typescript
@@
match: strict
metavar $X: single
metavar $Y: single
@@
- foo($X, $Y)
+ bar($X)
# sites R1
file1.ts
file2.ts
file3.ts
```

All three sites are fully explained by the rule; no residual sections emitted.

### 5.2 Residual extraction — the `f(x+1, a) → g(x)` example

**Input (2 file pairs).**

```
file1.ts:  f(x+1, a)   →   g(x)
file2.ts:  f(3, 1)     →   g(3)
```

**Anti-unification alone** would emit `- f($h1, $h2) / + g($h3)` with `$h3`
orphaned on the `+` side — rejected by the spatch engine, cluster dissolves.

**With cross-side alignment (§4.3)**: GumTree maps the after's `x` in file1
to the `x` subterm of `x+1` in the before; the after's `3` in file2 maps to
the before's `3`. So `$h3` (with values `{x, 3}`) corresponds to the first
argument across sites — rename `$h3 := $h1`.

**With residual extraction (§4.4)**: applying `f($X, $Y) → g($X)` to
file1's before yields `g(x+1)`; comparing to `g(x)` leaves a residual
`x+1 → x`.

**Expected `summary` (§9 format).**

```
# rule R1  support=2  language=typescript
@@
match: strict
metavar $X: single
metavar $Y: single
@@
- f($X, $Y)
+ g($X)
# sites R1
file1.ts
file2.ts

# residual  rule=R1
--- a/file1.ts
+++ b/file1.ts
@@ -1 +1 @@
-g(x + 1)
+g(x)
```

R1 covers both sites; file1's site retains a `rule=R1`-attributed residual
capturing the `x+1 → x` gap (the diff is against file1's intermediate after
R1 was applied, which is `g(x + 1)`). file2's site is fully explained and
emits no residual.

If a third site had a similar `y+1 → y` residual, M2 recursive clustering
would lift both into a secondary rule with `after=R1`.

### 5.3 Conjunctive rule emission — correlated import + call-site

**Input (2 file pairs, each exhibits both changes).**

```
file1.ts:
  - import { useAppSelector } from "app/hooks";
  + import { useUser } from "app/UserContext";
  ...
  - const user = useAppSelector((s) => s.users.user);
  + const { user } = useUser();

file2.ts:
  - import { useAppSelector } from "app/hooks";
  + import { useUser } from "app/UserContext";
  ...
  - const viewer = useAppSelector((state) => state.users.user);
  + const { user: viewer } = useUser();
```

**Independent clustering** produces two clusters, each firing in both files:

- Cluster A (import swap): 2 sites.
- Cluster B (call-site rewrite): 2 sites.

**Jaccard overlap** `J(A, B) = 2/2 = 1.0`. Above threshold → fuse.

**Expected `summary` (§9 format).**

```
# rule R1  support=2  language=typescript
@@
match: strict
@@
- import { useAppSelector } from "app/hooks";
+ import { useUser } from "app/UserContext";

@@
match: strict
metavar $NAME: single
metavar $PARAM: single
@@
- const $NAME = useAppSelector(($PARAM) => $PARAM.users.user);
+ const { user: $NAME } = useUser();
# sites R1
file1.ts
file2.ts
```

Both sections live under the single `# rule R1` header; the pair of `@@`
blocks in the body is the conjunctive multi-section pattern. The body is
exactly `useAppSelector.pat`, making it a natural golden target for this
case (alpha-equivalent match per §9.2).

### 5.4 Hierarchy — a rule with specialisations

**Input (4 file pairs).**

```
file1.ts:  useAppSelector((s) => s.users.user)         → ...
file2.ts:  useAppSelector((state) => state.users.user) → ...
file3.ts:  useAppSelector((s) => s.users.legalEntity)  → ...
file4.ts:  useAppSelector((s) => s.app.loading)        → ...
```

**Dendrogram** (leaf patterns omitted for brevity):

```
                        useAppSelector(($P) => $P.$X.$Y)    (4 sites)
                          ├── useAppSelector(($P) => $P.users.$Y)    (3 sites)
                          │     ├── useAppSelector(($P) => $P.users.user)         (2 sites)
                          │     └── useAppSelector(($P) => $P.users.legalEntity)  (1 site)
                          └── useAppSelector(($P) => $P.app.loading)              (1 site)
```

**Default cut.** Emit the coarsest coherent rule — a single
`# rule R1  support=4  language=typescript` carrying
`useAppSelector(($P) => $P.$X.$Y)`, followed by a `# sites R1` block
listing all four files. In the M1 summary format (§9.4) only this
top-level rule appears. M4 extends the format to surface the subtree
specialisations for drill-down review; the exact syntax is TBD and this
example will be revisited then.

### 5.5 Pure additions and removals

A rule that is only `+` lines (or only `-` lines) has no structural anchor
for the spatch engine. Conjunctive semantics don't rescue it either: each
section of a conjunctive rule must itself match somewhere, so a
pure-addition section is just as unattachable as a standalone one.

**Replacements vs. pure additions.** The useAppSelector case in §5.3 works
because both sections have a `-` *and* a `+` line — they rewrite
something that was already there. That is an *import swap*, not a pure
addition. Swaps are fine; raw additions are not.

**What happens to pure additions, then.** They become residuals (post-
M1.9). Three files adding the same new import, with nothing correlated to
rewrite, yield three unified-diff hunks:

```
# residual
--- a/file1.tsx
+++ b/file1.tsx
@@ -1,0 +1,1 @@
+import { useCallback } from "react";

# residual
--- a/file2.tsx
+++ b/file2.tsx
@@ -1,0 +1,1 @@
+import { useCallback } from "react";

# residual
--- a/file3.tsx
+++ b/file3.tsx
@@ -1,0 +1,1 @@
+import { useCallback } from "react";
```

Three near-identical residuals is noisy but honest. A future "residual
cluster" reporting mode could group identical residuals without promoting
them to rules; not in scope for the staged milestones. The same applies
symmetrically to pure removals.

### 5.6 File-level operations

**Input.**

```
added:   src/UserContext.tsx    (contents: export const UserContext = ...)
deleted: src/features/users/slice.ts    (contents: export const slice = ...)
```

**Expected `summary` (§9 format).**

```
# residual
--- /dev/null
+++ b/src/UserContext.tsx
@@ -0,0 +1,1 @@
+export const UserContext = ...

# residual
--- a/src/features/users/slice.ts
+++ /dev/null
@@ -1,1 +0,0 @@
-export const slice = ...
```

File add/delete uses the standard unified-diff idiom of `/dev/null` on the
absent side. No dedicated `file_ops` section — the format is uniform.

## 6. Milestones

Ordered for early end-to-end usability. Each milestone is testable in
isolation against a synthetic fixture and leaves the tool in a usable state.

- **M1 — lift prototype to library + CLI.** Move `examples/change_summary.ml`
  logic into `lib/change_summary.ml{,i}`. Expose
  `summarize : changeset -> summary`. Add `diffract summarize BEFORE_DIR
  AFTER_DIR` subcommand to `bin/main.ml`, sharing the existing `--language`,
  `--include`, `--exclude` flags with other subcommands. Pair files by
  relative path under the two roots. Rules-only output (no residuals, no
  `after=` tiers — see §9). Folder-based E2E harness with hand-written
  `expected.summary` files compared via alpha-equivalence (§9).

- **M1.5 — Added/Removed as fusion candidates and residuals.** Extract
  `Added n` / `Removed n` from `child_change` and track them as candidates
  that can participate in conjunctive fusion (M1.6) by providing the match
  anchor that additions themselves lack. Additions/removals that do not get
  fused fall through into unified-diff residuals (once M1.9 lands); they
  are *not* emitted as standalone rules, because a pure `+`-only or
  `-`-only block has no structural anchor for the spatch engine to attach
  to. Test: §5.5.

- **M1.6 — Cross-cluster file-overlap fusion.** After independent clustering,
  compute Jaccard between every cluster's file set and fuse candidates per
  §4.2 — covering all three cases: Removed+Added → single two-sided
  section, one-sided+two-sided, and two-sided+two-sided → conjunctive
  multi-section. One-sided candidates unmatched after fusion do *not*
  become rules (they fall to residuals in M1.9). Test: §5.3; target
  reproducing `useAppSelector.pat`.

- **M1.7 — File-level operations.** Thread added/deleted files through the
  changeset type and into the summary. Test: §5.6.

- **M1.8a — Orphan-hole rejection (done).** Coherence gate rejects any
  cluster whose `+`-side has a metavariable not present on the `-` side
  (would render as `Metavars in replacement not bound in match` at apply
  time). The cut falls back to the coherent dendrogram parent, which
  typically captures the surrounding context that carries the binding
  source. Tested by `ts_lodash_to_native` and `kotlin_assert_migration`
  fixtures, where shared `hole_for` memoization across the before/after
  anti-unifications aligns the holes when the same concrete subtree
  appears on both sides. (Pending: M1.8b below.)

- **M1.8b — GumTree-based hole renaming.** Post-process anti-unified
  patterns to rename `+`-side holes to their corresponding `-`-side
  holes via the GumTree mapping, for cases where memoization-by-pair
  doesn't suffice — i.e. when the after's subtree is a *part of* or a
  *reshaping of* the before's subtree (the §5.2 example). Test: §5.2
  (first part: cluster doesn't dissolve).

- **M1.9 — Residual extraction (single tier).** Apply each rule to each site
  via `Match.transform`, diff the result against the site's after-source,
  emit the gap as a `residual` section in unified-diff form with optional
  `rule=Rn` attribution. Single-tier only: no `after=Rn` chains, no recursive
  re-clustering of residuals yet. Test: §5.2.

- **M2 — Recursive residual clustering + tiered rules.** Run the clustering
  pipeline on accumulated residuals to find secondary patterns; emit such
  rules with `after=Rn` attribution. Residuals attributed with chains
  (`rule=R1,R2`). Test: §5.2 extended with a third site producing a
  secondary rule.

- **M2.5 — Decomposition-based safety.** Property test: for every rule and
  every site, applying the rule (and then the attached residual, if any)
  reconstructs the after-source exactly.

- **M3 — Role-aware metavar naming.** Use tree-sitter field names to name
  metavariables (`$function`, `$arguments`) when unambiguous.

- **M4 — Hierarchy exposure.** Output format supports emitting dendrogram
  children. Test: §5.4.

- **M5 — Tuning and real-world soak.** `--min-support`, `--max-hole-fraction`,
  `--strategy`. Snapshot the summary of `changeset/` and `remove-redux.patch`
  as regression fixtures.

## 7. Non-goals

- **Sequential multi-step patches beyond one residual level.** Recursive
  clustering handles *one* level of residual decomposition cleanly. Deeper
  compositions (truly sequential refactors that aren't captured as "common +
  residual") are out of scope. This matches the spdiff paper's honest
  statement of its own limits.

- **Refactoring-classification.** Nothing attempts to label a rule as "this
  is a rename" or "this is a move". Rules are structural, named only by
  their position in the emitted list.

- **Truediff-style typed edit scripts.** We use the existing GumTree mapping
  as the before/after correspondence and do not introduce a separate
  linearly typed edit language.

- **Cross-language summaries.** Each changeset is processed per grammar; a
  changeset spanning languages produces independent summaries per language.

- **Inference of control-flow-sensitive changes.** A rewrite that reorders
  statements or moves logic across functions is outside the term-replacement
  fragment and will appear in residuals.

## 8. Testing strategy

Three tiers, each strictly cheaper than the next to author and run:

**Tier 1 — folder-based golden tests.** Each case lives under
`tests/change_summary_cases/<case_name>/` with subdirectories `before/` and
`after/` (paired by relative path) plus a hand-written `expected.summary`
file in the format of §9. The runner auto-discovers cases by listing that
directory, so adding a case is `mkdir` + drop files. Expected outputs are
authored by hand — there is no "run and bless" promotion path. If reality
drifts, the test fails; the fix is either a code change or a conscious,
reviewed edit to `expected.summary` with a real reason in the commit
message. Comparison uses the structural equivalence rules of §9.2, so
metavar names, section ordering, and whitespace-in-pattern-body do not
cause churn.

**Tier 2 — decomposition property tests.** For every rule in the output,
for every site, verify that applying the rule (via `Match.transform` or
`Match.transform_nested`) and then applying the site's attached residual
reconstructs the site's after-source byte-for-byte. This is the safety
invariant from §4.4 expressed as a test. Lands with M2.5.

**Tier 3 — real-changeset tests.** Run the full pipeline against
`changeset/` (the hedeby frontend sample) and a curated slice of the
remove-redux patch, placed under `tests/change_summary_cases/` like any
other Tier 1 case. These are judged by the same hand-written-expectation
rule — large, but still reviewed. Use to steer defaults in M5.

## 9. Output format and test harness

The output of `diffract summarize` and the expected-file format used by the
Tier 1 harness are **the same format**. Designing them once ensures the tool
emits what tests read.

### 9.1 Format

A `.summary` file is a sequence of sections. Each section begins with a
header line whose first two characters are `# ` (hash then space) at
column 0, followed by the section kind and a set of `key=value` attributes.
The section body runs from the line after the header until the next `# `
header line or EOF.

Section kinds:

- **`rule <id>`** — body is a diffract pattern in the existing `.pat`
  syntax (single-section or conjunctive multi-section). Required
  attributes: `support=<n>` and `language=<name>`. Optional:
  `after=<rule_id>` (§9.3, M2+).
- **`sites <id>`** — body is the list of files where rule `<id>` fires,
  one per line, sorted lexicographically. Distinct files only;
  `support=` on the rule header may exceed the sites count when a rule
  fires at multiple positions within one file.
- **`residual`** — body is a standard unified diff. Optional attribute
  `rule=<rule_id>[,<rule_id>...]` attributing the residual to the rule(s)
  whose application produced it. Absence of `rule=` means no rule
  covered this change — a pure one-off. (M1.9+.)

File add/delete is expressed as a residual with `--- /dev/null` or
`+++ /dev/null`. No dedicated `file_ops` section.

**Section-delimiter safety: the column-0 role-indicator contract.** The
parser treats any line beginning with `# ` at column 0 as a section
header. For this to be unambiguous, emitted rule bodies must never
produce a line that starts with `#` at column 0. The emitter guarantees
this by prefixing *every* line of a rule body with a role indicator —
one of `-`, `+`, ` ` (space, for context), or an expansion prefix (`,`,
`;`, `~`, …). This holds uniformly across all section kinds in a rule:
transformation sections (with `-`/`+` lines) and guard sections (a
conjunctive section that must match but produces no edits). Guard
sections, which otherwise have no syntactic need for role indicators,
are emitted with every context line space-prefixed. This contract is
enforced by the summary emitter only; diffract's pattern parser
continues to accept bodies without space-prefixed context lines, so
existing patterns outside the summary context are unaffected.

**No pure-addition or pure-removal rule sections.** Every section of
every emitted rule must have at least one `-` line *and* at least one
`+` line. Conjunctive semantics do not rescue one-sided sections: each
section of a conjunctive rule must itself match somewhere in the file,
so a pure-addition section is just as unattachable as a standalone one.
This is a structural requirement of the spatch engine, not a stylistic
preference — a section without a match side has nothing to apply to.
(Guard sections are a separate case: they have context lines matching
existing code, no `-`/`+` lines, and contribute no edits; they still
have a match side.)

**One-sided candidates are allowed internally.** The constraint above
governs *emitted* rules only. During clustering, the pipeline keeps
one-sided candidates (a Removed cluster or an Added cluster alone) so
that M1.6 fusion can pair them: a Removed cluster `C_R` and an Added
cluster `C_A` whose file sets coincide fuse into a single two-sided
section `- r_body / + a_body`. A one-sided candidate that finds no
counterpart is not emitted as a rule; it falls through to residuals
(M1.9).

Example covering rule, sites, attributed residual, unattributed
residual, and a new file:

```
# rule R1  support=3  language=typescript
@@
match: strict
metavar $X: single
metavar $Y: single
@@
- f($X, $Y)
+ g($X)
# sites R1
a.ts
b.ts
d.ts

# residual  rule=R1
--- a/a.ts
+++ b/a.ts
@@ -12 +12 @@
-f(x + 1, a)
+g(x)

# residual
--- a/d.ts
+++ b/d.ts
@@ -8,3 +8,2 @@
...

--- /dev/null
+++ b/src/UserContext.tsx
@@ -0,0 +1,42 @@
...
```

Residual hunks are recomputed canonically with context size 0 so the
format is stable across diff-algorithm tweaks.

### 9.2 Comparison (alpha-equivalence)

Tier 1 comparison is structural, not textual:

1. Parse the rule body with `Match_parse` on both sides.
2. Canonicalise each rule by walking match-side then replace-side in fixed
   pre-order, assigning metavar indices `#0, #1, ...` by first occurrence.
3. For conjunctive rules, canonicalise per section (each section's metavar
   scope is independent, matching diffract's scoping rule).
4. Sort rules within a summary by their canonicalised match-side before
   comparing, so rule ordering is irrelevant.
5. Compare metavar *types* (`single`/`sequence`) and match *modes*
   (`strict`/`partial`/`field`) literally — they're structural, not
   cosmetic.
6. Compare `support=` literally.
7. Residual hunks compared textually after trailing-whitespace trim and
   blank-line normalisation. Their `rule=` attribution compared after
   applying the same rule-id renaming that fell out of step 4.

This insulates expected files from metavar-name churn, section-order churn,
and irrelevant whitespace, while still catching real structural drift.

### 9.3 Tiered rules (M2+)

When recursive residual clustering (M2) finds a pattern inside the residuals
of some rule `R1`, the secondary rule is emitted with `after=R1`:

```
# rule R2  support=2  after=R1
@@
match: partial
metavar $X: single
metavar $K: single
@@
- $X + $K
+ $X
```

Semantics: R2 is applied to residuals of R1, not to raw source. A residual
remaining after both fired carries the chain: `# residual rule=R1,R2`.

Only linear chains are supported; no branching DAG of tiers in v1. Order
within a chain is application order.

### 9.4 M1 subset

M1 emits only `rule` sections (no residuals, no `after=`). A site that
doesn't match any emitted rule simply doesn't appear. The format above is
forward-compatible: existing expected files stay valid when M1.9 and M2
introduce residuals and tiered rules respectively.

### 9.5 Folder layout for Tier 1

```
tests/change_summary_cases/
  <case_name>/
    before/
      <relative_paths>...
    after/
      <relative_paths>...
    expected.summary
    language           (required)
```

Pairing is by relative path. A path present only in `before/` is a deletion;
only in `after/` is an addition. The runner registers one Alcotest case per
folder, named by `<case_name>`.

The `language` file contains a single language name (e.g. `typescript`,
`kotlin`, `php`) used as the *default* when a file's extension is not in
the extension→language map (`.ts` → typescript, `.tsx` → tsx). The file
is required even when every file has a mapped extension — explicit
declaration prevents silent misconfiguration.

## 10. Related work

- Andersen & Lawall, *Generic Patch Inference* (2010). Establishes safety
  and compactness formally. Our decomposition story corresponds to their
  sequential-patch composition `gp_1 ; gp_2`, computed constructively via
  residual extraction rather than via enumerate-and-intersect.
- Bader, Scott, Pradel & Chandra, *Getafix* (FSE 2019). Provides the
  anti-unification-with-memoisation primitive, the agglomerative clustering
  structure, and the hierarchy-as-output idea. Our clustering pipeline is
  essentially Getafix, minus the deployment-oriented ranking.
- Falleri et al., *GumTree* (ASE 2014). The node-mapping algorithm
  underlying `Tree_diff.compute_mapping`. Used here for change-pair
  extraction and for cross-side hole alignment.
- Erdweg, Szabó & Pacak, *Concise, Type-Safe, and Efficient Structural
  Diffing* (PLDI 2021). Typed edit-script framing. Relevant to how a
  residual could be represented if we ever want it in a more structured
  form than a `Tree_diff.diff`.
- Padioleau et al., *Coccinelle* / semantic patches. The output language of
  this feature — diffract's spatch DSL — is directly inspired by
  Coccinelle's SmPL. The `docs/patterns.md` file documents the concrete
  syntax.
