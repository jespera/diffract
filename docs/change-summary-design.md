# Change Summary: Design Document

Status: living design document; implemented through M2. Per-feature status
is marked consistently as *(implemented)*, *(planned)*, or *(deferred)* —
see the milestones in §6. The user-facing guide is
[change-summary.md](change-summary.md); the papers the design draws on are
indexed in [references.md](references.md). Keep this document up to date as
decisions change.

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
- **Safe**. For a site pair `(t, t')` and a rule with `t'' = apply(rule, t)`,
  the rule is *safe at the site* iff, for a tree edit-distance metric `d`,

  ```
  d(t, t'') + d(t'', t') = d(t, t')
  ```

  — applying the rule stays on a geodesic from `t` to `t'`: it makes no
  change that would have to be undone to reach the after-source.
  Equivalently, the rule's edits are a subset of a minimal edit script
  from `t` to `t'`. Every emitted `(site, rule)` association is safe;
  sites that fail the check are shed from the rule (they fall to
  residuals once those are emitted, M1.9). The summary must never
  include a transformation that wrongly states what changed — a rule
  that, scoped to a claimed site, edits something the changeset did not
  change (or writes different content than the changeset wrote) is
  excluded from that site no matter how high its support elsewhere.

  Safety is the *constraint*; generality remains the *objective*. The
  concrete per-site diff is maximally safe but minimally general, so
  safety never rewards collapsing back to a standard diff — among safe
  rules, the most abstract, highest-support ones are still preferred.

  Safety composes: if `R1` is safe wrt `(t, t')` with intermediate `t₁`
  and `R2` is safe wrt `(t₁, t')`, the chain is additive end-to-end
  (`d(t,t₁) + d(t₁,t₂) + d(t₂,t') = d(t,t')`). A tiered summary (§4.4,
  M2) is therefore a factorization of the geodesic, honest at every
  tier. The older "safe by decomposition" phrasing — `(rule,
  residual_at_site)` reproduces the site's after-source exactly — is
  the decomposed form of the same property. The operational check is
  the per-site safety gate (§3.1).
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

## 3. Architecture: propose, evaluate, select

> Status: implemented (M1.10). Adopted after the M1.8–M1.9 experience
> showed the original emission-from-cluster-provenance design
> accumulating correction passes. §3.3 records the lesson and the
> definitions; §3.1 describes the proposer's extraction, which is
> unchanged.

```
changeset
   │
   ├──▶ file operations ──────────────────────────────────────────────┐
   │                                                                   │
   └──▶ per-file Tree_diff ──▶ change-pair extraction (§3.1)           │
                                          │                            │
        ┌─────────────────────────────────┘                            │
        │            PROPOSE  (candidate generation)                   │
        ▼                                                              │
   anti-unification clustering (Getafix dendrogram, §4.1)              │
   cross-side hole alignment (§4.3) · coherence cuts                   │
   re-specialization · concrete regrouping · swap fusion (§4.2)        │
        │                                                              │
        ▼                                                              │
   candidate patterns  ← provenance (instances, merge history)         │
        │                 stays internal to this phase                 │
        │            EVALUATE  (semantics, §3.3)                       │
        ▼                                                              │
   per candidate × Modified file: edits + per-site safety              │
   classification (§3.1 gate) → the candidate's true extension,        │
   fires, and minimal-edit effects                                     │
        │                                                              │
        │            SELECT  (§3.3)                                    │
        ▼                                                              │
   greedy set-cover over the changeset's changed regions using         │
   true extensions; subsumption inherent (a candidate adding no        │
   uncovered region is never selected)                                 │
        │                                                              │
        ▼                                                              ▼
   rules ──▶ residuals = regions no selected rule covers (§4.4) ──▶ summary
```

Each phase is a pure function from the previous phase's output. The
boundary that matters is after PROPOSE: candidate patterns cross it,
their provenance does not. Everything user-visible — a rule's sites,
support, coverage, residual attribution — is derived by EVALUATE and
SELECT from the candidate's behaviour alone.

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
- **No depth gating.** Every `Modified` ancestor on the change chain
  is emitted unconditionally. Earlier iterations gated by
  change-density (emit only ancestors whose named-children change
  ratio exceeded a threshold), but with the safety and
  covering filters below, gating loses well-scoped lift candidates
  whose density looks low (a single property rename inside a longer
  member-access chain) for no real benefit — the unhelpful candidates
  get filtered later anyway.
- **Covering by byte-range overlap** *(superseded by §3.3 selection in
  M1.10 — the rank order below carries over as the set-cover
  preference; the instance-byte-range contest does not).* Clusters are
  ranked by support
  (desc), then by asymmetric-shape-first (patterns with arg-count or
  structural diffs between `-` and `+` carry strictly more
  information than same-shape renames), then by concrete-edit-count
  (desc — number of leaf-value renames captured; a member-expression
  rule like `legacyStore.fetch → store.get` captures two renames and
  beats the single-rename leaf `legacyStore → store` at the same
  site, even though both are symmetric), then by concrete-node-count
  (asc — among ties on edit count, smaller wins so a leaf rename
  beats its enclosing scaffold when both capture the same renames),
  then by hole fraction, then by pattern text. The winning cluster
  claims the byte ranges of its sites; subsequent clusters whose
  instances overlap a claim in the same file are dropped. Disjoint
  sites at the same call (e.g. independent receiver and method
  renames at byte-disjoint positions) do not overlap and can each win
  their own rule.
- **Coherence gate.** A cluster survives only if its pattern contains
  *at least one* concrete anchor on at least one side — a named leaf
  or a keyword-shaped unnamed token (one whose text contains an
  alphabetic character, e.g. `array`, `function`, `class`). Pure
  punctuation (`,`, `(`, `;`) does not count. The either-side rule
  accommodates asymmetric reshapes whose `+` side has no keyword:
  PHP's `array($H0, $H1) → [$H0, $H1]` is informative because the
  `array` keyword on the `-` side is the anchor, even though the `+`
  side is just brackets and holes. The cluster must also have at
  least one concrete edit (either differing multisets of leaf values
  or differing structural shape modulo holes), which prevents
  fully-holed patterns on both sides from sneaking through. All-hole
  scaffolding like `$.$($)→$.$($)` is rejected here before it can
  compete for coverage. The gate also rejects clusters whose `+`-side
  has a metavariable not present on the `-` side (M1.8a).
- **Per-site safety gate.** The operational form of the safety
  desideratum (§2.3), checked behaviourally per `(cluster, site)`
  association. Render the cluster's pattern as `.pat` text and run
  the matcher against the site's full pre-change source; the
  application's edits — *every* match, not just the originating
  instance — must each satisfy two legs against the site's
  `Tree_diff`:

  1. *Placement*: the edit's span lies within a region the diff marks
     as changed. An edit landing in an unchanged region is, by
     construction, a change that must be undone to reach the
     after-source — unsafe.
  2. *Content*: the edit reproduces what the changeset actually wrote
     there. For a removal, the region must be `Removed` in the diff;
     for a rewrite, the instantiated replacement must equal the
     region's after-side content.

  A site at which any edit fails is *shed* from the cluster (the rule
  simply doesn't claim that site; with M1.9 the site's changes fall to
  residuals); a cluster whose safe sites drop below `min_support`
  dissolves, and the dendrogram cut falls back to its children —
  typically the concrete-majority subtree.

  The motivating failure is the over-merged removal-only rule: files
  that each remove one import, but not all the *same* import,
  anti-unify to `- import _H0` — a rule that, applied to any of its
  claimed sites, would remove every import in the file, almost all of
  them in unchanged regions. Placement rejects it per-site, and the
  cut falls back to the concrete rule for the majority import. The
  content leg symmetrically rejects a rule that rewrites a changed
  region to something other than what the changeset wrote (claiming
  `f → h` where the change was `f → g`). Structural proxies
  (hole-fraction thresholds) cannot express either reliably —
  grammatical scaffolding pads node counts, e.g. `- import _H0`
  scores 1 hole / 3 nodes and slips under a 0.35 threshold.

  The gate is *implemented as residual computation* (§4.4): applying
  the rule at a site and comparing against the after-source classifies
  the association as `exact` (residual empty — the rule fully explains
  every region it touches), `decomposable` (residual is pure shrinkage
  of the original diff: each rule edit appears in the diff's script,
  untouched regions remain), or `unsafe` (the residual would have to
  correct the rule's own edits). What may be *emitted* is staged by
  milestone: M1 emits `exact` sites only, because the M1 format cannot
  attach residuals and an unexplained gap would mis-state the change;
  M1.9 admits `decomposable` sites with `rule=` residual attribution;
  M2 clusters those residuals into `after=` tiers. The gate never
  relaxes — only the output format's ability to state the
  decomposition honestly grows.

  Because `Tree_diff` is GumTree-style rather than provably minimal,
  the check can be conservative: where the diff's script is
  non-minimal a genuinely safe rule may be rejected. The failure
  direction is deliberate — the tool may under-summarize, but never
  mis-state. The gate subsumes the earlier zero-match applicability
  check (a rendering that re-parses at the wrong grammatical position
  matches nowhere, so it has no safe sites) — e.g. a
  `property_identifier` rendered standalone re-parsing as a bare
  `identifier`.
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

### 3.2 Contextual emission with ellipses (planned)

The §3.1 emission is bimodal: a `Modified` ancestor emits with its
non-changed siblings rendered as `$Hk` placeholders, and a `Replaced`
leaf emits in isolation. This produces two failure modes when the change
is a single localized edit embedded in a larger scope:

- **Leaf too narrow.** A bare `Removed` child (e.g. one dropped import
  line in a 35-import block) currently emits no standalone change pair
  at all — only its parent does. Even when leaves do emit, the rendered
  text often re-parses at a different grammatical position and is
  rejected by the safety gate.
- **Parent too wide.** The parent's other siblings appear as
  `$H0..$Hn` placeholders. Across files those siblings differ, so
  anti-unification keeps them parameterized rather than collapsing the
  change. The actual delta is buried in scaffolding. The
  `kotlin_common_import_drop` fixture is a minimal instance: three
  files each drop the same single import line out of a longer block,
  where the *useful* rule is the one-line removal alone, but the
  current emission produces no rule (or, on larger real-world inputs,
  multiple block-level rules differing only in their surrounding
  imports).

The generalization is to treat a change pair as **scope + ellipses +
delta** rather than scope + holes + delta. The parent is kept as the
matching context, but its non-changed children are rendered as `...`
(anonymous sequence matching) rather than `$Hk` placeholders. Three
cases collapse into one emission unit:

- `foo() { ... - p ... }` — removal inside a scope
- `foo() { ... - p; + p' ... }` — replacement inside a scope
- `... - import com.example.LegacyHelper ...` — degenerate
  flat-sequence case (empty scope, just an anchored removal in a
  sequence)

The pattern language already supports `...` with auto-context detection
(see CLAUDE.md "Ellipsis"); the change lives on the emission side. The
cluster engine then anti-unifies on the delta itself, with scope
contents ellipsed out, so two files whose change occurs inside
structurally different scopes can still cluster on the delta.

This subsumes the "anchored removal" case (where the surrounding
context is the immediate adjacent siblings, kept literal rather than
ellipsed) and the "removal-only standalone rule" case (empty context).
It gives a uniform mechanism for emitting change pairs at the right
granularity without per-grammar tuning of which ancestor levels to
prefer.

### 3.3 Evaluation-based semantics

**The lesson (M1.8–M1.9).** The original design derived a rule's
user-visible properties — its sites, support, byte-range claims —
from the *cluster instances that produced it*: which change pairs
survived emission levels, dendrogram merges, coherence cuts, and the
covering contest. That lineage describes how the rule was *found*, not
what the rule *means*, and the two systematically diverge. Each
divergence was patched as it surfaced: the safety gate sheds sites the
cluster wrongly claimed; re-specialization repairs patterns whose
evidence shrank after covering; dedupe merges rules separate clusters
created; subsumption (§4.5) drops rules whose behaviour another rule
contains; and M1.9a's residuals exposed the remaining gap — changes
present in a file that an emitted rule demonstrably explains, leaking
into residuals because the rule's cluster never enrolled that file.
Chasing that last gap with yet another reconciliation pass
("site completion") would never terminate: every change to the
proposer re-opens it. The fault is architectural — provenance was
allowed to define semantics.

**The principle.** A rule's meaning is its *extension*: the set of
sites where it fires safely. This is the same semantics the safety
property (§2.3) is defined over, and the same one spdiff gives a
generic patch (its support *is* the set of term pairs where it is
safe). Clustering proposes; evaluation defines:

- **Propose.** Extraction (§3.1), anti-unification (§4.1), cross-side
  alignment (§4.3), coherence cuts, re-specialization, concrete
  regrouping, and swap fusion (§4.2) produce *candidate patterns*.
  Their instance bookkeeping is internal scaffolding for proposing
  good candidates; nothing downstream reads it. A weak proposer costs
  recall (a systematic change may go un-proposed and fall to
  residuals); it can no longer cost honesty.

- **Evaluate.** For each candidate pattern and each Modified file:
  compute the candidate's edits (`Matcher.transform_edits`) and run
  the per-site safety classification (§3.1). The candidate's
  **extension** is the set of files where it fires safely; its
  **support** is its fire count over the extension; its
  **minimal-edit effects** (§4.5) at each site record which changed
  regions it resolves. The gate is thereby promoted from a filter on
  cluster output to the definition of rule semantics. Per-site
  classification keeps the M1 emission policy unchanged (exact-only;
  `decomposable` arrives with M1.9b as an evaluator extension).

- **Select.** Choose the emitted rule set as a greedy weighted
  set-cover of the changeset's **changed regions** (the site-DB
  regions of §3.1), using true extensions: a candidate's value is the
  set of still-uncovered regions its effects resolve; rank by the
  existing covering order (support desc, asymmetric-shape first,
  concrete-edit count, concreteness, hole fraction, text); stop when
  no candidate adds coverage at `min_support` or above. Subsumption is
  inherent rather than a separate pass: a candidate whose resolved
  regions are already covered adds nothing and is never selected.
  Greedy, not optimal — consistent with §2.3's "practical rather than
  optimal" stance.

- **Residuals (§4.4).** Unchanged in mechanism, but leakage-free by
  construction: a rule claims a file iff evaluation put the file in
  its extension, so a residual contains only changes *no* selected
  rule resolves.

**What this obsoletes.** Covering-by-instance-byte-ranges (replaced by
region set-cover), the standalone subsumption pass of §4.5 (its
minimal-edit/effect machinery moves into EVALUATE/SELECT; the partial
order remains the organizing relation), emission-side dedupe
(candidates dedupe at proposal; equal-effect candidates resolve at
selection), and all instance-set threading through emission.
Re-specialization survives inside PROPOSE as candidate improvement.

**Support semantics.** Support becomes the behavioural fire count over
the extension. Where a cluster's instance count understated a rule's
true reach (the M1.9a leakage cases), supports rise; they can no
longer overstate. Fixture expectations change accordingly — a
conscious, one-time review at implementation.

**Cost model.** EVALUATE is `|candidates| × |Modified files|` gate
checks, short-circuited by a cheap no-match test and memoized per
(pattern, file) as today; it is embarrassingly parallel. The dominant
cost remains the PROPOSE dendrogram — and the inversion deliberately
lowers the stakes there: a cheaper, sloppier proposer (sampling,
aggressive pre-grouping) only affects which candidates exist, never
the correctness of what is emitted about them.

**Overlapping extensions and common factors.** The `(rule_i, C_i)`
decomposition is *not* a partition — extensions overlap, and the
subpatch order (§4.5) structures the overlap: a weaker patch is safe
wherever a stronger one is, so extension is antitone in patch strength
and candidates relate to site sets as a concept lattice, not a flat
cover. The consequence that matters: given two major clusters `C_1,
C_2` with primary rules `R_1, R_2`, decomposing further *inside* each
may surface a change common to both — a factor `gp_c` with
`gp_c ⪯ R_1` on `C_1` and `gp_c ⪯ R_2` on `C_2`, whose extension spans
`C_1 ∪ C_2` even though neither primary's does. The flat summary's
set-cover deliberately hides `gp_c` (once `R_1` and `R_2` are selected
it resolves no uncovered region), which is the right call for
compactness but loses the factoring. Two consequences for later
milestones:

- M2's recursive clustering must run over the residuals of *all*
  rules globally, not per rule, so a shared secondary change emerging
  inside both clusters becomes one rule — which makes `after=`
  attribution per-*site*, not per-rule (`after=R_1` at `C_1` sites,
  `after=R_2` at `C_2` sites); §9.3's format needs that refinement.
- Selection policy is a genuine degree of freedom: the same changeset
  admits multiple safe factorizations — monolithic (fewer, larger
  rules) versus factored (a common-factor rule plus per-cluster
  completions) — and the safety property guarantees both are honest.
  The flat summary picks one by the compactness rank; M4's hierarchy
  exposure should present the lattice itself, with the common factor
  as the shared ancestor of `R_1` and `R_2`, rather than baking in
  one cut of it.

## 4. Key design decisions

### 4.1 Hierarchical clustering (Getafix)

Following Bader et al., *Getafix* (§4.2): start with one singleton cluster per
change pair, greedily merge the pair of clusters whose anti-unification
introduces the fewest new holes (measured as hole fraction relative to pattern
size), and record the merge tree. The result is a dendrogram whose leaves are
concrete edits and whose interior nodes are increasingly abstract patterns.

**Merge score, not raw hole fraction.** Pure hole-fraction picks
greedy merges that produce orphan after-holes — e.g. merging
`tokenCache.read → tokenCache.get` with `tokenCache.write →
tokenCache.set` (same receiver) holes the property on each side
independently and gives a `+`-side hole with no `-`-side binding
source. The coherence gate (§3.1) rejects such patterns, dropping the
merge to singletons and losing the chance to express
`$X.read → $X.get` as a generalised rule. Penalise these merges by a
large constant in the score so the greedy step prefers a sibling
pairing that keeps holes aligned (e.g. merging different receivers
with the same property: `tokenCache.read + rateCache.read →
$H0.read`).

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

### 4.3 Cross-side alignment by content sharing (after hdiff)

> Adopts the change representation and core algorithm of Miraldo &
> Swierstra's *hdiff* (ICFP 2019, §10), scoped to cross-side alignment and
> composed with our cross-file generalisation. Supersedes the earlier
> "GumTree-based hole renaming" framing (M1.8b), which only repaired an
> already-formed orphan and could not produce the fused-context rules
> below.

**The problem.** A change pair's `(-, +)` contexts are built by
anti-unifying the before-tree and after-tree *independently* (shared
hole counter), so a `+`-side metavariable binds to a `-`-side one only
when the *same concrete subtree* sits at a corresponding position. It
orphans whenever the after reuses a value that is present in the before
but *position-misaligned* — pervasive in real refactors, and the subject
of the `*_unwrap_*` / `*_extract_*` fixtures:

```
box(x + 1).get()           ⤳  x + 1        (extract a wrapped expression)
save({ id: u, name: n })   ⤳  save(u)      (field → positional argument)
<B>{e}</B>                 ⤳  e            (unwrap a JSX element)
```

Today the orphan is rejected by the coherence gate and the change falls
to the one-sided-removal path: delete the whole before-compound and
residualise the re-inserted value (the broken-looking `val v = `,
`save()`, `const c = ` intermediates seen in the soaks).

**Representation = our rule.** hdiff represents a change as a pair
`(del, ins)` of contexts sharing metavariables — exactly our `-`/`+`
rule. The leverage is in *how* the metavariables get assigned.

**Content-keyed assignment (`wcs` / `extract`).** Rather than aligning
positions, hdiff assigns a metavariable to every subtree *common to
both* before and after (the `wcs`, "which common subtree", oracle), then
`extract` replaces each maximal common subtree by its metavariable in
*both* contexts. A value nested in the before and standalone in the
after thus receives the **same metavariable regardless of position** —
the orphan never arises. For `box(x + 1).get() ⤳ x + 1`, `x + 1` is a
common subtree, so extraction yields `box($H).get() ⤳ $H` directly.

The oracle is effectively free for us. hdiff makes `wcs` linear with a
Merkle hash per subtree plus a set of common digests; `Tree.hash` *is*
that Merkle root (a structural, position-independent hash), and the
common set is a `Hashtbl` over before/after subtree hashes.
(`Tree_diff.compute_mapping`'s GumTree correspondence is an alternative
oracle; the hash intersection is simpler, linear, and needs no anchor
alignment.)

**Closure.** Content extraction can still leave a `+`-metavariable
unbound in the `-` context when its binding source is a *sibling* part
of the change — e.g. the `): M[]` return type whose `M` is the default
of a `<T = M[]>` deleted elsewhere in the same function. hdiff's
`closure` enlarges the change *up the spine* until every `+`-metavariable
is bound. For us this reads the §3.1 multi-level emission as a ladder:
closure is the choice of the smallest enclosing Modified ancestor at
which the orphan binds. Its two outcomes *are* the rule/residual
boundary:

- *Closes at some ancestor* → that ancestor is the rule. The function-
  level `<T = M[]>(…): T → body` ⤳ `(…): M[] → body` is a closure — it
  binds `M`, fusing the generic-drop, the `select`-drop and the
  return-type change into one rule.
- *Never closes, even at the change-pair root* → the value is genuinely
  new (not a copy of any before-content); the unbound part is a true
  residual (§4.4, M1.9b). Closure-fails-at-root is the precise criterion
  separating "this is a rule" from "this is a residual."

**Composition with generalisation — and its limit.** hdiff diffs a
*single* pair; our output is a rule generalised over *many*. The two
objectives are duals: content extraction holes *commonalities within a
pair* (copies); our anti-unification (§4.1) holes *variabilities across
pairs* (generalisation). They compose in the common case — build each
pair's `(del, ins)` by content extraction + closure, then anti-unify
across files — and for the `*_unwrap_*` fixtures, where the surrounding
structure is identical across the cluster, this yields one clean rule
with no residual.

They *conflict* when the surrounding structure varies across the
cluster. §5.2 (`f(x + 1, a) ⤳ g(x)` and `f(3, 1) ⤳ g(3)`) is the
canonical case: per-pair content extraction gives `f($H + 1, a) ⤳ g($H)`
for one site and `f($H, 1) ⤳ g($H)` for the other — exact per pair, but
the `-` contexts disagree in shape and do not generalise to one rule.
The generalising choice is to *un-share*: hole the whole first argument
(`f($X, $Y) ⤳ g($X)`) and let a residual close the gap where the after
used only a *sub-part* of it (`x + 1 ⤳ x`). That is the M1.9b
decomposition, which is why §5.2 is a residual example, not a §4.3 one.
The distinction is sharp: **§4.3 handles verbatim cross-side common
subtrees that positional anti-unification mis-aligns; sub-part reshapes
whose surrounding shape varies across the cluster are M1.9b residual
cases.** Selection (§3.3) arbitrates — content extraction merely
*proposes* the maximally-shared candidates; whether they win or are
coarsened to a rule-plus-residual falls out of the region set-cover.

We restrict content-keying to **cross-side** alignment (binding an
after-value to a before-value). hdiff also shares *within* a side by
default — forcing two before-occurrences of one value to a single
metavariable, a contraction that imposes a non-linear match — which we
leave to the existing generalisation, to avoid over-constraining emitted
rules.

**Empirical applicability and deferral (2026-06-09).** §4.3 is
implemented (containment slice + `ev_clean` selection) and kept as a
regression-tested capability, but it is **dormant** — on every real
changeset available it fires on nothing, and output is byte-identical to
the pre-§4.3 baseline. Its distinctive reach is narrow: a preserved value
*relocated* such that the tree-diff orphans it positionally, with no
containment (else the implemented slice already catches it) and not a
sibling reorder (which the diff reports as *unchanged*, §5.3 / the swap
case — a tree-diff sensitivity problem, upstream of content-keying). Two
real soaks confirm the gap is elsewhere:

- An *introduce-variant* refactor residualises a novel one-off type
  definition (correctly — support 1), a handful of sub-`min_support`
  renames, and deletions. None is relocation-with-misalignment.
- A *hook-reshape* changeset's dominant residual (≈40 of 54 hunks) is one
  systematic shape, `HOOK({ select: (m) => BODY }) ⤳ useAppMemo(HOOK(),
  useCallback((m) => BODY, [DEPS]))`. `HOOK` and `BODY` are
  verbatim-preserved and the diff matches them positionally (so
  content-keying is not even *needed*); the blocker is `[DEPS]` — a
  per-site *computed* value (the lambda's free variables), which no
  metavariable substitution can produce. That is an **M1.9b** rule +
  per-site residual, not a content-keying case. *(Post-M1.9b/c
  correction: the verbatim-repeated instances of this shape were already
  exact concrete rules in the baseline; the rest vary in hook, body, and
  object fields simultaneously, so they sit behind the clustering
  frontier, not the deps orphan — see M1.9c's real-changeset reading.)*

So even the most hdiff-adjacent real residual needs M1.9b as the
load-bearing piece, with content-keying at most a non-load-bearing
helper. We therefore prioritise M1.9b (next section) and do not extend
content-keying (within-side sharing, content-keyed orphan rescue beyond
containment) until a changeset that actually exhibits
relocation-with-misalignment appears.

### 4.4 Residual extraction and recursive decomposition

Cross-side alignment does not guarantee the rule reproduces the after-source
exactly. When it doesn't, the gap is a **residual**: the additional local
change that was applied at that site on top of the common rule.

**Mechanism.** For each site `(t_i, t_i')` in a cluster with rule `p ⤳ p'`:

1. Apply the rule: `intermediate_i = Match.transform(p⤳p', t_i)`.
2. If `intermediate_i = t_i'`, the pair is fully explained.
3. Otherwise, the residual is `Tree_diff.diff(intermediate_i, t_i')`. Attach
   it to the site.

**Shared with the safety gate.** This is the same computation the per-site
safety gate (§3.1) performs — the gate *is* residual extraction with a
classification on the result: `exact` (empty residual), `decomposable`
(residual is pure shrinkage of the original site diff), `unsafe` (the
residual would have to undo the rule's own edits — the rule left the
geodesic, §2.3). M1.9 does not add a new analysis; it surfaces the
residuals the gate already computes, in unified-diff form with `rule=`
attribution.

**Recursive clustering on residuals.** Residuals are themselves change pairs.
Running the clustering pipeline on the accumulated residuals produces
secondary rules — common small changes that appear across a subset of the
sites. This process terminates when every remaining residual is a singleton.

This is exactly spdiff's sequential-patch composition `gp_1; gp_2`
constructively computed instead of searched. Because safety composes
(§2.3), the resulting tiers are a factorization of each site's geodesic:
the summary can state "this overall change happened everywhere; within
the cluster, these further decompositions are also present" — with every
tier individually safe, never mis-stating the change. Every site's change
equals (common rule) ∘ (secondary rules) ∘ (residual), all explicit in
the summary.

**Why this is valuable, not a workaround.** A reviewer of a large refactor
has two questions: "Is the mechanical part correct?" (answered by inspecting
the primary rule once) and "Is anything else going on?" (answered by the
residuals). Traditional diffs conflate these. Residual extraction separates
them, and when residuals cluster, surfaces hidden secondary patterns — for
instance a small bug fix riding along with an API migration.

### 4.5 Subsumption reduction under the partial order

> Status: implemented (M1.8d) as a standalone post-emission pass. With
> the §3.3 inversion (M1.10) the pass dissolves into selection — a
> candidate whose resolved regions are already covered is never
> selected — while the minimal-edit canonicalisation and effect
> comparison defined here move into the evaluator. The partial order
> itself remains the organizing relation (and the backbone for M4
> hierarchy exposure).

The safety property induces a partial order on transforms: `A ⊑ B` (A is
*part of* B) wrt a site set when, at every site of A, applying B covers
A's effect — the straightforward extension of §2.3's safety to partially
applied terms. The summariser can use this to *reduce* its output: a rule
whose effect at every claimed site is reproduced by another emitted rule
is subsumed, and its sites fold into the subsuming rule.

This catches redundancy that site covering (§3.1) structurally cannot:
covering dedupes at the *instance* level (each cluster's claimed byte
ranges), but two pipelines can state the same change about the same code
through different cluster instances — a parent-level block rewrite whose
net effect is exactly one child's deletion, alongside the deletion rule
itself. Subsumption compares what the emitted rules *do* when applied,
which is the honest comparison.

**Operational check.** For each emitted rule × site, take the rule's
edits (already computed by the safety gate) and canonicalise each to its
*minimal* form by trimming the common prefix/suffix between the replaced
span and its replacement — this makes a block-level rewrite that
reproduces 62 of 63 children comparable to the one-line deletion it
actually performs. Then `A ⊑ B` iff at every site of A, A's minimal
edits are a subset of B's (span and content), and reduction drops A,
folding its sites and support into B (positions are disjoint, so support
adds honestly). Pairwise over emitted rules restricted to shared sites.

The same partial order is the natural backbone for hierarchy exposure
(M4): reduction *deletes* subsumed rules from the flat summary, but the
poset is what a drill-down view would *expose* — a general rule and,
within it, the site-specific specialisations it subsumes.

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

**Why this is M1.9b, not §4.3.** Per-pair content extraction (§4.3) would
share the leaf `x`, giving `f($H + 1, a) ⤳ g($H)` for file1 and
`f($H, 1) ⤳ g($H)` for file2 — exact per pair, but the `-` contexts
disagree in shape and do not generalise. The generalising choice
*un-shares*: hole the whole first argument as `f($X, $Y) ⤳ g($X)` and
bind `$X` to the after across sites (for file2, `3` matches verbatim; for
file1, the after's `x` is a *sub-part* of the before's `x + 1`, which is
exactly the residual). This coarse rule does not reproduce file1's after
on its own.

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

- **M1 — lift prototype to library + CLI (implemented).** Move `examples/change_summary.ml`
  logic into `lib/change_summary.ml{,i}`. Expose
  `summarize : changeset -> summary`. Add `diffract summarize BEFORE_DIR
  AFTER_DIR` subcommand to `bin/main.ml`, sharing the existing `--language`,
  `--include`, `--exclude` flags with other subcommands. Pair files by
  relative path under the two roots. Rules-only output (no residuals, no
  `after=` tiers — see §9). Folder-based E2E harness with hand-written
  `expected.summary` files compared via alpha-equivalence (§9).

- **M1.5 — Added/Removed as fusion candidates and residuals (implemented).** Extract
  `Added n` / `Removed n` from `child_change` and track them as candidates
  that can participate in conjunctive fusion (M1.6) by providing the match
  anchor that additions themselves lack. Additions/removals that do not get
  fused fall through into unified-diff residuals (once M1.9 lands); they
  are *not* emitted as standalone rules, because a pure `+`-only or
  `-`-only block has no structural anchor for the spatch engine to attach
  to. Test: §5.5.

- **M1.6 — Cross-cluster file-overlap fusion (implemented).** After independent
  clustering, two-sided clusters and one-sided swap pairs are unified into a
  single fusion graph keyed on file sets; connected components above a
  Jaccard threshold (default 0.7) emit as a single multi-section rule whose
  sites are the intersection of the components' file sets. Case 1
  (Removed+Added → single two-sided section) is the existing `fuse_swap`
  path; cases 2 and 3 fall out for free from the unified graph because a
  case-1 swap pair enters the graph as just another two-sided node. If a
  fusion's all-way intersection has fewer than `min_support` sites, the
  fusion is abandoned and members emit standalone. Tested by
  `mixed_systematic` (case 3, two property renames) and
  `co_occurring_renames` (case 3, two function renames) fixtures. The
  §5.3 `useAppSelector.pat` reproduction is still aspirational —
  reaching it depends on cross-side alignment (§4.3) resolving the import
  rewrite and the call-site rewrite into clean two-sided clusters with
  bound holes; a dedicated fixture for that case will land alongside
  M1.8b.

- **M1.7 — File-level operations (implemented).** Added/Deleted files emit as
  unattributed residual sections with `/dev/null` on the absent side
  (§5.6, §9); implemented together with M1.9a. Test: §5.6.

- **M1.8a — Orphan-hole rejection (implemented).** Coherence gate rejects any
  cluster whose `+`-side has a metavariable not present on the `-` side
  (would render as `Metavars in replacement not bound in match` at apply
  time). The cut falls back to the coherent dendrogram parent, which
  typically captures the surrounding context that carries the binding
  source. Tested by `ts_lodash_to_native` and `kotlin_assert_migration`
  fixtures, where shared `hole_for` memoization across the before/after
  anti-unifications aligns the holes when the same concrete subtree
  appears on both sides. (Pending: M1.8b below.)

- **M1.8b — Cross-side alignment by content sharing (§4.3, reframed; implemented).**
  Originally scoped as a post-process that renames a `+`-side hole to its
  `-`-side hole via the GumTree mapping. Superseded by the hdiff-based
  formulation in §4.3: assign metavariables by *content* (common-subtree
  oracle over `Tree.hash`) so a position-misaligned cross-side value binds
  by construction. As built (`extraction_pairs`): when a `Modified` node
  has a `Removed` child `r` and an `Added` child `a` with one a structural
  subtree of the other (`Tree.hash` membership), emit the `(r, a)` pair as
  a two-sided change pair *in addition to* the one-sided candidates. The
  existing cross-file anti-unification then forms the extraction rule
  (`box($H).get() ⤳ $H`) and binds the shared hole by content; the §3.3
  selector arbitrates, with a `clean` tie-break (`ev_clean`: candidate
  alone reproduces the site's after) so a reconstructing extraction beats
  a bare removal that defers the rest to a residual. "Closure" is realised
  implicitly: pairs are emitted at every level and selection picks the
  tightest applicable one. Tests: `ts_unwrap_chain`, `tsx_unwrap_element`,
  `kotlin_ctor_extract_arg`, `ts_object_to_positional` are live and green
  (the latter two emit the bare extraction — more general than, and
  preferred over, the original save()-wrapped targets). `kotlin_unwrap_chain`
  is also live (see fusion-input arbitration fix below). Note §5.2 is *not*
  covered here — it is the sub-part-reshape-with-varying-surround case,
  handled as rule + residual under M1.9b (§4.3 "Composition" para).

  **Fusion-input arbitration over global semantics, not provenance.**
  `kotlin_unwrap_chain` first carried unchanged `val v =` context: the bare
  `box($H).get() ⤳ $H` call-expression cluster *did* form, but the
  fusion-input arbitration ("keep one representative per change-family,
  prefer more resolved regions then shorter text") scored each cluster's
  resolved regions over its *own provenance files* — and one call-level
  instance had been shed during clustering, so the call cluster's
  provenance was 2 files vs the statement cluster's 3. The broader cluster
  won on region count and shadowed the tighter one before it could be
  evaluated globally (where its pattern resolves all 3 sites). The fix:
  score `resolved_of` over **all changed files**. Nested granularities of
  the same change then resolve the same regions, and the existing
  shorter-pattern tie-break picks the tighter rule. The dendrogram
  `try_emit`-skips-recursion path was *not* the cause — both clusters were
  already present after the cut.

- **M1.8c — Per-site safety gate (implemented).** Replace the zero-match behavioural
  applicability check with the per-site safety classification (§3.1):
  every edit the rule would make at a claimed site must land in a
  changed region of the site's diff and reproduce that region's
  after-content. Sites are shed individually; clusters below
  `min_support` dissolve and the cut falls back. Implemented as
  residual computation (§4.4) classifying each `(cluster, site)` as
  `exact` / `decomposable` / `unsafe`; M1 emission policy is `exact`
  only. Kills over-merged removal-only rules (`- import _H0`) whose
  extra matches land in unchanged regions, and surfaces the
  concrete-majority rule beneath them. Tests: import-removal fixture
  (concrete majority survives, fully-holed variant must not appear);
  wrong-content fixture (over-general rewrite must not claim a site
  whose region changed differently).

- **M1.8d — Subsumption reduction (§4.5) (implemented; dissolved into selection by M1.10).** Post-emission pass: drop
  every rule whose minimal edits at all its sites are reproduced by
  another emitted rule, folding sites/support into the subsuming rule.
  On the real-changeset soak this removes the parent-level block
  rewrites (import-block and parameter-list rules) whose net effect is
  stated exactly by the fine-grained deletion/rewrite rules. Also under
  this heading: contextual emission (§3.2) for deltas whose fine-grained
  rule never existed — partial-mode for container scopes; ellipsis-strict
  once siblings matching handles grammar-restricted positions (pinned as
  a known-bug test on the matcher side).

- **M1.9a — Residual emission / completeness (implemented).** Per Modified
  file: apply the file's claiming rules (in id order) and diff the
  intermediate against the real after-source with zero context; the
  gap, if any, emits as a `residual` section attributed `rule=R1,R2`
  (application order). The residual is computed against what the rules
  *actually* produce, so rules ∘ residual reproduce the site's change
  by construction. Files no rule claims emit unattributed residuals.
  Layout-only gaps are skipped (the gate's whitespace tolerance).
  Rules + residuals now account for the whole changeset — the Covering
  desideratum of §2.3 holds.

- **M1.9b — Decomposable-site relaxation (implemented; gate = tree
  inclusion + net progress).** Relax the M1 exact-only
  emission policy so safe-but-partial (`decomposable`) sites count
  toward a rule's support, carrying their `rule=`-attributed residual
  (the §5.2 case: `f($X,$Y) → g($X)` at `f(x+1,a) → g(x)` with
  residual `x+1 → x`). The `decomposable`/`unsafe` distinction is the
  **geodesic test** (§2.3), computed at evaluation in two legs:

  *Well-formedness precondition (all verdicts, not just decomposable):*
  a transform must produce parseable code — every parse-ERROR in the
  rule's output must already exist (by error text) in the site's before
  or after; an error in neither endpoint is one the rule invented.
  A removal-only rule deleting a grammar-required sub-expression yields
  a broken intermediate (`const r = ;`) whose re-diff is unreliable — it
  once judged such sites fully explained, letting a bare deletion rule
  out-cover the real extraction rule and mis-state preserved values as
  deleted-then-readded (fixture `ts_unwrap_rename_confound`). Damage the
  rule itself causes is not a residual's job to repair. Pre-existing
  errors are tolerated rather than disqualifying the file — real corpora
  contain the odd unparseable stretch (a soak survey found 5 of 3240
  files), and a rule editing elsewhere in such a file is unaffected
  (fixture `ts_rename_with_parse_error`).

  *Residual leg — ordered tree inclusion* (`Tree_inclusion`, on `main`;
  Kilpeläinen & Mannila): `t''` and the after must be
  inclusion-comparable — one obtainable from the other by node deletion
  alone, internal deletions promoting children. The residual is then a
  pure insertion (`t'' ⊑ after`: the rule under-wrote, e.g. an emptied
  dependency array the site fills) or a pure deletion (`after ⊑ t''`:
  the rule over-wrote through a metavariable, `g(x+1)` where the site
  keeps `x` — guarded so deleted content must be before-derived, never an
  invented template literal). A detour (`f→h` where the change is `f→g`)
  is a relabel, forbidden in both directions. Inclusion replaced an
  earlier `diff_node_count` distance equality, which was not additive
  across multi-element edits (inserting `[b, c]` into an
  already-rewritten region) and shed exactly the sites that mattered;
  inclusion tests the trees directly, with no additivity assumption and
  no sensitivity to how the diff groups changes.

  *Rule leg — net progress.* Inclusion alone misses delete-then-readd: a
  rule that empties a function body to `{}` passes the residual leg (the
  re-add is "pure insertion") yet does work the residual must undo —
  the soak surfaced precisely this, +437 output lines of a rule
  re-stating bodies its residuals re-inserted. The guard is the
  compactness half (spdiff's largest *common* part, MDL): the in-zone
  gap a decomposable claim leaves must be strictly smaller than the
  change it explains, so claiming a site always states the change more
  compactly than its raw hunk.

  As built (M1.10 propose/evaluate): the coarse rule is *proposed* from
  clean pairs (or by M1.9c coarsening, below) and *claims* decomposable
  sites at evaluation (`extension` and `pattern_safe_at` count
  `ev_exact || ev_decomposable`); the existing residual re-diff emits
  each in-zone gap as a `rule=`-attributed residual with no further
  work. Single-tier only: no `after=Rn` chains, no recursive
  re-clustering of residuals yet. Tests: `ts_arg_drop_residual`,
  `ts_arg_drop_detour`, `tsx_memo_reshape_deps`; all round-trip.

- **M1.9c — Proposal-side orphan coarsening (tree embedding)
  (implemented, first cut).** When anti-unification leaves a `+`-side hole with no
  `-`-side binding (an orphan — a freshly-introduced value that varies
  per site, e.g. a `useCallback` dependency array), the coherence gate
  rejected the whole candidate. `coarsen_orphans` instead coarsens a
  varying-arity *container* orphan to its empty delimiter skeleton
  (`[]`/`()`/`{}`) — the largest common core under ordered tree
  embedding that every instance's after includes — and the M1.9b gate
  claims each site decomposably, contents falling to residuals.
  `respecialize` re-applies coarsening (it rebuilds patterns from
  concrete instances, which would resurface the orphan). Soundness is
  the gate's job — coarsening only proposes, and the net-progress guard
  is what stops it over-reaching (the emptied-function-body pathology).
  Fixture `tsx_memo_reshape_deps` pins the delivered shape: deps varying
  in arity (`[a]`/`[b, c]`/`[d, e, f]`) yield one rule with `[]` plus
  three one-line residuals. Follow-ons, in the embedding frame: the
  over-approximating direction (bind an orphan to the *whole* before
  hole it is a sub-part of), common-subsequence skeletons richer than
  empty, recursive coarsening into shared structure, and unordered
  embedding (sibling reorders — NP-complete in general, needs
  heuristics; noted, deferred).

  **Real-changeset reading (corrected).** With the guard in place the
  hook-reshape soak's output is byte-identical to the pre-M1.9b
  baseline. Two findings. (1) The verbatim-repeated reshapes were
  *already* captured as exact concrete rules by the baseline — an
  earlier "zero `useAppMemo` rules in the baseline" reading was a
  measurement artifact (a grep window too short to reach the rules'
  `+` lines). (2) The remaining reshape instances are heterogeneous
  beyond the deps orphan — different hooks, wildly different `select`
  bodies, extra object fields — so no cluster forms for coarsening to
  rescue. Capturing those needs more abstract structural clustering
  (and partial-field movement), which is the real-input frontier, not
  this mechanism.

- **M1.10 — Evaluation-based semantics (§3.3) (implemented).** Invert the back half
  of the pipeline: clustering becomes a candidate generator whose
  instance bookkeeping stays internal; every emitted rule's sites,
  support, and coverage derive from evaluating the candidate against
  all Modified files (the §3.1 gate as evaluator); emission becomes a
  greedy set-cover over changed regions. Deletes the instance-range
  covering contest, the standalone subsumption pass, and emission-side
  dedupe. Support becomes the behavioural fire count (fixture
  expectations reviewed once). Acceptance: the M1.9a leakage class is
  impossible — no residual may contain a change that a selected rule
  resolves at that file (a property test over the soak corpus, not a
  golden file).

- **M2 — Recursive residual clustering + tiered rules (implemented).** Run the
  clustering pipeline on the accumulated residuals of *all* rules
  globally — not per rule — so a secondary change shared across two
  primary clusters becomes one rule (§3.3 "common factors"). `after=`
  attribution is per-site, not per-rule; §9.3 renders it in the rule
  header when uniform, as site-line annotations when mixed. Residuals
  attributed with chains (`rule=R1,R2`).

  As built: `summarize` is a loop over `tier_rules` (the extracted
  propose/evaluate/select core). After each tier, the changeset is
  rebuilt from the (intermediate, after) pairs the rules so far leave
  unexplained — including files no rule claims, whose residuals join the
  global pool — and the loop recurses until a tier emits nothing (depth
  cap 5 and a no-progress check as backstops; each emitting tier
  strictly shrinks the gap by the net-progress guard). Application
  contract: per file, claiming rules apply in rule-id order; ids number
  across tiers, so id order is tier order. **Dead-rule pruning**: a
  tier's rules are evaluated independently against its changeset, but
  application composes sequentially — an earlier rule can consume a
  later rule's matches entirely (`f($X,$Y) ⤳ g($X)` rewrites the call
  that `f($X+1,$Y) ⤳ g($X)` would match). Such never-firing rules are
  dropped; their regions fall through to the next tier, which
  re-proposes against the actual intermediate. **Chain-effect
  accounting** extends this per site: a rule can be live at one file
  and consumed-by-an-earlier-rule at others — the fused-rescue shape
  from a real soak, where `assignee = null ⤳ assignees = emptySet()`
  is the only rule safe at a file the bare rename cannot claim (an
  unrelated `assignee` lives there), yet a no-op everywhere the rename
  runs first. After selection, the chain is applied once per file and
  every rule's `sites`, `support`, and per-site `after` are shrunk to
  where it actually edits — so support never lists a file the rule
  doesn't change, a chain-pruned rule may honestly report support
  below `min_support` (it was selected for coverage it genuinely
  provides), and residual `rule=` chains name only rules that acted on
  the file. Selection, ids, and application order are not revisited;
  the pass only makes the bookkeeping describe the chain truthfully.
  Tests: `ts_arg_drop_tiered` (the §5.2 ideal: coarse rule support 4 +
  `($X+1) ⤳ ($X)` `after=R1`, no residuals), `tsx_memo_tiered_deps`
  (reshape + shared-deps tier-2 rule, no residuals),
  `kotlin_rename_fused_rescue` (rename + fused rescue at the blocked
  file + anchored tier-2 value fix — the chain-accounting shape), plus
  direct format tests for uniform/mixed `after=` rendering.

  Two findings from building it. (1) *Aligned secondary changes are
  caught at tier 1*: multi-level change-pair emission already proposes
  an inner pair (`x+1 ⤳ x`) whenever the tree-diff aligns it, and
  selection takes both rules in one pass — tier 2 is genuinely needed
  only for gaps invisible to the tier-1 diff, chiefly content with no
  before-counterpart (a dependency array a coarsened rule writes as
  `[]`). (2) *Cross-primary common factors rarely survive the anchoring
  bar*: a minimal shared gap like `[] ⤳ [dep]` is punctuation-only on
  the match side and is rightly rejected as unanchored
  (`tsx_tier_unanchored_factor` pins the honest fallback — attributed
  per-primary residuals); anchored wider levels are primary-specific.
  Mixed per-site `after=` therefore exists in the format but no natural
  fixture produces it yet. On the real soaks M2 is byte-neutral: their
  residuals are one-offs or context-dependent renames (only some
  occurrences of an identifier renamed), which fail the placement gate
  at any tier — §3.2 contextual emission is the unlock there, and tiers
  will compound with it.

- **M2.5 — Decomposition safety as a property test (implemented).** The M1.8c gate
  enforces safety at emission time; this milestone re-states it as an
  end-to-end Tier 2 property test over the tool's actual output: for
  every rule and every site, applying the rule (and then the attached
  residual and `after=` tiers, if any) reconstructs the after-source
  exactly. Implemented as the round-trip suite in
  `tests/test_change_summary.ml`, which checks this property (comparing
  parsed trees, rules applied in id order so tiers compose) over every
  golden case; soak corpora are exercised manually.

- **M3 — Role-aware metavar naming (planned).** Use tree-sitter field names to name
  metavariables (`$function`, `$arguments`) when unambiguous.

- **M4 — Hierarchy exposure (planned).** Output format supports emitting dendrogram
  children. Test: §5.4.

- **M5 — Tuning and real-world soak (planned).** `--min-support`, `--max-hole-fraction`,
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
`changeset/` and a curated slice of the remove-redux patch, placed
under `tests/change_summary_cases/` like any other Tier 1 case. These are judged by the same hand-written-expectation
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

**Layout-only hunks are dropped from residuals.** A residual hunk is
emitted only when it touches a tree-level changed region of the
(intermediate, after) diff (`residual_diff`): re-indentation, spacing
(`{ }` vs `{}`), and line splits are invisible to the parse tree and
state nothing about the change — the reconstruction guarantee is
already modulo layout (the whole-file gap check, the gate's tree-level
re-diff). A file whose entire gap is layout emits no residual at all.
The boundary is deliberately the *tree*, not the token stream, for two
reasons. First, in newline-sensitive grammars token-stream equality
does not imply semantic equality (Kotlin's `return` + newline +
expression is not `return expression`), so a token-level criterion
could hide real changes. Second, the tool has no oracle beyond the
parse: tree-sitter-kotlin reshapes its tree when a property's
`get() = …` moves onto its own line (the getter goes from
`property_declaration` child to `class_body` sibling) even though both
forms are semantically identical Kotlin — a grammar artifact, but one
the filter cannot distinguish from a real restructure, so such hunks
are conservatively kept (noise may survive; nothing real is ever
dropped). Test: `ts_layout_residual_filtered` (a layout-only gap emits
nothing; a mixed gap keeps only its real hunk).

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

1. Parse the rule body (preamble + pattern lines, as `Matcher` does) on
   both sides.
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

### 9.4 Current subset

With M1.9a, summaries emit `rule` sections followed by `residual`
sections (no `after=` tiers yet — those arrive with M2). Every change in
the changeset appears exactly once: in a rule's effect at a claimed
site, or in a residual. The format is forward-compatible: existing
expected files stay valid when M2 introduces tiered rules.

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
  (one-step reachability, their Def. 5/6 — our §2.3 geodesic property is
  the same condition in metric phrasing) and the subpatch partial order
  (their Def. 8 — our §4.5) formally. The decisive difference is their
  Def. 7: a patch must be safe for *every* pair in the input set `C`,
  with monotonicity making `LCP(C)` collapse to ∅ when one odd pair
  joins — which is why `spfind` needed a hand-curated `C` and left a
  frequency threshold as future work. This design replaces the single
  `(gp, C)` with *discovered* `(rule_i, C_i)` pairs: clustering proposes
  partition seeds, per-site safety shedding refines each `C_i` to where
  safety actually holds (the M1.10 extension), `min_support` is their
  proposed threshold realised, and residuals account for the complement
  that Def. 7's world silently drops. Their Theorem 1 (all of `LCP(C)`
  extensionally equivalent; the quotient a join semi-lattice) is the
  formal license for selection's effect-equality treatment of
  equal-behaviour candidates. Our decomposition story corresponds to
  their sequential composition `gp_1 ; gp_2`, computed constructively
  via residual extraction rather than searched via safe-after-prefix
  extension; their exhaustive abstract-and-intersect generation remains
  the completeness gold standard within one small `C_i` and could back
  a second-chance proposer over residual clusters.
- Bader, Scott, Pradel & Chandra, *Getafix* (FSE 2019). Provides the
  anti-unification-with-memoisation primitive, the agglomerative clustering
  structure, and the hierarchy-as-output idea. Our clustering pipeline is
  essentially Getafix, minus the deployment-oriented ranking.
- Falleri et al., *GumTree* (ASE 2014). The node-mapping algorithm
  underlying `Tree_diff.compute_mapping`, used for change-pair
  extraction. (It was also the original basis for cross-side alignment;
  §4.3 now does that by content sharing over `Tree.hash` instead, with
  the GumTree mapping as a fallback oracle.)
- Miraldo & Swierstra, *An Efficient Algorithm for Type-Safe Structural
  Diffing* (`hdiff`, ICFP 2019). The basis for §4.3. Its change — a pair
  of contexts `(del, ins)` sharing metavariables — *is* our `-`/`+`
  rule. Two mechanisms transfer directly: (1) metavariables assigned by
  *content* via a "which common subtree" (`wcs`) oracle, so a
  position-misaligned cross-side value binds by construction — and the
  oracle is free for us, since hdiff's Merkle root per subtree is our
  `Tree.hash`; (2) *closure*, which enlarges a change up the spine until
  every `+`-metavariable is bound — our rule/residual boundary
  (closure-fails-at-root ⇒ residual). It diffs a single pair, so we use
  it to build cleaner per-pair changes that feed the cross-file
  generalisation (§4.1), not as a replacement; the two metavariable
  notions are duals (commonalities-within-a-pair vs.
  variabilities-across-pairs), and they conflict for sub-part reshapes
  whose surround varies across the cluster (§5.2, → residual).
- Erdweg, Szabó & Pacak, *Concise, Type-Safe, and Efficient Structural
  Diffing* (`truediff`, PLDI 2021). Linearly-typed edit scripts
  (detach/attach/load/unload/update) whose type system guarantees
  well-typed intermediate trees, in linear time. Not the §4.3 driver —
  its "reuse" is subtree *moves via URIs*, not metavariable sharing
  across a reshape — but the right reference for the *deferred*
  applicable-residual work: if residuals ever need to be type-safe
  *structured* edits rather than textual unified diffs (§9.1, the (b)
  serialization track), `truechange` is the model. It also independently
  confirms the hash oracle (it decides tree equivalence by cryptographic
  hash) and its "candidates equal-except-for-literals" is a fast
  anti-unification-by-hash primitive the proposer could borrow; heed its
  critique of hdiff (whole-file patches mention many unchanged nodes —
  moot for our *localised* rules, which want minimal context anyway).
- Kilpeläinen & Mannila, *Ordered and unordered tree inclusion* (SIAM
  J. Comput. 1995). The ordered tree embedding (`Tree_inclusion`,
  delete-only obtainability with child promotion) that the M1.9b gate
  uses as the geodesic's residual-leg test: `t''` and the after must be
  inclusion-comparable (pure insertion or pure deletion; a relabel — a
  detour — is forbidden in both directions). Ordered inclusion is
  polynomial; the unordered variant (sibling reorders) is NP-complete
  and deferred.
- Bille & Gørtz, *The Tree Inclusion Problem: In Linear Space and
  Faster* (arXiv cs/0608124). The O(n_T)-space inclusion algorithm —
  cited in `lib/tree_inclusion.ml` as the upgrade path if inclusion is
  ever hot; the implemented Kilpeläinen–Mannila-style memoised DP is
  adequate for the gate's inputs.
- Bille, *A survey on tree edit distance and related problems* (TCS
  2005). The map of the edit-distance / alignment / inclusion family.
  Inclusion is the delete-only specialization of tree edit distance;
  the recorded upgrade path for the gate — exact Zhang–Shasha TED on
  changed-region subtrees when inclusion's conservatism starts costing
  coverage (relabel-bearing or mixed insert+delete residuals) — lives
  in this map.
- Padioleau et al., *Coccinelle* / semantic patches. The output language of
  this feature — diffract's spatch DSL — is directly inspired by
  Coccinelle's SmPL. The `docs/patterns.md` file documents the concrete
  syntax.
