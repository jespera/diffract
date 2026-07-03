# Plan: the geodesic gate (branch `change-summary-geodesic`)

Working plan and progress tracker for replacing the safety gate's
approximation strata with the metric predicate the design states, plus the
selection fix that must precede it. Lives on this branch only — at merge
time, fold the outcome into `change-summary-design.md` (§2.3/§3.1 update +
§6 milestone entry) and delete this file.

## Motivation

The gate's docstring states the safety property as a metric equation —
`d(t,t'') + d(t'',t') = d(t,t')` (cs_evaluate.ml, §2.3) — but the
implementation is a stack of approximations of it: the placement leg,
tree-inclusion comparability (leftovers must be pure-insert or
pure-delete), the before-derivedness side condition, and `net_progress`.
Each stratum was added to admit or reject a case the previous ones got
wrong, and the next candidate stratum (remaining-edits containment) was
attempted 2026-07-02 and **regressed** the androidx corpus — see "prior
findings" below. Consolidating to the definition shrinks the predicate zoo
to: `geodesic` (betweenness, one test) + `net_progress` (MDL/compactness,
a genuinely separate axis that stays).

## Baselines (main @ 03ff78f)

All 498 tests green. Corpus reference outputs regenerate with
`summarize -l <lang> -i '<glob>' [--ignore-formatting] before/ after/`:

| corpus | invocation | rules | residuals | lines | time |
|---|---|---|---|---|---|
| androidx (143 kt files) | kotlin, `--ignore-formatting` | 32 | 40 | 919 | ~43 s |
| fun-exp | kotlin, `--ignore-formatting` | 2 | 2 | — | ~31 s |
| gen3 | ts / tsx / kotlin | 47/42/— | 21/24/— | — | fast |
| drupal (scratch, shas in memory) | php, `--ignore-formatting` | 1 | 83 | — | ~2 m |

Quality meter: `androidx/ideal.summary` (hand-built ground truth;
12 high-support rules + irreducible per-class tail + curated residuals).
Known remaining gap on androidx: the lifecycle family splits 30/17 between
tier-1 R2 and a tier-2 duplicate (`import _H0.arch.lifecycle._H1`), with
`after=R1` echo stubs (R21/R23/R24) — plus the multi-import "monster"
rules (R7/R17-shape) whose applications mangle unchanged imports into
intermediates the residuals repair.

## Prior findings this plan builds on (details in session memory)

- **Disproof**: the direct lifecycle rule applies **byte-exactly** at
  ViewModelFactory.kt (one of the 17 split files) — so `ev_decomposable`
  never blocked it; the 30/17 split is a *selection bookkeeping* problem.
  Suspects: the `exempt` table (general candidate textually coinciding
  with an anchored realisation is deferred to round 2, floor 1, over
  round-1 leftovers only), `field_cand_files` scope restriction on textual
  collision, prune_dead/cover interplay.
- **Failed attempt (do not repeat as-is)**: remaining-edits containment as
  an additive decomposability disjunct. Sound and green on fixtures, but
  androidx regressed 32/40/919 → 35/46/1088, 43 s → 79 s: the route
  blesses messy-subset work (monster rules) exactly as much as
  clean-subset work, selection re-weights toward the monsters, the
  R1+echo structure returns. Lesson: **the gate defines soundness; quality
  lives in selection** — loosen the gate only after selection is healthy.

## Phase 1 — selection trace & fix

The bounded bug hunt: why does an `ev_exact` candidate lose 17 of its 47
files?

- [x] 1.1 Trace the lifecycle candidate through one androidx tier-1 run.
      Result: the candidate is healthy at every suspected stage — a plain
      general candidate (NOT exempt, NOT field-scoped, all-files),
      extension = 46 files including ViewModelFactory.kt, support 75.
      All the round-2/scoping suspects are exonerated.
- [x] 1.2 Mechanism named — TWO interacting ORDER effects downstream of
      selection:
      (E1) Application order: tier ids are assigned by support desc, so
      the broad leaf rule (`- android + androidx`, biggest extension)
      applies FIRST. At composite files where it is genuinely needed
      (an arch.core pure-flip import), it flips `android` everywhere and
      consumes the direct rule's matches — those files then need the
      tier-2 echo (`import _H0.arch.lifecycle._H1`, holed prefix
      matching both raw and flipped states).
      (E2) Minimal-claiming trial order (a581de8): at a PURE lifecycle
      file, {direct} and {echo} are both minimal identity-preserving
      claiming sets; the pass tries drops in id order, so it drops the
      leaf rule (compensated), then the DIRECT rule (echo compensates!),
      leaving the echo as survivor — hence ViewModelFactory claimed by
      the echo with no after= predecessors.
- [x] 1.3 Fix landed: Option A — `sort_for_application` in cs_select
      assigns tier ids by match-side specificity (concrete-token count,
      desc; support, then text, as tie-breaks) instead of support desc.
      Behaviour pinned by the re-blessed goldens: kotlin_rename_fused_rescue
      (fused rule now precedes the bare rename: 3 rules → 2, echo + 
      support-1 fragment gone) and ts_arg_drop_tiered (tiered factoring →
      two flat rules with disjoint sites); tsx_memo_tiered_deps still
      exercises genuine tiering unchanged. docs/change-summary.md's
      "Tiered rules" worked example rewritten accordingly.
- [x] 1.4 Gauntlet: 498 tests green. gen3 ×3 and fun-exp semantically
      IDENTICAL (same (pattern, support, sites) multisets and residual
      bodies; only rule numbering/attributions moved). androidx:
      32→29 rules, 40→33 residuals, 919→795 lines, after= 30→15,
      runtime 43s→32s. The `_H0.arch.lifecycle._H1` echo family is GONE;
      the lifecycle rule claims 46/47 files incl. ViewModelFactory (the
      holdout is TestExtensions.kt, an honest composite residual — its
      import block also gains @UiThread).

**Phase 1 exit criteria: met.** Follow-up (small, optional): the
minimal-claiming trial order (now specificity order) hands some pure-flip
files from the concrete InstantTaskExecutorRule rule to the leaf rename
(16→6 sites; leaf 3→14 across two same-text rules in different tiers) —
E2's mirror image. If it grates, Option B with an explicit
"prefer-specific" swap direction is the tool; measure first whether it
matters outside this corpus.

### Phase 1 fix options (decide before 1.3)

- **(A) Application order by specificity**: assign tier ids so more
  specific rules apply before broader ones (concrete-token count of the
  match side, descending), instead of support-descending. Kills E1 at the
  root: the direct rule fixes lifecycle lines before the leaf rule ever
  runs; echoes never arise; E2 becomes moot at these files (no echo to
  hand the file to). Reporting semantics unchanged (id order remains
  application order — ids are just assigned differently). Risks: ordering
  is corpus-global, so every multi-rule file's chain changes → golden
  churn to review; needs a specificity measure on rendered patterns
  (token count of `-` lines, metavars excluded) — crude but monotone.
- **(B) Preference-aware minimal claiming**: keep id order, extend the
  a581de8 pass with a swap step — after the drop pass, for each kept
  later rule L and dropped earlier rule E (E.id < L.id), if
  chain(kept − L + E) is identical, swap to keep E. Fixes E2 (pure files
  return to the tier-1 rule) but NOT E1 (composite files still route
  through leaf + echo, echoes survive at those sites).
- Assessment: A is the root-cause fix and likely eliminates the echo
  family entirely (Phase-1 exit criteria); B is a smaller, provably
  reconstruction-preserving patch that fixes only the pure-file half.
  A and B compose; A first, then measure whether B still has work to do.

**Exit criteria**: one tier-1 rule claims the whole 47-file lifecycle
family; R22/R21/R23/R24-shape duplicates gone or justified; no corpus
regression; expected androidx movement: rules 32 → high 20s, residual
mangle-repairs unchanged (they are Phase 3's target).

## Phase 2 — token-level metric, standalone

A true metric, cheap enough for the eval loop. Token = tree-sitter leaf
text stream (the matcher's own alphabet; whitespace-free, which dissolves
the `--ignore-formatting` interaction). Tree edit distance is out (exact
TED too slow for thousands of evals; Tree_diff is a heuristic, not a
metric — its script sizes can't carry an equality).

- [x] 2.1 `lib/leaf_metric.ml`: `leaves` (leaf-stream extraction; skips
      zero-width missing leaves, keeps comments), `distance` /
      `distance_upto` (Myers O(ND) LCS distance, prefix/suffix trim,
      per-call token interning), `geodesic ?d_endpoints ~before ~mid
      ~after ()` — both legs cut off by the triangle inequality against
      the (caller-cacheable) endpoint distance, so an off-geodesic mid
      never pays a full far-pair search.
- [x] 2.2 Unit tests (17, group "Leaf metric"): composite-subset ✓;
      mangler ✗; intra-line leaf flip ✓ (the relabel inclusion rejects);
      formatting neutrality; comment edits count; empty/identity edges;
      bound cutoffs. One correction to the plan's expectation:
      *same-position* delete-then-readd is **off**-geodesic under the LCS
      metric (each re-added token pays twice — the emptied-body soak
      shape, which inclusion blessed and only `net_progress` caught, is
      now rejected by the metric itself). Delete-then-readd of a *moved*
      element ✓ remains on-geodesic — policing metric-neutral wasted work
      is still `net_progress`'s job. Both pinned by tests.
- [x] 2.3 Micro-benchmark (androidx, 143 modified kt pairs, parses in
      hand, scaffold not committed): inclusion both ways 707 ms total
      (~4.9 ms/pair) vs leaves×2 28 ms + endpoint distance 7 ms +
      geodesic 11–12 ms (~0.3 ms/pair all-in, ~15×; the gate additionally
      caches before/after streams + endpoint distance per site, leaving
      ~0.2 ms per candidate eval). Side observation: only 4/143 pairs are
      inclusion-comparable at all, while every pair supports meaningful
      geodesic queries — the metric has strictly more resolution on real
      composite files.

**Exit criteria: met** (tests green — 515 total; per-eval cost ~15× below
the inclusion checks).

## Phase 3 — gate swap

- [x] 3.1 `decomposable := net_progress && geodesic` (cs_evaluate).
      Placement prefilter, ERROR guard, `exact` unchanged. `site_info`
      caches the before/after leaf streams and the endpoint distance;
      the per-eval cost is one `leaves(t'')` + two triangle-bounded
      Myers legs. The inclusion-both-ways disjunct and its
      before-derivedness side condition are gone from the gate
      (`string_mem` stays — cs_select's needle prefilter uses it).
- [x] 3.2 Gauntlet: 515 tests green with exactly ONE golden churn,
      re-blessed as an improvement: ts_unwrap_rename_confound — the
      unwrap rule now claims the confounded site (support 2→3) with the
      leftover rename as a rule-attributed residual; the old gate called
      that intra-node leaf flip a relabel and shed the whole site.
      Corpora: fun-exp and gen3-kotlin byte-identical; drupal
      byte-identical (140 s vs 147 s — noise); gen3-ts +1 rule,
      gen3-tsx +1 rule (both real: sites previously residual-only,
      residual counts unchanged). androidx regressed 29→33 rules,
      33→41 residuals, 795→1027 lines — the 3.3 contingency, landed
      with this measured gap on record. No eval-loop slowdown anywhere
      (androidx 35.8 s vs 40.7 s same-session pre-swap).
- [ ] 3.3 The contingency fired; cause fully attributed, counterweight
      NOT yet landed. Mechanism (traced on androidx): the geodesic
      correctly fattens decomposable extensions, which lets *umbrella
      candidates* — patterns carrying pass-through junk lines, e.g.
      `- import _H3` / `+ import _H3`, that bind fresh holes and rewrite
      nothing — reach the support floor by aggregating straggler regions
      across families. Junk lines inflate concrete-token specificity, so
      they apply FIRST (Phase-1 order), consuming the per-family rules'
      matches (lifecycle 75→68, annotation 44→42) and spawning after=
      echoes — E1's shape, re-enabled at the candidate level.
      Experiments, all measured on androidx:
      - fresh−stale cover marginal: 48 rules (fragments; family rules
        legitimately overlap each other's resolved regions) — rejected.
      - zero-stale eligibility: 50 rules — rejected.
      - dropping candidates with pass-through junk lines: 27 rules /
        37 residuals / 833 lines, families unified (lifecycle support 77,
        > Phase-1's 75 — geodesic admits it at composite files), shape
        matches ideal.summary. Confirms umbrellas are the single
        load-bearing cause. BUT the probe implementation string-scanned
        rendered pattern text for `_H` tokens — wrong layer, and a false
        positive killed drupal's one rule (a genuine `$this->_H0(...)`
        context line). Do not re-discover metavars by string-scanning.
      Follow-up (next workstream): the constraint belongs in the
      pattern/coherence layer — a structural predicate over the internal
      pattern representation ("a match-side line whose every token is a
      hole bound nowhere else and whose rewrite is the identity
      constrains only adjacency"), rejecting umbrella candidates before
      rendering. cs_pattern's coherence predicates are the natural home.

**Exit criteria**: androidx ≤ Phase-1 rule count with mangle-repair
residuals gone; gen3/fun-exp/drupal reviewed (byte-identity not required,
justified diffs only); no eval-loop slowdown beyond noise.
**Status: perf + gen3/fun-exp/drupal criteria met; androidx criterion
open, gated on the 3.3 coherence-layer follow-up.**

## Phase 4 — documentation & landing

- [ ] 4.1 `change-summary-design.md`: rewrite the §2.3/§3.1 gate story
      as geodesic(metric) + MDL guard; retire the approximation-strata
      prose; add the §6 milestone entry.
- [ ] 4.2 Update `docs/change-summary.md` if any user-visible behaviour
      shifted (rule/residual shapes in the worked examples).
- [ ] 4.3 Merge to main; delete this file (content lives in the design
      doc + git history).

## Risks

- Selection re-weighting is the theme of this whole effort: any gate
  change shifts extensions, which shifts greedy cover. Phase ordering is
  the mitigation; the corpora are the instruments.
- Golden churn in Phase 3 may be broad; each re-bless is a review
  decision, not a mechanical update.
- Metric-choice risk: leaf streams ignore tree structure, so a geodesic
  intermediate could in principle re-associate tokens across node
  boundaries; the placement leg and parse-error guard bound this. If a
  real counterexample shows up, record it here before patching.
