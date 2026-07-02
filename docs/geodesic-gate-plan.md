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

- [ ] 1.1 Trace the lifecycle candidate through one androidx tier-1 run
      (CS_TRACE + targeted temp instrumentation): proposed? deduped?
      exempt? file scope? extension size? selection round + marginal?
- [ ] 1.2 Name the mechanism; write it down (memory + this file).
- [ ] 1.3 Fix, with a small golden fixture pinning the behaviour
      (mirror the kotlin_minimal_claiming recipe: needs a file keeping the
      broad rule alive, a composite file, and a pure file).
- [ ] 1.4 Gauntlet: `dune test`; gen3 ×3, fun-exp, drupal byte-compared;
      androidx reviewed qualitatively.

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

- [ ] 2.1 `lib/leaf_metric.ml` (name TBD): leaf-stream extraction, Myers
      O(ND) distance, `geodesic ~before ~mid ~after` with the
      per-file `d(before, after)` cached by the caller.
- [ ] 2.2 Unit tests: composite-subset ✓; mangler ✗ (invented text);
      intra-line partial step ✓ (leaf flip); delete-then-readd ✓ by
      geodesic (rejecting it is `net_progress`'s job — test documents the
      division of labour); formatting neutrality; empty/identity edges.
- [ ] 2.3 Micro-benchmark vs the two `Tree_inclusion` calls it replaces,
      on corpus-sized files.

**Exit criteria**: unit tests green; per-eval cost ≤ current inclusion
checks.

## Phase 3 — gate swap

- [ ] 3.1 Replace the decomposable disjunction (inclusion both ways +
      before-derivedness) with `net_progress && geodesic`; keep the
      placement prefilter, the ERROR well-formedness guard, and `exact`
      unchanged.
- [ ] 3.2 Full gauntlet. Golden churn is *expected* — review each changed
      fixture and re-bless deliberately. Corpus diffs reviewed; androidx
      measured against ideal.summary; eval-loop perf measured (the gate
      runs thousands of times).
- [ ] 3.3 Contingency if monsters resurface (they are on-geodesic):
      selection-side counterweight — e.g. weighting cover by resolved
      fraction or preferring `ev_clean` more aggressively. Only with
      Phase 1 landed can this be attributed cleanly.

**Exit criteria**: androidx ≤ Phase-1 rule count with mangle-repair
residuals gone; gen3/fun-exp/drupal reviewed (byte-identity not required,
justified diffs only); no eval-loop slowdown beyond noise.

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
