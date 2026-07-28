# Candidate public corpora for `summarize`

A shortlist of public codemod commits suitable as benchmark corpora for
`summarize` (researched 2026-07-03; every SHA below was verified against the
GitHub API and sample hunks were inspected). All candidates are in supported
grammars: typescript, tsx, kotlin, php, scala.

## Evaluation axes

`summarize` has two things to demonstrate, and they need different corpora:

1. **Rule recovery** — given a purely mechanical change, does it infer the
   rule(s) that generated it? Pristine tool-generated commits (tier 1) test
   this with no confound; the commit message naming the tool and rule is the
   ground truth.
2. **Decomposition** — given a *mixed* commit, does it separate the mechanical
   rule from the ride-along edits, reporting the latter as residuals?
   Residuals are a deliverable here, not noise. This is the differentiating
   capability, and it needs corpora where the *split* is documented — ideally
   by the committer — so residual precision can be checked against ground
   truth. Formatting-only ride-alongs are the least interesting kind.

The clean cases double as calibration for the mixed ones: if a pristine
single-rule commit doesn't come out as one rule with zero residuals,
residuals on a mixed corpus can't be attributed to decomposition.

## Tier 1 — pristine single-tool commits (rule recovery / calibration)

One per language:

- **twitter/finagle** `04f4281fc170990b1be475fc287a03c074c7de17` (Scala) —
  Scalafix `ProcedureSyntax`: `def f(...) { ... }` → `def f(...): Unit = { ... }`.
  61 `.scala` files, +104/−104, zero ride-along. Ground truth = exactly one
  rule. Sibling commit on the test tree:
  `1a1af42d31c986ade61f6971b18cbc767f680b2e` (64 files). Structurally a
  declaration-anchored body rewrite. Caveat: the Scala grammar is registered
  but `summarize` has barely been exercised on it.
- **symfony/symfony** `bbe96c7d7285ea6591c056182615a28e66375ec0` (PHP) —
  `strpos`/`substr` comparisons → `str_contains`/`str_starts_with`/`str_ends_with`.
  95 files, +194/−194, exactly one line out / one line in per site. Ground
  truth = 3–4 metavar rules.
- **getsentry/sentry** `19b35c757fe2f1481c61c710a95898e8fbaf414d` (TSX,
  PR #114521) — JSX prop rename `priority=` → `variant=` on
  `Button`/`LinkButton`. 36 files, +67/−67 symmetric. Sentry has ~140
  commits explicitly tagged `codemod(...)`, applied in per-team sibling
  series — a systematic source of TSX corpora at any size.
  Series anatomy (mapped 2026-07-28, 32 PRs): an unmerged first wave
  (#113817–113826), the merged per-team automated wave (#114521–114543,
  #114722 — the corpus above is the dashboards slice, now
  `evaluation/sentry-prop-rename.sh`), then three "manual pass after the
  codemod" PRs. The codemod source itself is PRIVATE
  (getsentry/design-engineering), so ground truth is inferred from the
  series + pristine diffs, unlike web-xforge's committed GritQL.
  Follow-up candidates from the manual tail — same rule hand-applied +
  documented ride-alongs (decomposition ground truth):
  - #114731 `09cf9c1dcf314f4b8a55c6d7d4896c4bbf993e60` — events/profiling/
    misc; ride-along: `playPausePriority`→`playPauseVariant` prop + type
    edit on ReplayPreviewPlayer.
  - #114732 `a1d7ade4da448bb6a9afff9afa7dbb19973a44ab` — settings/alerts/
    remaining views.

## Tier 2 — richer single-source commits

- **getsentry/sentry** `c2964b540b4d9d7ca4094741ab2836e29056529d` —
  `forwardRef` → ref prop (the official React 19 codemod). 227 `.tsx` files;
  declaration restructuring, a genuinely hard target.
- **getsentry/sentry** `9511e3318b7daa2f3a01f13529738cb67c4c75e9` —
  `${space(2)}` → theme tokens inside styled-component template literals,
  101 files. A fixed mapping table (0.5→xs, 1→md, 2→xl, …): tests whether a
  rule *family* keyed by a constant emerges.
- **tuskyapp/Tusky** `ff69a2ad0ddadba40c484879add8d80aa641f757` (Kotlin) —
  synthetics → ViewBinding, part 2 of a 6-part series. 21 `.kt` files
  (16 `.xml` ride along; restrict the corpus to `.kt`). Per-class binding
  names pressure generalization the same way import paths did on the
  AndroidX corpus.
- **playframework/play-bootstrap** `17ad9fbb01bf871c056433f8300f2022a36474e8`
  (Scala) — symbol literal `'id` → `Symbol("id")`. Only 17 files but ~300
  sites; the densest single-rule case found.
- **debiki/talkyard** `646ee978dc22912d82dcdd80862e760de967c1cd` (Scala) —
  scala-collection-compat `Collection213Upgrade`. 32 files. One scalafix
  invocation, but inherently a bundle of 2–4 sub-rewrites plus import edits:
  tests whether the bundle factors into its natural sub-rules.
- **phpmyadmin/phpmyadmin** PR #18125, merge
  `5280f044f5b1ad4745a6eda15162b3e316d50778` (PHP) — Rector
  `UseIdenticalOverEqualWithSameTypeRector` (`==` → `===`). 32 files. A
  single-token pattern; stresses the coherence/junk predicates (can a
  one-operator rewrite survive as one general rule?).

## Multi-rule commits (rule separation)

- **chipsalliance/firrtl** `fb4133cd76600cc8707e9a7b2f639cf120bd825c`
  (Scala, PR #1074) — two named scalafix rules in one commit
  (`RemoveUnused` + `ProcedureSyntax`), 72 files, plus small `.sbt`/`.md`
  ride-alongs. Ground truth = 2 rules + near-zero residual. Import-deletion
  heavy (−165/+64).
- **grafana/grafana** `49175bb2cb486977b78462cb80e04edbd9e7461c` (TS/TSX,
  PR #114575) — two React 19 codemods at once: `scoped-jsx` import insertion
  plus implicit-return ref-callback rewrites. 193 files. Mostly import-level.

## Documented mixtures (decomposition ground truth from the committer)

- **sillsdev/web-xforge** PR #3066, merge
  `b6911f236779ce3318fce926e6526aebf630035e` (TypeScript/Angular) —
  `this.subscribe(x, cb)` → `x.pipe(takeUntilDestroyed(destroyRef)).subscribe(cb)`.
  82 files. The PR states "mostly automated, though a few manual replacements
  of QuietDestroyRef with IDestroyRef had to be made", and the codemod itself
  is committed as GritQL patterns under `.grit/patterns/` in the same commit —
  machine-readable rule ground truth *and* a documented manual part. The best
  decomposition corpus found.
- **smartlabsAT/NPMDeck** `5519316f3c32bfb0b4a79e358ac5cd5d837a3916` (TSX) —
  MUI v5→v7: three named codemods (`deprecations/all`, `system-props`,
  `theme-v6`) plus the Grid-v2 migration, with the manual fixes itemized
  line-by-line in the commit message. 61 files (58 `.tsx`). Multi-rule and
  documented-manual at once.
- **ensime/ensime-server** `3d5dc6bc30ec0e947dd8319c332c78a35abeabb9`
  (Scala) — squash commit preserving sub-commit titles: "ran scalafix for a
  bunch of rules" / "manual cleanup of scalafix"; the `.scalafix.conf` added
  in the same commit records the rule set. 37 files, +43/−74.
- **astehlik/typo3-extension-mediaoembed**
  `110176022f43d58ac4c5ba8c4e38e6c1ae0ed31c` (PHP) — "manual and Rector
  refactoring", 45 files. The in-repo `rector.php` pins the rules, but the
  manual hunks are not itemized — coarser ground truth than the above.
- Smaller / conditional: **zio/zio-ftp** `34b3af9125` (Scala, 14 files,
  "Scalafix plus a bunch of manual fixes" — smoke case);
  **ForumMagnum/ForumMagnum** `929739c3905a` ("run import codemod and various
  manual fixes", 1048 `.tsx` files — usable only subsetted to a directory).
- **Kotlin gap:** no Kotlin commit with a documented split was found —
  Kotlin migrations in the wild are typically IDE-driven and undocumented
  per-commit. Kotlin decomposition coverage stays with the AndroidX corpus.

## Stress cases (large or noisy)

- **apache/daffodil** `c9ca5d6665d2d17fe637b13595a8b244584fd977` (Scala) —
  `ProcedureSyntax` over the whole codebase, 313 files. Scale test for a
  known-clean rule.
- **element-hq/element-android** PR #2542 (Kotlin) — app-wide synthetics →
  ViewBinding, 274 files across 32 WIP commits, with a genuine base-class
  refactoring riding along. Decomposition stress; too large to hand-audit.
- **phpmyadmin/phpmyadmin** `7fc96d6621ccd817c114aef197c1784b1ba6e2a9` (PHP) —
  `$this->assertX(...)` → `self::assertX(...)`, 300+ files, heavy
  formatting ride-along (multi-line calls collapsed) — needs
  `--ignore-formatting`.

## Negative results

Searched for but not found (don't re-hunt these): a clean ≥20-file
`createRoot` migration in a major TS repo; a large `@OptIn` sweep in Kotlin;
a non-comment-centric PHPUnit annotation→attribute sweep outside Drupal; a
clean mid-sized `JavaConverters` → `jdk.CollectionConverters` commit in the
major Scala orgs.

## Building a corpus from a commit

```bash
git clone --filter=blob:none <repo-url> work && cd work
git checkout <sha>^ && cp -r --parents $(git diff --name-only <sha>^ <sha> -- '*.kt') ../before/
git checkout <sha>  && cp -r --parents $(git diff --name-only <sha>^ <sha> -- '*.kt') ../after/
```

Restrict to the supported extension(s) for the language, and drop files that
exist on only one side unless add/delete handling is under test.
