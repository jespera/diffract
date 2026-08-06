# Evaluation harness

Manual evaluation of `summarize` against real, public codemod commits. Each
corpus is a pinned before/after commit pair from a public repository, so every
number here is reproducible by anyone from repository history alone.

This is a manual step - it is not wired into `dune test` or any build alias.
Run it before adding a change that touches the summarize pipeline
(`lib/cs_*.ml`, `lib/tree_diff.ml`, matcher rendering).

## Workflow

```bash
# Run the corpora and regenerate their baselines, then review:
./evaluation/all.sh && git diff

# One corpus:
./evaluation/webxforge.sh > evaluation/webxforge.baseline

# The slow, known-fail stress corpus (deliberately not in all.sh, ~2 min):
./evaluation/drupal-attr.sh > evaluation/drupal-attr.baseline
```

A clean `git diff` on the `*.baseline` files means no behavior change. A dirty
diff is the review artifact: inspect it, and commit the new baseline together
with the code change that caused it. Re-runs should produce the same results,
so any diff is real signal, not noise.

When a number moves and you need to see *what* changed semantically, the full
summaries are left in `_work/`:

```bash
less evaluation/_work/webxforge/summary.txt
```

Baselines must be generated from a clean checkout - the script warns on stderr
if build inputs (`lib/`, `bin/`, `grammars/`, `dune-project`) are dirty.

## Reading a baseline line

```
webxforge  typescript  80 pairs  43 rules  77 residuals  3/80 factored  render/diff 0.16
```

- **pairs** - in-scope file pairs materialized from the commit (only changed
  files, never the whole tree; checked against `EXPECTED_PAIRS`).
- **rules / residuals** - rule count and residual block count in the summary.
- **factored** - files fully explained by rules (no residual) over total.
- **render/diff** - bytes of the `--format text-minimal` summary over bytes of
  the corpus's raw unified diff. A corpus-local compression ratio (lower is
  better), not a universal MDL metric.

  Since the residual digest landed, the numerator is a *digested* render:
  moved files collapse into a rename table and repeated hunks into groups. So
  the ratio now reflects how repetitive the residuals are as well as how well
  rules factored the change, and it dropped sharply on rename-heavy corpora
  (pekko 0.38 to 0.01) where it had largely been measuring the same long path
  repeated in each residual's git header. Numbers are comparable across runs,
  not across that change.

Timings are printed to stderr only, so they never churn baselines.

A corpus with a holdout (currently `pekko.sh`) adds a second half to the line:

```
pekko  scala  131 pairs  3 rules  131 residuals  86/131 factored  render/diff 0.01  holdout 57: 10 exact  43% closed  2 regressed
```

Everything before `holdout` measures how well the rules fit the files they were
*derived from* - which a sufficiently over-specific rule set always aces. The
holdout measures whether they generalize: a slice of the same commit that
summarize never saw, with the derived rules applied to its before-state and the
result compared against its real after-state.

- **exact** - holdout files the rules reconstruct byte-for-byte.
- **closed** - share of the holdout's total diff bytes the rules eliminate.
  Partial is the expected outcome: a holdout has its own ride-along edits, and
  rules anchored to the derivation slice's context legitimately miss some sites.
- **regressed** - files the rules move *further* from their target. **This one
  should be zero.** Coverage numbers are a matter of degree; a regression means
  a rule is over-firing, which is a correctness signal.

## Rename-aware corpora

Two directory trees cannot say "this before-file is that differently-named
after-file", so a renamed file reads as an unrelated deletion plus an unrelated
addition, and the systematic edits inside it yield no rules at all. A corpus
whose commit renames files sets `RENAMES=1`, which materializes through the
shipped extractor (`scripts/diffract-checkout.sh`) and runs `summarize --pairs`
against the manifest it writes. That keeps the extractor exercised on every
harness run, and it is what makes `pekko.sh` produce rules rather than 262
whole-file `/dev/null` residuals.

## The corpora

| script | source | lang | character |
|---|---|---|---|
| `androidx.sh` | duckduckgo/Android AndroidX migration | kotlin | mechanical migration + real ride-along edits (demo corpus) |
| `webxforge.sh` | sillsdev/web-xforge DestroyRef migration | typescript | committer's GritQL codemod is in the commit (`.grit/patterns/`) - machine-readable rule ground truth + documented manual edits |
| `drupal-attr.sh` | drupal/drupal PHPUnit annotations→attributes | php | known-fail stress corpus: edits live in docblock comment tokens and pure insertions |
| `sentry-prop-rename.sh` | getsentry/sentry `codemod(button-variant)` | tsx | calibration corpus: a JSX prop rename plus a value-remap sub-rule, an object-literal form, and one deliberate exclusion (`<Confirm>` keeps its prop - over-fire bait for the gate); fully factored since the JSX ellipsis-context fix |
| `sentry-prop-manual.sh` | getsentry/sentry PR #114731, the manual pass after the same codemod | tsx | decomposition corpus: same rename hand-applied plus a documented ride-along (`playPausePriority`→`playPauseVariant` + a type edit) that should factor separately from the main rule |
| `finagle.sh` | twitter/finagle scalafix `ProcedureSyntax` | scala | calibration corpus and the first Scala one: `def f(...) { ... }` → `def f(...): Unit = { ... }`, zero ride-along; ground truth = one rule (residuals to date are its curried and abstract-def variants, not noise) |
| `symfony-str.sh` | symfony/symfony strpos/substr → `str_contains`/`str_starts_with`/`str_ends_with` | php | the PHP counterweight to drupal-attr: pristine one-line-per-site expression rewrites; ground truth ≈ 10 form rules (3 target functions × positive/negated × strpos/substr sources); slow (~3 min/format), so not in all.sh |
| `daffodil.sh` | apache/daffodil scalafix `ProcedureSyntax` (whole codebase) | scala | scale corpus: the same rewrite as finagle at 5× the files and ~4500 sites; motivated the dendrogram bucket cap (discovery samples, the gate assigns support — the top rule carries support 3295); slow (~3 min/format), so not in all.sh |
| `pekko.sh` | apache/pekko `akka` → `org.apache.pekko` package rename | scala | the rename corpus: every in-scope file moves, so path pairing yields *zero* rules and 262 `/dev/null` residuals - `RENAMES=1` is what makes it a corpus at all. Also the only one with a holdout slice. Sliced to one module of a 3,474-file commit; the residual tail is the sub-token gap (`docs/subtoken-rename-generalization.md`), not noise |

## Adding a corpus

Copy an existing corpus script and edit its variables - the script *is* the
manifest (repo URL, both full 40-char SHAs, include glob, language, expected
pair count, extra flags). Include a link to where the change is discussed -
the PR, or the issue/commit where no PR exists - so the corpus is easy to
find when browsing, and its review comments stay one click away. Add each
new corpus as its own commit with its first baseline.
Candidates with verified SHAs are shortlisted in `docs/summarize-corpus-candidates.md`.

Corpora must be reproducible from public data: pinned commits in public
repositories only.

`_work/` is a gitignored materialization cache; delete a corpus's directory
under it to force a re-fetch.

## Reference material (`refs/`)

- `refs/androidx.ideal.summary` — hand-built ground truth for the androidx
  corpus (12 high-support rules + per-class rename table + curated
  residuals, with expressiveness-gap notes). Hand-derived, so kept in-repo;
  it is not reproducible from the commits alone.
- The webxforge corpus's ground truth (the committer's GritQL codemod) is
  in the pinned commit itself and needs no local copy:

  ```bash
  git -C evaluation/_work/webxforge/repo show \
    b6911f236779ce3318fce926e6526aebf630035e:.grit/patterns/take_until_destroyed.md
  ```
