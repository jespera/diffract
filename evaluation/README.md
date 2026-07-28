# Evaluation harness

Manual evaluation of `summarize` against real, public codemod commits. Each
corpus is a pinned before/after commit pair from a public repository, so every
number here is reproducible by anyone from repository history alone.

This is a manual step — it is not wired into `dune test` or any build alias.
Run it before landing a change that touches the summarize pipeline
(`lib/cs_*.ml`, `lib/tree_diff.ml`, matcher rendering).

## Workflow

```bash
# Run the fast corpora and regenerate their baselines, then review:
./evaluation/all.sh && git diff

# One corpus:
./evaluation/webxforge.sh > evaluation/webxforge.baseline

# The slow, known-fail stress corpus (deliberately not in all.sh, ~2 min):
./evaluation/drupal-attr.sh > evaluation/drupal-attr.baseline
```

A clean `git diff` on the `*.baseline` files means no behavior change. A dirty
diff is the review artifact: inspect it, and commit the new baseline together
with the code change that caused it. Re-runs are byte-stable, so any diff is
real signal, not noise.

When a number moves and you need to see *what* changed semantically, the full
summaries are left in `_work/`:

```bash
less evaluation/_work/webxforge/summary.txt
```

Baselines must be generated from a clean checkout — the script warns on stderr
if build inputs (`lib/`, `bin/`, `grammars/`, `dune-project`) are dirty.

## Reading a baseline line

```
webxforge  typescript  80 pairs  45 rules  77 residuals  3/80 factored  render/diff 0.30
```

- **pairs** — modified in-scope file pairs materialized from the commit
  (only changed files, never the whole tree; checked against
  `EXPECTED_PAIRS`).
- **rules / residuals** — rule count and residual block count in the summary.
- **factored** — files fully explained by rules (no residual) over total.
- **render/diff** — bytes of the `--format text-minimal` summary over bytes of
  the raw `diff -ruN`. A corpus-local compression ratio (lower is better),
  not a universal MDL metric.

Timings are printed to stderr only, so they never churn baselines.

## The corpora

| script | source | lang | character |
|---|---|---|---|
| `androidx.sh` | duckduckgo/Android AndroidX migration | kotlin | mechanical migration + real ride-along edits (demo corpus) |
| `webxforge.sh` | sillsdev/web-xforge DestroyRef migration | typescript | committer's GritQL codemod is in the commit (`.grit/patterns/`) — machine-readable rule ground truth + documented manual edits |
| `drupal-attr.sh` | drupal/drupal PHPUnit annotations→attributes | php | known-fail stress corpus: edits live in docblock comment tokens and pure insertions |

## Adding a corpus

Copy an existing corpus script and edit its variables — the script *is* the
manifest (repo URL, both full 40-char SHAs, include glob, language, expected
pair count, extra flags). Land each new corpus as its own commit with its
first baseline. Candidates with verified SHAs are shortlisted in
`docs/summarize-corpus-candidates.md`.

Corpora must be reproducible from public data: pinned commits in public
repositories only.

`_work/` is a gitignored materialization cache; delete a corpus's directory
under it to force a re-fetch.
