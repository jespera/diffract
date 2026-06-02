# Benchmarks

Performance benchmarks for diffract. Not part of the test suite — these
are development tools for sanity-checking that performance-sensitive
changes don't regress, and for grounding "should we optimize this?"
decisions in actual measurements rather than intuition.

## Why this directory exists

Optimization decisions in diffract should be backed by data. Examples of
questions these benchmarks answer:

- Is the matcher fast enough on realistic source sizes, or do we need to
  add an index?
- Does a refactor that "should be the same speed" actually preserve
  performance?
- How does cost scale with source size — linearly, super-linearly?
- Which patterns are cheap and which are expensive, and why?

Without measurements, it's easy to add complexity that turns out not to
matter (anchor search would be ~150 LOC of code for a 3× speedup on
patterns that already run in 10ms — not worth it), or to miss
regressions that ship.

## Running

From the project root:

```
dune exec bench/find_matches.exe
```

Output is printed to stdout. No special flags or environment.

Re-running is cheap (a few seconds total). Run multiple times if you
care about noise — single runs vary 10-30% depending on what else the
machine is doing.

## Current benchmarks

### `find_matches.ml` — outer-loop performance

Measures `Stmatch.Make(Tree_sitter_cursor).find_matches` against
synthetic TypeScript sources of increasing size (100, 500, 2000, 10000
"blocks"; each block contributes ~5 statements, so 10000 blocks ≈ 750KB
and ~366K AST nodes).

For each size it tries several pattern shapes:

| Pattern | Purpose |
|---|---|
| `foo($x)` | Distinctive-first Concrete; the kind of pattern where anchor search would help most |
| `bar($x)` | Common-first Concrete; matches every `bar` call (10× more frequent than `foo`) |
| `$fn($x)` | Wildcard-first; matches every call expression in source |
| `baz($x, $y)` | Longer pattern (more tokens, two wildcards) |
| `pair($x, ..., $x)` | Non-linear pattern within an argument list (the two `$x` must bind to equal subtrees) |

The output table shows pattern name, source size in nodes and bytes,
elapsed wall-clock time in milliseconds, and number of matches found.

### What to look for

- **Linear scaling.** Time should grow roughly proportional to node
  count. Super-linear growth would signal an algorithmic problem.
- **Match-count dominates over pattern complexity.** Patterns producing
  many matches (e.g., wildcard-first matching every call) take longer
  than patterns producing few. Pattern length / wildcard count adds
  only modest overhead.
- **Non-linearity should be cheap.** `subtree_equal` uses hash-based
  fast rejection (`Tree.hash`), so non-linear patterns shouldn't be
  much slower than equivalent patterns without name reuse.

### Reference numbers

As of the initial commit (brute-force outer loop, no anchor search),
on a typical development machine:

| Size | Time range across patterns |
|---|---|
| 100 blocks (~3.7K nodes, 6KB) | 1-3 ms |
| 500 blocks (~18K nodes, 33KB) | 4-9 ms |
| 2000 blocks (~73K nodes, 142KB) | 15-34 ms |
| 10000 blocks (~366K nodes, 743KB) | 97-322 ms |

These were the numbers that led to the decision to skip anchor search:
even 750KB synthetic sources complete in <500ms with the simplest
implementation. Reasonable file sizes are sub-50ms.

## Adding benchmarks

When to add one:

- After a phase that touches the matcher's hot path (match modes,
  named bindings, etc.), to verify no regression.
- Before deciding whether to optimize. If you find yourself proposing
  an optimization, write the benchmark first.
- When investigating a specific reported slowness. Reproduce the
  scenario as a synthetic source, time it, then iterate.

What makes a good benchmark:

- **Scales with input size.** Pick at least 3-4 sizes spanning 10-100×
  to verify scaling behavior.
- **Realistic-ish source.** Pure synthetic (`foo(1); foo(2); ...`)
  exercises one shape; mixed synthetic exercises more.
- **Reports actionable numbers.** Wall-clock time in ms, plus context
  (size, match count). Don't just print "fast" or "slow."
- **Self-contained.** No external files or environment setup required.

When NOT to add one:

- For unit-test-scale fixtures. Use `dune test` for correctness; this
  directory is for *performance*.
- For micro-benchmarks of individual cursor operations. The matcher's
  end-to-end behavior is what users see.

## Removing benchmarks

If a benchmark stops being relevant (e.g., the code path it measures
gets deleted), remove it. The directory should reflect what we
currently care about measuring, not the historical accumulation.
