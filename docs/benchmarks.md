# Testing

Tests use the [Alcotest](https://github.com/mirage/alcotest) framework.

```bash
# Run all tests
dune test

# Run a single group (e.g. the end-to-end matcher tests)
dune exec tests/test_runner.exe -- test Matcher

# Run one named test
dune exec tests/test_runner.exe -- test Matcher "find calls"
```

Test groups (see `tests/test_runner.ml`):

- **Node**, **Tree diff**, **Grammar metadata**, **Raw vs Wrapped** — parsing,
  AST diffing, grammar metadata, raw/wrapped node handling.
- **Cursor**, **Tree_sitter_cursor** — the `Cursor.S` interface and its
  tree-sitter implementation.
- **Tokenize** — pattern-body tokenization (sigil-free metavars, ellipsis,
  fragments, missing-node handling).
- **STMatch** — the matching engine (strict / partial / field leaf-level
  matching, backtracking) over hand-built fixtures and real cursors.
- **Matcher** — end-to-end: preamble parse → tokenize → match/transform across
  the supported languages (strict, partial, field, multi-section, `foreach`,
  splice/join, removal, validation, warnings).

## Benchmarks

The old AST matcher had a `bechamel`-based benchmark harness under
`benchmarks/`. It was retired together with that matcher (it measured its
index-based API, which no longer exists). New benchmarks for the
tokenizer-based matcher have not been written yet; ad-hoc timing during
development used `Sys.time` loops over `Matcher.find`/`transform`.
