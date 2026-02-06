# Repository Guidelines

## Project Structure & Module Organization
- `lib/`: core OCaml library (tree-sitter bindings, tree model, matching engine).
- `bin/`: CLI entry point (`main.ml`).
- `grammars/`: tree-sitter grammars and build script (`build-grammars.sh`), compiled `.so` in `grammars/lib/`.
- `tests/`: Alcotest unit tests (see `tests/test_runner.exe`).
- `benchmarks/`: performance benchmarks and results.
- `docs/`: internal architecture and pattern format references.

## Build, Test, and Development Commands
- `opam install . --deps-only --with-test`: install OCaml deps (include test/benchmark deps).
- `cd grammars && ./build-grammars.sh && cd ..`: build grammar `.so` libraries (requires `npm`).
- `dune build`: build library + CLI.
- `dune test`: run full test suite.
- `dune exec tests/test_runner.exe -- test Match`: run a test group.
- `dune exec tests/test_runner.exe -- test Match "simple expression match"`: run a named test.
- `dune exec benchmarks/bench_match.exe`: run pattern-matching benchmarks.

## Coding Style & Naming Conventions
- Language: OCaml (plus small C helper in `lib/ts_helper.c`).
- Indentation: follow existing OCaml style in `lib/` and `tests/` (2-space indentation).
- Naming: modules and functions use `snake_case`; types and modules use `CamelCase` when appropriate.
- No repo-wide formatter is configured; keep diffs consistent with nearby code.

## Testing Guidelines
- Framework: Alcotest (see `tests/` and `docs/benchmarks.md`).
- Test names are human-readable strings in `Alcotest.test_case` (e.g., "format_match output").
- Run all tests with `dune test`, or target groups via `tests/test_runner.exe`.

## Commit & Pull Request Guidelines
- Commit messages in history are short, imperative statements (e.g., "Improve language listing"). Follow that style.
- PRs should include a clear description of behavior changes, test coverage notes, and relevant command output when benchmarks or tests are affected.

## Architecture Notes
- Core parsing flow: tree-sitter -> C helper wrappers -> OCaml bindings -> pure OCaml tree (`lib/tree.ml`).
- Grammar libraries are loaded at runtime from `grammars/lib/`; rebuild when adding languages.
