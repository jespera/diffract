# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Install OCaml dependencies
opam install . --deps-only

# Build grammar libraries (requires npm)
cd grammars && ./build-grammars.sh && cd ..

# Build the project
dune build

# Run all tests
dune test

# Run specific test group (e.g., just Match tests)
dune exec tests/test_runner.exe -- test Match

# Run a single named test
dune exec tests/test_runner.exe -- test Match "simple expression match"
```

## Project Overview

diffract is an OCaml library and CLI for parsing source files with tree-sitter and computing structural diffs. Key capabilities:
- Parse source to S-expressions using tree-sitter grammars
- Compute structural diffs between files
- Anti-unify changes to find common transformation patterns
- Pattern matching with concrete syntax and metavariables

## Architecture

**Core Library (`lib/`)**
- `diffract.ml` - Main module, re-exports submodules
- `tree.ml` - Pure OCaml tree representation (eliminates FFI overhead during traversal)
- `tree_sitter_bindings.ml` - Low-level ctypes FFI bindings
- `ts_helper.c` - C helper layer wrapping TSNode in OCaml custom blocks (libffi can't handle 32-byte structs by value)
- `node.ml` - FFI-based tree traversal (internal, used during parsing)
- `languages.ml` - Dynamic grammar loading via dlopen
- `diff.ml` - Tree diff algorithm (recursive comparison with greedy child matching)
- `antiunify.ml` - Anti-unification for finding what differs between before/after
- `match.ml` - Index-based pattern matching with concrete syntax
- `pattern.ml` - Pattern matching DSL

**Tree-sitter Integration Flow:**
1. C helper layer wraps TSNode/TSTree in OCaml custom blocks with finalizers
2. ctypes-foreign binds to libtree-sitter and C helpers
3. `languages.ml` loads grammar .so files at runtime via dlopen
4. `tree.ml` converts FFI nodes to pure OCaml representation once during parsing

## Adding a New Language

1. Add grammar info to `lib/languages.ml`:
```ocaml
let language_info = [
  ...
  ("ruby", ("tree-sitter-ruby", "tree_sitter_ruby"));
]
```

2. Update `grammars/build-grammars.sh` with the compilation command

3. Rebuild: `cd grammars && ./build-grammars.sh`

## Pattern File Format

Patterns use `@@` delimiters with metavariable declarations:
```
@@
metavar $obj: single
metavar $method: single
@@
$obj.$method()
```

Types: `single` (one AST node), `sequence` (zero or more nodes)

Ellipsis (`...`) can be used as anonymous sequence matching:
```
@@
@@
<?php
function test() {
    ...
    echo "middle";
    ...
}
```
- `...` matches zero or more nodes (like sequence metavars)
- Auto-detects context: adds `;` in statement position, not in argument position
- Does NOT replace `...$var` (PHP spread operator is preserved)
- Each `...` gets a unique binding name (`..._0`, `..._1`, etc.)

Matching modes:
- `match: partial` - Subset matching (ignores extra children, unordered)
- `match: field` - Field-based matching (matches children by tree-sitter field name instead of position, ignores extra source fields not in pattern, preserves order within each field)
