# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Install OCaml dependencies
opam install . --deps-only

# Build grammar libraries (TypeScript, Kotlin, JavaScript)
cd grammars && ./build-grammars.sh && cd ..

# Build the project
dune build

# Run all tests
dune test

# Run tests with verbose output
dune build @runtest --force

# Run benchmarks
dune exec benchmarks/bench_match.exe
dune exec benchmarks/bench_compare.exe -- --history   # benchmark history
dune exec benchmarks/bench_scaling.exe               # scaling analysis
```

## Project Overview

diffract is an OCaml library and CLI for parsing source files using tree-sitter and computing structural diffs. Key features:
- Parse source files to S-expressions
- Compute structural diffs between two files
- Anti-unify changes to find common transformation patterns
- Pattern matching with concrete syntax and metavariables

## Architecture

### Core Library (lib/)

- **diffract.ml** - Main module, re-exports submodules
- **tree_sitter_bindings.ml** - Low-level ctypes FFI bindings to libtree-sitter
- **ts_helper.c** - C helper layer that wraps TSNode (32-byte struct) in OCaml custom blocks to work around libffi limitations
- **node.ml** - High-level tree traversal API
- **languages.ml** - Dynamic grammar loading via dlopen
- **diff.ml** - Recursive tree diff algorithm with greedy child matching
- **antiunify.ml** - Anti-unification to find what differs between before/after
- **match.ml** - Concrete syntax pattern matching with metavariables
- **pattern.ml** - Pattern matching DSL for querying AST nodes

### Key Design Decisions

1. **TSNode handling**: Tree-sitter's TSNode is passed by value (32 bytes), which libffi doesn't support well. The C helper layer allocates nodes on the heap and wraps them in OCaml custom blocks with finalizers.

2. **Tree lifetime management**: Nodes hold a reference to their tree to prevent use-after-free when the tree is garbage collected.

3. **Grammar loading**: Grammars are loaded at runtime from .so files using dlopen. Each grammar exports a function like `tree_sitter_typescript()`.

## Adding a New Language

1. Add grammar info to `lib/languages.ml`:
```ocaml
let language_info = [
  ...
  ("ruby", ("tree-sitter-ruby", "tree_sitter_ruby"));
]
```

2. Update `grammars/build-grammars.sh` to build the grammar

3. Rebuild grammars: `cd grammars && ./build-grammars.sh`

## Pattern File Format

Pattern files use `@@` delimiters with metavariable declarations:
```
@@
metavar $obj: single
metavar $method: single
@@
$obj.$method()
```

Metavar types: `single` (one node) or `sequence` (zero or more nodes).
