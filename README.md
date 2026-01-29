# diffract

An OCaml library and CLI tool for parsing source files using [tree-sitter](https://tree-sitter.github.io/tree-sitter/) and computing structural diffs between them.

## Features

- Parse source files to S-expressions using tree-sitter grammars
- Compute structural diffs between two source files
- Anti-unify changes to find common transformation patterns across multiple files
- Pattern matching with concrete syntax and metavariables
- Support for TypeScript, Kotlin, and other languages (extensible)

## Building

### Prerequisites

- OCaml 5.2+
- opam
- tree-sitter library (`libtree-sitter`)
- Node.js (for building grammar libraries)

### Build Steps

```bash
# Install OCaml dependencies
opam install . --deps-only

# Build grammar libraries (TypeScript, Kotlin)
cd grammars && ./build-grammars.sh && cd ..

# Build the project
dune build

# Run tests
dune test
```

## Usage

```bash
# Parse a file to S-expression
diffract example.ts

# Parse with explicit language
diffract --language kotlin example.kt

# Diff two files
diffract --diff before.ts after.ts

# Diff with flat output (one change per line)
diffract --diff --flat before.ts after.ts

# Diff with anti-unified output (shows [before → after] for changes)
diffract --diff --antiunify before.ts after.ts

# Match a pattern against a single file
diffract --match pattern.txt source.ts

# Scan a directory for pattern matches
diffract --match pattern.txt --include '*.ts' src/

# Scan with custom directory exclusions
diffract --match pattern.txt --include '*.ts' -e vendor -e dist src/

# List available languages
diffract --list-languages
```

### Directory Scanning

When the target is a directory, use `--include` to specify which files to scan:

| Option | Description |
|--------|-------------|
| `--include GLOB` / `-i` | Glob pattern for files (e.g., `*.ts`, `*.py`). Required for directories. |
| `--exclude DIR` / `-e` | Directory names to skip (repeatable). Defaults: `node_modules`, `.git`, `_build`, `target`, `__pycache__` |

Supported glob patterns:
- `*.ts` - files ending with `.ts`
- `prefix*` - files starting with `prefix`
- `*suffix` - files ending with `suffix`

**Example output:**
```
src/api/auth.ts:15: console.log("login")
  $msg = "login"

src/utils/logger.ts:8: console.log("initialized")
  $msg = "initialized"

Found 2 match(es) in 2 file(s) (scanned 47 files)
```

## Documentation

- [Pattern format and library API](docs/patterns.md)
- [Architecture and internals](docs/internals.md)
- [Testing and benchmarks](docs/benchmarks.md)

## License

GPL-3.0-or-later
