# diffract

An OCaml library and CLI tool for parsing source files using [tree-sitter](https://tree-sitter.github.io/tree-sitter/) and pattern matching with concrete syntax.

## Features

- Parse source files to S-expressions using tree-sitter grammars
- Pattern matching with concrete syntax and metavariables
- Support for TypeScript, Kotlin, and other languages (extensible)

## Building

### Prerequisites

- OCaml 5.2+
- opam
- tree-sitter library (`libtree-sitter`) - see below
- npm (to fetch grammar sources)

#### Installing tree-sitter

**macOS:**
```bash
brew install tree-sitter
```

**Ubuntu/Debian:**
```bash
sudo apt install libtree-sitter-dev
```

**Arch Linux:**
```bash
sudo pacman -S tree-sitter
```

### Build Steps

```bash
# Install OCaml dependencies (add --with-test to include test/benchmark deps)
opam install . --deps-only --with-test

# Build grammar libraries (TypeScript, Kotlin)
cd grammars && ./build-grammars.sh && cd ..

# Build the project
dune build

# Run tests
dune test
```

## Usage

```bash
# Parse and print parsed tree
diffract example.ts

# Parse with explicit language
diffract --language kotlin example.kt

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
| `--exclude DIR` / `-e` | Directory names to skip (repeatable). Defaults: `node_modules`, `.git`, `_build`, `target`, `__pycache__`, `.hg`, `.svn` |

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

## Match Architecture (Quick Overview)

The matching pipeline is split into focused modules:

- `match_parse` handles `@@` preambles, metavars, and ellipsis expansion, then parses with tree-sitter.
- `match_engine` performs the structural matching (`strict`, `field`, `partial`) and sequence metavars.
- `match_search` drives traversal, nested pattern contexts, indexing, and formatting.
- `match` exposes the public API surface.

## License

GPL-3.0-or-later
