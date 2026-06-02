# diffract

An OCaml library and CLI tool for parsing source files using [tree-sitter](https://tree-sitter.github.io/tree-sitter/) and pattern matching with concrete syntax.

## Features

- Parse source using tree-sitter grammars
- Pattern matching with concrete syntax and metavariables
- Generic patch transforms in the style of Coccinelle (not-really semantic patches)
- Expansion transforms: join or restructure each element of a matched sequence
- Support for TypeScript, TSX, Kotlin, PHP, Scala (extensible)

## Building

### Prerequisites

- OCaml 5.4+
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
# Install OCaml dependencies (add --with-test to include test deps)
opam install . --deps-only --with-test

# Build grammar libraries (TypeScript, Kotlin)
cd grammars && ./build-grammars.sh && cd ..

# Build the project
dune build

# Run tests
dune test

# Format code (requires ocamlformat: opam install ocamlformat)
dune fmt
```

Formatting is managed via [ocamlformat](https://github.com/ocaml-pp/ocamlformat).
Run `dune fmt` to reformat all OCaml and dune files before committing.

Because of caching, rerunning `dune fmt` might not produce warnings for things like stray `@` in doc comments.
One can just run `ocamlformat` manually:

```
ocamlformat --check $(find lib -name '*.ml' -o -name '*.mli')
```


## Usage

```bash
# Parse and print the syntax tree
diffract parse example.ts
diffract parse --language kotlin example.kt

# Search for a pattern in a single file
diffract search pattern.txt source.ts

# Scan a directory for pattern matches
diffract search --include '*.ts' pattern.txt src/

# Scan with custom directory exclusions
diffract search --include '*.ts' -e vendor -e dist pattern.txt src/

# Show how a pattern tokenizes (diagnose a pattern that matches nothing)
diffract search --debug-tokens pattern.txt source.ts

# Apply a semantic patch (preview diff)
diffract apply patch.txt source.ts

# Apply a semantic patch in place
diffract apply --in-place patch.txt source.ts

# Apply across a directory
diffract apply --include '*.ts' patch.txt src/

# Show AST-level changes between two file versions
diffract diff before.ts after.ts

# List available languages
diffract languages
```

### Transforms (Semantic Patches)

Patterns can include `-`/`+` prefixed lines to describe code transformations.
For example, to rename `console.log` to `logger.info`:

**patch.txt:**
```
@@
match: strict
metavar $MSG: single
@@
- console.log($MSG)
+ logger.info($MSG)
```

```bash
$ diffract apply patch.txt source.ts
--- a/source.ts
+++ b/source.ts
@@ -1,3 +1,3 @@
 function greet(name: string) {
-    console.log(name);
+    logger.info(name);
 }
```

Lines prefixed with `- ` are matched and removed; lines with `+ ` are inserted.
Unprefixed (or space-prefixed) lines are context that appears in both match and replace.
Metavariables carry values from the match side to the replace side.

### Sequence rendering: `join` and `foreach`

A `sequence` metavar referenced in a `+` template is rendered and substituted
in place. Two knobs: a `join $VAR by "<sep>"` preamble directive sets the
separator between elements (default: empty), and a following `foreach $VAR`
section applies a per-element transform.

For per-element transforms (e.g. converting a match expression to a method
chain), use a two-section pattern with `foreach`:

```
@@
match: strict
metavar $TAG: single
metavar $CASES: sequence
@@
- matchExhaustive($TAG, { $CASES });
+ match($TAG)$CASES.exhaustive();
@@
match: strict
foreach $CASES
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ .with("$KEY", $VAL)
```

Applied to:
```typescript
matchExhaustive(tag, { A: () => 1, B: () => 2 });
```

Produces:
```typescript
match(tag).with("A", () => 1).with("B", () => 2).exhaustive();
```

See [Transform documentation](docs/patterns.md#transforms-semantic-patches) for
partial-mode, field-mode, and sequence transforms.

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
- [Testing](docs/benchmarks.md)
- [The universal tokenizer matcher](docs/universal-tokenizer.md)

## Matcher Architecture (Quick Overview)

The matching pipeline is split into focused modules:

- `tokenize` parses a pattern body with tree-sitter as a *lexer*, keeping leaves as a `(text, node_type)` token stream (sigil-free metavars, ellipsis, fragments).
- `cursor` / `tree_sitter_cursor` define and implement the tree-cursor interface the engine runs over.
- `stmatch` is the matching engine: leaf-level `strict` / `partial` / `field` matching with backtracking, over any `Cursor.S`.
- `matcher` ties it together — preamble parse → tokenize → match → transform — and exposes the public `find` / `transform` / `debug_tokens` / `pattern_warnings` API.

## License

GPL-3.0-or-later
