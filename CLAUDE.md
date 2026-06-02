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

# Run specific test group (e.g., just the end-to-end Matcher tests)
dune exec tests/test_runner.exe -- test Matcher

# Run a single named test
dune exec tests/test_runner.exe -- test Matcher "find calls"
```

## Project Overview

diffract is an OCaml library and CLI for parsing source files with tree-sitter and pattern matching. Key capabilities:
- Parse source to S-expressions using tree-sitter grammars
- Pattern matching with concrete syntax and metavariables

## Architecture

**Core Library (`lib/`)**
- `diffract.ml` - Main module, re-exports submodules
- `tree.ml` - Pure OCaml tree representation (eliminates FFI overhead during traversal)
- `tree_sitter_bindings.ml` - Low-level ctypes FFI bindings
- `tree_sitter_helper.c` - C helper layer wrapping TSNode in OCaml custom blocks (libffi can't handle 32-byte structs by value)
- `node.ml` - FFI-based tree traversal (internal, used during parsing)
- `languages.ml` - Static grammar registry (language name → `external` C binding)

**Matcher (`lib/`)** — the tokenizer-based matcher (the only matcher):
- `tokenize.ml` - Parse a pattern body with tree-sitter as a *lexer*, keeping leaves; produces a `pattern_token` stream (sigil-free metavars, ellipsis, fragments)
- `cursor.ml` - Abstract tree-cursor interface (`Cursor.S`) the matching engine runs over
- `tree_sitter_cursor.ml` - `Cursor.S` over a real tree-sitter parse
- `stmatch.ml` - The matching engine: strict/partial/field leaf-level matching with backtracking (`Make` functor over a `Cursor.S`)
- `matcher.ml` - End-to-end: preamble parse → tokenize → match → transform; the public `find`/`transform`/`debug_tokens`/`pattern_warnings` API
- `text_diff.ml` - Line-based unified diff (for `apply`'s output)

**Tree-sitter Integration Flow:**
1. C helper layer wraps TSNode/TSTree in OCaml custom blocks with finalizers
2. ctypes-foreign binds to libtree-sitter and C helpers
3. `languages.ml` dispatches to grammar language functions statically linked into the binary
4. `tree.ml` converts FFI nodes to pure OCaml representation once during parsing

**Matching Flow:** a pattern's preamble (`@@` sections) is parsed, its body is
tokenized into a `(text, node_type)` leaf stream, and `stmatch` walks the
source tree (via a `Cursor.S`) matching those tokens — comparing leaves on
both text and node type. Metavars are sigil-free (a leaf is a metavar iff its
text equals a declared name). See `docs/universal-tokenizer.md`.

## Adding a New Language

1. Add C wrapper and `external` binding to `lib/tree_sitter_helper.c` and `lib/languages.ml`:
```c
/* lib/tree_sitter_helper.c */
extern const TSLanguage *tree_sitter_ruby(void);
CAMLprim value tsh_ruby_language(value v_unit) {
    CAMLparam1(v_unit); CAMLreturn(caml_copy_nativeint((intnat)tree_sitter_ruby())); }
```
```ocaml
(* lib/languages.ml *)
external ruby_language : unit -> nativeint = "tsh_ruby_language"
(* add to canonical_info: ("ruby", [], ruby_language) *)
```

2. Update `grammars/build-grammars.sh` with the compilation command:
```bash
npm install tree-sitter-ruby
cc -O2 -c -o "$TMPDIR_LOCAL/ruby_parser.o" \
  -I node_modules/tree-sitter-ruby/src \
  node_modules/tree-sitter-ruby/src/parser.c
cc -O2 -c -o "$TMPDIR_LOCAL/ruby_scanner.o" \
  -I node_modules/tree-sitter-ruby/src \
  node_modules/tree-sitter-ruby/src/scanner.c
ar rcs lib/libtree-sitter-ruby.a "$TMPDIR_LOCAL/ruby_parser.o" "$TMPDIR_LOCAL/ruby_scanner.o"
```

3. Add copy rule and `(foreign_archives ...)` entry to `lib/dune`:
```
(rule (target libtree-sitter-ruby.a)
 (deps ../grammars/lib/libtree-sitter-ruby.a)
 (action (copy %{deps} %{target})))
```
And add `tree-sitter-ruby` to the `(foreign_archives ...)` list.

4. Rebuild: `cd grammars && ./build-grammars.sh && cd .. && dune build`

## Pattern File Format

Patterns use `@@` delimiters with a **required** match mode and optional metavariable declarations:
```
@@
match: strict
metavar $obj: single
metavar $method: single
@@
$obj.$method()
```

Types: `single` (one AST node), `sequence` (zero or more nodes)

Ellipsis (`...`) can be used as anonymous sequence matching:
```
@@
match: strict
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
- Sequence metavars (including `...`) are not supported with `match: partial`.

Matching modes (required - must specify one):
- `match: strict` - Exact positional matching (no extra children allowed, ordered). Use for function calls, arrays.
- `match: partial` - Subset matching (ignores extra children, unordered). Use for object literals, JSX attributes.
- `match: field` - Declaration matching that ignores optional fields the pattern omits (decorators, annotations, modifier groups, return types). The pattern's leaf stream is aligned to a *subsequence* of the declaration node's children: a child the pattern addresses is matched in full, a child it omits is skipped. Use for decorated/annotated definitions. (No per-language config — see `docs/field-mode.md`.) **Transform caveat:** a field (or partial) section that replaces the whole matched span drops the very fields it ignores; the CLI warns. To edit part of a declaration while keeping the rest, use a `foreach`/`on` section.

### Conjunctive sibling sections

A pattern file with multiple `@@` sections and **no** `on $VAR` directives is a
conjunctive rule: every section must find at least one match for any transforms
to fire. If any section finds nothing, the source is returned unchanged.
Metavars of the same name across sections refer to the same binding (threaded
in declaration order); a section without `-`/`+` lines acts as a pure guard
(must match but produces no edits). `Matcher.find`/`Matcher.transform` handle
multi-section patterns directly.

### Sequence rendering: `join` and `foreach`

When a `sequence` metavar is referenced inside a `+` replacement template, its
elements are rendered and substituted in place. Two independent knobs control
this:

- **`join $VAR by "<sep>"`** (a preamble directive): the string placed between
  rendered elements. `<sep>` interprets `\n`, `\t`, `\\`. The default (no
  directive) is the empty string, so elements are concatenated.
- **`foreach $VAR`** (a following `@@` section): a per-element transform. The
  section matches one element of `$VAR` and its `-`/`+` lines rewrite it; the
  rewritten elements are then joined. Without a `foreach`, elements render as
  their source text (identity).

The sequence metavar must be declared `metavar $VAR: sequence` and appear on the
match side. Everything goes through `Matcher.transform`.

#### Example — identity render with a join separator

```
@@
match: strict
metavar $ELEMS: sequence
join $ELEMS by " && "
@@
- all([$ELEMS])
+ ($ELEMS)
```

`all([x, y, z])` becomes `(x && y && z)`.

#### Example — method chain (`foreach` per-element transform)

```
@@
match: strict
metavar $TAG: single
metavar $PROPS: sequence
@@
- matchExhaustive($TAG, { $PROPS });
+ match($TAG)$PROPS.exhaustive();
@@
match: strict
foreach $PROPS
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ .with("$KEY", $VAL)
```

`matchExhaustive(tag, { a: f, b: g });` becomes
`match(tag).with("a", f).with("b", g).exhaustive();` — the `foreach` section
rewrites each property, and `$PROPS` in the outer template is replaced by the
joined results (default empty join here).

A `foreach` element with an empty replacement (a `-` line, no `+`) deletes the
element and cleans up the adjacent separator — e.g. removing a deprecated
property or unused argument from a list.
