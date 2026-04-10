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
- `match.ml` - Index-based pattern matching with concrete syntax
- `pattern.ml` - Pattern matching DSL

**Tree-sitter Integration Flow:**
1. C helper layer wraps TSNode/TSTree in OCaml custom blocks with finalizers
2. ctypes-foreign binds to libtree-sitter and C helpers
3. `languages.ml` dispatches to grammar language functions statically linked into the binary
4. `tree.ml` converts FFI nodes to pure OCaml representation once during parsing

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
- `match: field` - Field-based matching (matches children by tree-sitter field name instead of position, ignores extra source fields not in pattern, preserves order within each field). Use for definitions with decorators/attributes.

### Conjunctive sibling sections

A pattern file with multiple `@@` sections and **no** `on $VAR` directives is a
conjunctive rule: every section must find at least one match for any transforms
to fire. If any section finds nothing, the source is returned unchanged. Each
section has its own independent metavar scope. A section without `-`/`+` lines
acts as a pure guard (must match but produces no edits). Use
`Match.transform_nested` to apply conjunctive patterns.

### Expansion lines

A replacement line may use a separator character as its prefix instead of `+`,
followed by a space. The prefix character IS the join string between expanded
elements, except `~` which stands for newline. The line must reference at least
one `sequence` metavar.

Any punctuation character that is not a reserved spatch role marker (`-`, `+`,
space, tab) and not an identifier character (`$`, letters, digits, `_`) is valid.
Common conventions:

```
~   $VAR      — expand $VAR, joining with newline (~ stands for \n)
,   $VAR      — expand $VAR, joining with ","
;   $VAR      — expand $VAR, joining with ";"
|   $VAR      — expand $VAR, joining with "|"
```

Other characters like `!`, `&`, `.` also work and are used literally as the separator.

The sequence variable(s) must be declared as `metavar $VAR: sequence` in the preamble.

**Verbatim expansion** (no following section): elements are joined with the specified separator and substituted directly. Use `Match.transform` as normal.

**Transform expansion** (with following `@@` section): if a subsequent section declares `on $VAR` matching an expansion line's variable, that section's transform is applied to each element and the results are joined. Use `Match.transform_nested` to enable this.

#### Example — comma-join (verbatim expansion)

```
@@
match: strict
metavar $BEFORE: sequence
metavar $AFTER: sequence
@@
- import { $BEFORE Stack $AFTER } from "@mui/system";
+ import {
,   $BEFORE $AFTER
+ } from "@mui/system";
+ import { Stack } from "@mui/not.system";
```

`$BEFORE` and `$AFTER` elements are gathered and comma-joined.

#### Example — method chain (transform expansion, requires `transform_nested`)

```
@@
match: strict
metavar $TAG: single
metavar $PROPS: sequence
@@
- matchStringExhaustive($TAG, {
-   $PROPS
- });
+ match($TAG)
~   $PROPS
+   .exhaustive();
@@
match: field
on $PROPS
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ .with("$KEY", $VAL)
```

The inner section's transform (`- $KEY: $VAL` / `+ .with("$KEY", $VAL)`) is applied to each element node in `$PROPS`; results are joined with newline.

#### Constraints

- Expansion prefix lines are valid only in the replacement side (like `+` lines).
- Expansion vars must be declared as `sequence` metavars and must appear in the match side.
- Inner sections used for transform expansion cannot themselves contain expansion lines.
- `on $VAR` in an inner section targets an expansion slot when the var matches; this is distinct from context-nesting use of `on $VAR`.
