# Architecture and Internals

## Project Structure

```
diffract/
├── lib/                    # Core library
│   ├── diffract.ml          # Main module, re-exports submodules
│   ├── tree.ml             # Pure OCaml tree representation
│   ├── tree_sitter_bindings.ml  # Low-level ctypes FFI
│   ├── tree_sitter_helper.c         # C helper functions for tree-sitter
│   ├── node.ml             # FFI-based tree traversal (internal)
│   ├── languages.ml        # Static grammar registry
│   ├── tokenize.ml         # Pattern body -> (text, node_type) token stream
│   ├── cursor.ml           # Abstract tree-cursor interface (Cursor.S)
│   ├── tree_sitter_cursor.ml  # Cursor.S over a real tree-sitter parse
│   ├── stmatch.ml          # Matching engine (strict/partial/field, backtracking)
│   ├── matcher.ml          # End-to-end find/transform; public matcher API
│   └── text_diff.ml        # Line-based unified diff (for `apply`)
├── bin/                    # CLI
│   └── main.ml
├── grammars/               # Tree-sitter grammars
│   ├── build-grammars.sh   # Script to build grammar static archives
│   └── lib/                # Compiled grammar libraries
└── tests/                  # Alcotest unit tests
```

## Tree Module (`lib/tree.ml`)

The `Tree` module provides a pure OCaml representation of parsed syntax trees:

- Eliminates FFI overhead during tree traversal (26-48% faster than direct FFI)
- Simplifies memory management (no tree-sitter lifetime concerns)
- Provides idiomatic OCaml types with option types instead of null checks

```ocaml
type point = { row: int; column: int }

type t = {
  node_type: string;
  is_named: bool;
  start_byte: int;
  end_byte: int;
  start_point: point;
  end_point: point;
  children: child list;
  named_children: t list;
}

type tree = {
  root: t;
  source: string;
}
```

The tree is constructed once during parsing by traversing the tree-sitter nodes and copying all data into OCaml values. After that, all operations are pure OCaml with no FFI calls.

## Tree-sitter Integration

Tree-sitter is a C library that provides incremental parsing with concrete syntax trees. The integration involves several layers:

### 1. C Helper Layer (`lib/tree_sitter_helper.c`)

Tree-sitter's `TSNode` is a 32-byte struct passed by value, which is problematic for OCaml's ctypes FFI (libffi doesn't support large structs by value). We solve this with a C helper layer that:

1. **Wraps TSNode in OCaml custom blocks**: The C code allocates TSNode on the heap and wraps it in an OCaml custom block with a destructor.

2. **Manages tree lifetime**: `TSTree` is also wrapped in a custom block. Nodes hold a reference to their tree to prevent use-after-free.

```c
// Custom block for TSTree
static struct custom_operations tree_ops = {
  "ts_tree",
  custom_finalize_tree,  // Calls ts_tree_delete
  ...
};

// Custom block for TSNode (wrapper)
typedef struct {
  TSNode node;
  value tree_ref;  // Reference to tree to keep it alive
} TSNodeWrapper;
```

Key functions:
- `tsh_parse`: Parse source code, return tree custom block
- `tsh_tree_root_node`: Get root node from tree
- `tsh_node_child`: Navigate to child nodes
- `tsh_node_type`: Get node's grammar type name
- `tsh_node_string`: Get S-expression representation

### 2. OCaml FFI Bindings (`lib/tree_sitter_bindings.ml`)

Uses `external` declarations to bind to the C helper functions:

```ocaml
external parser_new : unit -> nativeint = "tsh_parser_new"
external parser_set_language : nativeint -> nativeint -> bool = "tsh_parser_set_language"
external parse_to_sexp : nativeint -> string -> string = "tsh_parse_to_sexp"
```

### 3. Static Grammar Registry (`lib/languages.ml`)

Grammar language functions are statically linked into the binary. Each grammar is exposed via a thin C wrapper in `tree_sitter_helper.c` and bound with an OCaml `external` declaration:

```c
/* tree_sitter_helper.c */
extern const TSLanguage *tree_sitter_typescript(void);
CAMLprim value tsh_typescript_language(value v_unit) {
    CAMLparam1(v_unit);
    CAMLreturn(caml_copy_nativeint((intnat)tree_sitter_typescript()));
}
```

```ocaml
(* languages.ml *)
external typescript_language : unit -> nativeint = "tsh_typescript_language"

let canonical_info = [
  ("typescript", ["ts"], typescript_language);
  ...
]
```

The grammar `.a` static archives are built by `grammars/build-grammars.sh`, copied into the dune build tree, and linked via `(foreign_archives ...)` in `lib/dune`. The binary is fully self-contained with no runtime library search.

### 4. Node Traversal API (`lib/node.ml`)

High-level OCaml API for tree navigation:

```ocaml
type tree  (* Abstract, backed by custom block *)
type t     (* Node type, also custom block *)

val root : tree -> t
val node_type : t -> string
val named_children : t -> t list
val text : string -> t -> string
val find_by_type : string -> t -> t list
val traverse : (t -> unit) -> t -> unit
```

## Pattern Matching (`lib/tokenize.ml`, `lib/stmatch.ml`, `lib/matcher.ml`)

The matcher is **tokenizer-based**: it uses tree-sitter as a *lexer* for the
pattern (keeping leaves, discarding hierarchy) and matches that leaf stream
against the source tree, comparing leaves on both **text and node type**. This
is what lets patterns be sub-syntactic *fragments* (e.g. `} else {`) that
aren't valid standalone ASTs. See `docs/universal-tokenizer.md` for the full
design.

The pipeline:

1. **Preamble parse** (`matcher.ml`): split the `@@` sections, read each
   section's `match:` mode, `metavar` declarations, `on`/`foreach` scope, and
   `join` directives.
2. **Tokenize** (`tokenize.ml`): parse the pattern body with the source
   language's grammar and walk its leaves into a `pattern_token` stream —
   `Concrete (text, node_type)`, `Subtree` (single metavar), or `Siblings`
   (sequence metavar / ellipsis). Metavars are **sigil-free**: a leaf is a
   metavar iff its text equals a declared name. ERROR nodes and zero-width
   "missing" leaves are handled so fragments and terminator-less patterns work.
3. **Match** (`stmatch.ml`): a `Make` functor over a `Cursor.S` runs the engine
   — `strict` (exact, ordered), `partial` (subset of a bracketed container's
   children), and `field` (align the pattern to a subsequence of a
   declaration's children) — with backtracking.
4. **Drive + transform** (`matcher.ml`): walk the source via
   `Tree_sitter_cursor`, compose multi-section patterns (shared metavars,
   `on`/`foreach` scoping), and apply the `-`/`+` replacement side
   (substitution, `foreach`/`join` sequence rendering, element removal). The
   matched span is replaced wholesale (`text_diff.ml` renders the diff for
   `apply`).

Context-sensitive node types (e.g. an object key being `property_identifier`
vs a standalone `identifier`) are handled by **source-context tokenization**:
the pattern fragment is re-tokenized spliced into its real surrounding source,
then its own leaves are extracted by byte span — used for `foreach` concrete
keys and field-mode concrete names.

Example (public API):

```ocaml
let ctx = Diffract.Context.create () in
let pattern_text = {|@@
match: strict
metavar $obj: single
metavar $method: single
@@
$obj.$method()
|} in

let composites =
  Diffract.Matcher.find ~ctx ~language:"typescript" ~pattern_text
    ~source_text:code
in
List.iter
  (fun (c : Diffract.Matcher.composite_match) ->
    List.iter
      (fun (r : Diffract.Matcher.M.match_result) ->
        Printf.printf "match at byte %d\n" r.start_byte)
      c.sections)
  composites
```

## Adding a New Language

1. Add a C wrapper to `lib/tree_sitter_helper.c`:
```c
extern const TSLanguage *tree_sitter_ruby(void);
CAMLprim value tsh_ruby_language(value v_unit) {
    CAMLparam1(v_unit);
    CAMLreturn(caml_copy_nativeint((intnat)tree_sitter_ruby()));
}
```

2. Add the `external` binding and registry entry to `lib/languages.ml`:
```ocaml
external ruby_language : unit -> nativeint = "tsh_ruby_language"

let canonical_info = [
  ...
  ("ruby", [], ruby_language);
]
```

3. Add a copy rule and `(foreign_archives ...)` entry to `lib/dune`:
```
(rule (target libtree-sitter-ruby.a)
 (deps ../grammars/lib/libtree-sitter-ruby.a)
 (action (copy %{deps} %{target})))
```
And add `tree-sitter-ruby` to the `(foreign_archives ...)` list in the library stanza.

4. Update `grammars/build-grammars.sh` to build the static archive:
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

5. Rebuild: `cd grammars && ./build-grammars.sh && cd .. && dune build`
