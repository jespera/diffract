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
│   ├── match.ml            # Public match API (facade)
│   ├── match_types.ml      # Match-related types
│   ├── match_parse.ml      # Pattern parsing and preamble handling
│   ├── match_engine.ml     # Core matching algorithm
│   └── match_search.ml     # Search, indexing, and formatting helpers
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

## Pattern Matching (`lib/match_*.ml`)

Pattern matching is implemented as a pipeline:

1. **Parse**: `match_parse.ml` parses the `@@` preamble, validates metavars,
   expands ellipsis (`...`) into sequence metavars, and parses the pattern with
   tree-sitter (`Tree.parse_as_pattern`).
2. **Match**: `match_engine.ml` performs structural matching with three modes
   (`strict`, `field`, `partial`) and supports sequence metavars (except in `partial` mode).
3. **Search**: `match_search.ml` traverses trees, handles nested patterns, and
   formats match results.
4. **Transform**: `match_transform.ml` computes text edits from match results and
   applies expansion slots for multi-section semantic patches.
   `match.ml` re-exports the public API.

Example (public API):

```ocaml
let ctx = Diffract.Context.create () in
let pattern_text = {|@@
match: strict
metavar $OBJ: single
metavar $METHOD: single
@@
$OBJ.$METHOD()
|} in

let matches = Diffract.Match.find_matches
  ~ctx
  ~language:"typescript"
  ~pattern_text
  ~source_text:code in
List.iter (fun m ->
  Printf.printf "Match at line %d\n" (m.start_point.row + 1)
) matches
```

## Index-Based Pattern Matching (`lib/match.ml`)

When matching multiple patterns against the same source file, the naive approach traverses the entire AST once per pattern, resulting in O(n × k) complexity where n is the number of nodes and k is the number of patterns.

The index-based approach builds a hash table mapping node types to node lists, then queries this index for each pattern:

```
Complexity comparison:
  Traversal: O(n) per pattern = O(n × k) total
  Indexed:   O(n) build + O(m) per pattern = O(n + k × m)
  Where m = average candidates per pattern type (typically m << n)
```

### Data Structures

```ocaml
(** Index for fast node lookup by type *)
type ast_index = {
  by_type: (string, Tree.t list) Hashtbl.t;
}
```

### Algorithm

**Index Building (O(n)):**
```
build_index(root):
  index = empty hashtable
  traverse(root, node ->
    type = node.type
    index[type] = node :: index[type]
  )
  return index
```

**Indexed Matching:**
```
find_matches_with_index(index, pattern, source):
  pattern_node = unwrap_pattern(pattern)  // Get actual pattern content

  if pattern_node is metavar placeholder:
    // Can't filter by type, fall back to full traversal
    return find_matches_in_subtree(pattern, source)

  // Query index for candidates
  pattern_type = pattern_node.type
  candidates = index[pattern_type]  // O(1) lookup

  // Match only against candidates of the right type
  return filter_map(candidates, node ->
    match_node(pattern, node)  // Returns Some(bindings) or None
  )
```

**Multi-Pattern Matching:**
```
find_matches_multi(language, patterns, source):
  tree = parse(source)
  index = build_index(tree.root)  // Built once

  results = []
  for i, pattern_text in patterns:
    pattern = parse_pattern(pattern_text)
    matches = find_matches_with_index(index, pattern, tree)
    results.append((i, matches))

  return results
```

### Usage

```ocaml
(* Match multiple patterns efficiently *)
let patterns = [
  {|@@
match: strict
metavar $msg: single
@@
console.log($msg)|};
  {|@@
match: strict
metavar $msg: single
@@
console.error($msg)|};
] in
let results = Match.find_matches_multi ~language:"typescript" ~patterns ~source_text in
(* Returns (pattern_index, match_result) pairs *)

(* Or build index manually for custom workflows *)
let tree = Tree.parse ~language:"typescript" source_text in
let index = Match.build_index tree.root in
(* Reuse index for multiple queries *)
let pattern = Match.parse_pattern ~language:"typescript" pattern_text in
let matches = Match.find_matches_with_index ~index ~pattern
    ~source:tree.source ~source_root:tree.root
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
