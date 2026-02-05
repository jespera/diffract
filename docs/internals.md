# Architecture and Internals

## Project Structure

```
diffract/
├── lib/                    # Core library
│   ├── diffract.ml          # Main module, re-exports submodules
│   ├── tree.ml             # Pure OCaml tree representation
│   ├── tree_sitter_bindings.ml  # Low-level ctypes FFI
│   ├── ts_helper.c         # C helper functions for tree-sitter
│   ├── node.ml             # FFI-based tree traversal (internal)
│   ├── languages.ml        # Dynamic grammar loading
│   ├── match.ml            # Public match API (facade)
│   ├── match_types.ml      # Match-related types
│   ├── match_parse.ml      # Pattern parsing and preamble handling
│   ├── match_engine.ml     # Core matching algorithm
│   └── match_search.ml     # Search, indexing, and formatting helpers
├── bin/                    # CLI
│   └── main.ml
├── grammars/               # Tree-sitter grammars
│   ├── build-grammars.sh   # Script to build grammar .so files
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

### 1. C Helper Layer (`lib/ts_helper.c`)

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
- `ts_helper_parse`: Parse source code, return tree custom block
- `ts_helper_tree_root_node`: Get root node from tree
- `ts_helper_node_child`: Navigate to child nodes
- `ts_helper_node_type`: Get node's grammar type name
- `ts_helper_node_string`: Get S-expression representation

### 2. OCaml FFI Bindings (`lib/tree_sitter_bindings.ml`)

Uses ctypes-foreign to bind to both libtree-sitter and our C helpers:

```ocaml
(* Bind to tree-sitter core *)
let parser_new = foreign "ts_parser_new" (void @-> returning nativeint)
let parser_set_language = foreign "ts_parser_set_language"
  (nativeint @-> nativeint @-> returning bool)

(* Bind to our C helpers *)
external parse : nativeint -> string -> tree = "ts_helper_parse"
external node_type : t -> string = "ts_helper_node_type"
```

### 3. Dynamic Grammar Loading (`lib/languages.ml`)

Grammars are loaded at runtime using `dlopen`:

```ocaml
let try_load_library lib_basename =
  let lib_name = "lib" ^ lib_basename ^ ".so" in
  (* Search multiple paths *)
  let rec try_paths = function
    | [] -> None
    | path :: rest ->
      try Some (Dl.dlopen ~filename:(path / lib_name) ~flags:[Dl.RTLD_NOW])
      with Dl.DL_error _ -> try_paths rest
  in
  try_paths library_search_paths

let load_language lib_basename func_name =
  match try_load_library lib_basename with
  | Some lib ->
    (* Each grammar exports a function like tree_sitter_typescript() *)
    let fn = foreign func_name (void @-> returning (ptr void)) ~from:lib in
    fn ()
  | None -> failwith "Grammar not found"
```

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
   (`strict`, `field`, `partial`) and supports sequence metavars.
3. **Search**: `match_search.ml` traverses trees, handles nested patterns, and
   formats match results. `match.ml` re-exports the public API.

Example (public API):

```ocaml
let pattern_text = {|@@
match: strict
metavar $OBJ: single
metavar $METHOD: single
@@
$OBJ.$METHOD()
|} in

let matches = Diffract.Match.find_matches
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

1. Add grammar info to `lib/languages.ml`:
```ocaml
let language_info = [
  ...
  ("ruby", ("tree-sitter-ruby", "tree_sitter_ruby"));
]
```

2. Update `grammars/build-grammars.sh` to build the grammar:
```bash
npm install tree-sitter-ruby
cc -shared -fPIC -o lib/libtree-sitter-ruby.so \
  node_modules/tree-sitter-ruby/src/parser.c \
  node_modules/tree-sitter-ruby/src/scanner.c
```

3. Rebuild grammars: `cd grammars && ./build-grammars.sh`
