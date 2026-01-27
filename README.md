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

### CLI

```bash
# Parse a file to S-expression
diffract example.ts

# Parse with explicit language
diffract --language kotlin example.kt

# Diff two files
diffract before.ts after.ts

# Diff with flat output (one change per line)
diffract --flat before.ts after.ts

# Diff with anti-unified output (shows [before → after] for changes)
diffract --antiunify before.ts after.ts

# Match a pattern against source code
diffract --match pattern.txt source.ts

# List available languages
diffract --list-languages
```

### Pattern File Format

Pattern files use `@@` delimiters with explicit metavariable declarations:

```
@@
metavar $obj: single
metavar $method: single
metavar $arg: single
@@
$obj.$method($arg)
```

Each metavariable must be declared with a type:
- `single` - matches exactly one AST node (which may be a compound expression)
- `sequence` - matches zero or more AST nodes

**Example with sequence metavar:**
```
@@
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }
```

This matches a class with any number of members, binding `$body` to all children.

**Example output:**
```
Found 2 match(es):

line 5: console.log("hello")
  $obj = console
  $method = log
  $arg = "hello"

line 12: Math.floor(x + 1)
  $obj = Math
  $method = floor
  $arg = x + 1
```

Metavariables are pre-processed before parsing, so they work across all supported languages regardless of whether `$name` is valid syntax in that language.

### Library

```ocaml
(* Parse and traverse *)
let tree = Diffract.parse_tree ~language:"typescript" code in
Diffract.Tree.traverse (fun node ->
  Printf.printf "%s: %s\n"
    node.node_type
    (Diffract.Tree.text tree.source node)
) tree.root

(* Compute diff *)
let result = Diffract.diff ~language:"typescript" ~before ~after in
List.iter (fun change ->
  Printf.printf "%s\n" (Diffract.Diff.change_node_type change)
) (Diffract.Diff.flatten_changes result.changes)

(* Anti-unify to find what changed *)
let ann = Diffract.Antiunify.antiunify_change result change in
Printf.printf "%s\n" (Diffract.Antiunify.to_string ann)

(* Pattern matching with concrete syntax *)
let pattern_text = {|@@
metavar $obj: single
metavar $method: single
@@
$obj.$method()|} in
let matches = Diffract.Match.find_matches
  ~language:"typescript"
  ~pattern_text
  ~source_text:code in
List.iter (fun m ->
  Printf.printf "Match at line %d:\n" (m.start_point.row + 1);
  List.iter (fun (var, value) ->
    Printf.printf "  %s = %s\n" var value
  ) m.bindings
) matches
```

---

## Architecture

```
diffract/
├── lib/                    # Core library
│   ├── diffract.ml          # Main module, re-exports submodules
│   ├── tree.ml             # Pure OCaml tree representation
│   ├── tree_sitter_bindings.ml  # Low-level ctypes FFI
│   ├── ts_helper.c         # C helper functions for tree-sitter
│   ├── node.ml             # FFI-based tree traversal (internal)
│   ├── languages.ml        # Dynamic grammar loading
│   ├── diff.ml             # Tree diff algorithm
│   ├── antiunify.ml        # Anti-unification algorithm
│   ├── match.ml            # Concrete syntax pattern matching
│   ├── pattern.ml          # Pattern matching DSL
│   └── abstract.ml         # Change abstraction
├── bin/                    # CLI
│   └── main.ml
├── grammars/               # Tree-sitter grammars
│   ├── build-grammars.sh   # Script to build grammar .so files
│   └── lib/                # Compiled grammar libraries
└── tests/                  # Alcotest unit tests
```

---

## Implementation Details

### Tree Module (`lib/tree.ml`)

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

### Tree-sitter Integration

Tree-sitter is a C library that provides incremental parsing with concrete syntax trees. The integration involves several layers:

#### 1. C Helper Layer (`lib/ts_helper.c`)

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

#### 2. OCaml FFI Bindings (`lib/tree_sitter_bindings.ml`)

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

#### 3. Dynamic Grammar Loading (`lib/languages.ml`)

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

#### 4. Node Traversal API (`lib/node.ml`)

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

### Diff Algorithm (`lib/diff.ml`)

The diff algorithm computes structural differences between two parsed trees.

#### Data Structures

```ocaml
type change =
  | Added of { context; node; shape }
  | Removed of { context; node; shape }
  | Modified of { context; node_type; before; after; children_changed }
  | Replaced of { context; before; after; before_shape; after_shape }

type diff_result = {
  changes: change list;
  before_source: string;
  after_source: string;
}
```

#### Algorithm

The algorithm is a recursive tree comparison:

```
diff_nodes(node1, node2):
  1. If both null → no change
  2. If only node1 null → Added(node2)
  3. If only node2 null → Removed(node1)
  4. If text identical → no change
  5. If types differ → Replaced(node1, node2)
  6. Same type, different content:
     a. Match children using greedy algorithm
     b. Recursively diff matched pairs
     c. Report unmatched as Added/Removed
     d. Wrap in Modified if any child changes
     e. If no child changes but text differs (leaf nodes), report Replaced
```

**Child Matching Algorithm:**

```
match_children(children1, children2):
  Pass 1 - Exact matches:
    For each child1, find child2 with identical text
    Mark both as matched

  Pass 2 - Type matches:
    For unmatched children, find same-type pairs
    Use similarity score (1.0 = identical, 0.8 = same shape, 0.3 = same type)

  Return: (matched_pairs, unmatched_from_1, unmatched_from_2)
```

### Anti-Unification Algorithm (`lib/antiunify.ml`)

Anti-unification finds the most specific generalization of two terms. In our context, it identifies exactly what differs between before and after.

#### Single-Change Anti-Unification

Given two nodes (before and after), produce an annotated tree showing:
- **Same**: Identical parts (kept concrete)
- **Diff**: Parts with same type but different text (shown as `[before → after]`)
- **Added**: Parts only in after (`{+text}`)
- **Removed**: Parts only in before (`{-text}`)

```ocaml
type annotated =
  | Same of { node_type; text; children }
  | Diff of { node_type; before: string; after: string }
  | Added of { node_type; text }
  | Removed of { node_type; text }
  | Modified of { node_type; children: annotated list }
```

**Algorithm:**

```
antiunify_nodes(src1, node1, src2, node2):
  If text1 = text2 → None (identical, no annotation needed)
  If type1 ≠ type2 → Replaced
  If both are leaves with different text → Diff { before=text1; after=text2 }
  Otherwise:
    Match children
    Recursively antiunify matched pairs
    Mark unmatched as Added/Removed
    Return Modified with annotated children
```

**Example:**

```typescript
// Before
console.log("Hello")
// After
console.warn("Hello")
```

Anti-unified result:
```
(call_expression
  (member_expression console [log → warn])
  ("Hello"))
```

Only `log → warn` is marked as different; `console` and `"Hello"` stay concrete.

#### Cross-Change Pattern Finding

When comparing multiple similar changes, we further anti-unify to find common patterns:

```ocaml
type pattern =
  | PConcrete of string           (* Same across all instances *)
  | PVar of int * string list     (* Varies - index and concrete values *)
  | PTransform of { before; after }
  | PNode of string * pattern list
```

**Algorithm:**

```
antiunify_patterns(p1, p2):
  If p1 = p2 (same concrete) → p1
  If both concrete but different → PVar(fresh_id, [p1, p2])
  If both transforms:
    antiunify befores
    antiunify afters
    return PTransform
  If both nodes with same type:
    antiunify each child pair
    return PNode
  Otherwise → PVar (incompatible)
```

**Example:** Two changes:
1. `console.log(x)` → `console.warn(x)`
2. `console.log(msg)` → `console.warn(msg)`

Cross-change anti-unification:
```
Pattern: console.[log → $0]($1)
Where: $0 = {warn}  (the replacement is always 'warn')
       $1 = {x, msg} (the argument varies)
```

This reveals:
- `console` and `log` are constant
- The transformation `log → warn` is the same
- Only the argument varies across instances

### Pattern Matching DSL (`lib/pattern.ml`)

A DSL for querying AST nodes (alternative to tree-sitter's query language):

```ocaml
(* Find all console.log calls *)
let pattern = Pattern.(
  node "call_expression" [
    field "function" (node "member_expression" [
      field "object" (node "identifier" [has_text "console"]);
      field "property" (node "property_identifier" [has_text "log"]);
    ]);
    capture "args" (any_node ());
  ]
)

let matches = Pattern.find_all pattern root
```

---

## Testing

Tests use the [Alcotest](https://github.com/mirage/alcotest) framework:

```bash
# Run all tests
dune test

# Run with verbose output
dune build @runtest --force
```

Test coverage:
- **Node module**: Parsing, traversal, text extraction
- **Diff module**: Change detection, node comparison, flattening
- **Antiunify module**: Annotation generation, pattern extraction, cross-pattern unification

---

## Benchmarks

Performance benchmarks are provided for the pattern matching functionality.

### Running Benchmarks

```bash
# Run benchmarks (outputs results to console and saves JSON)
dune exec benchmarks/bench_match.exe

# Compare last two benchmark runs
dune exec benchmarks/bench_compare.exe

# Show benchmark history table
dune exec benchmarks/bench_compare.exe -- --history

# Show trends with sparklines
dune exec benchmarks/bench_compare.exe -- --trends

# Generate gnuplot data files
dune exec benchmarks/bench_compare.exe -- --gnuplot
```

### Benchmark Suite

The benchmark suite measures pattern matching performance:

| Benchmark | Description |
|-----------|-------------|
| `simple_pattern` | Simple metavar matching (e.g., `$obj.$method($arg)`) |
| `sequence_N_children` | Sequence metavar (`$body*`) with N children (N=2,5,10,20) |
| `nested_seq_N_children` | Nested patterns with sequence metavar (N=2,5,10,20) |

### Scaling Test

A separate scaling test measures how performance grows with input size:

```bash
# Run scaling analysis
dune exec benchmarks/bench_scaling.exe
```

Example output:
```
Children     Sequence (ms)     Nested (ms)      Ratio
-------------------------------------------------------
n=10                  0.40 ms         0.46 ms       1.00x
n=20                  0.76 ms         1.00 ms       1.90x
n=50                  1.75 ms         2.21 ms       2.30x
n=100                 3.37 ms         4.33 ms       1.93x
n=200                 7.16 ms        13.48 ms       2.12x
```

The sequence matching algorithm uses O(n) precomputation of cumulative texts,
resulting in approximately linear scaling with the number of children.

### Output

**Console output:**
```
Benchmark                                   ns/iter
-------------------------------------------------------
pattern_matching/simple_pattern           117528.97
pattern_matching/sequence_02_children     107053.02
...
```

**JSON results:** Each run saves a timestamped JSON file to `benchmarks/results/`:
```json
{
  "timestamp": 1769497438.0,
  "git_commit": "abc1234",
  "benchmarks": [
    { "name": "pattern_matching/simple_pattern", "mean_ns": 117528.97 },
    ...
  ]
}
```

**Comparison output (`--trends`):**
```
Benchmark Trends (last 3 runs)

Benchmark                           First          Latest  Trend
----------------------------------------------------------------
simple_pattern                    107.94 us       117.53 us  ▁▅█ +8.9%
sequence_02_children               97.54 us       107.05 us  ▁▇█ +9.7%
```

---

## Extending

### Adding a New Language

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

---

## License

MIT
