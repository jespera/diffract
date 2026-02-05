# Pattern File Format

Pattern files use `@@` delimiters with a **required** match mode and optional metavariable declarations:

```
@@
match: strict
metavar $obj: single
metavar $method: single
metavar $arg: single
@@
$obj.$method($arg)
```

**Match modes (required - must specify one):**
- `match: strict` - Exact positional matching (no extra children allowed, ordered). Use for function calls, arrays.
- `match: partial` - Subset matching (ignores extra children, unordered). Use for object literals, JSX attributes.
- `match: field` - Field-based matching (matches by tree-sitter field name, ignores extra fields, preserves order within each field). Use for definitions with decorators/attributes.

## Comparing Match Modes

The three modes behave differently when the source has extra children or different ordering:

### Example: Function calls (use `strict`)

Pattern: `foo($a, $b)`

| Source | strict | partial | field |
|--------|--------|---------|-------|
| `foo(1, 2)` | ✓ match | ✓ match | ✓ match |
| `foo(1, 2, 3)` | ✗ no match (extra arg) | ✓ match | ✓ match |
| `foo(2, 1)` | ✓ match ($a=2, $b=1) | ✓ match | ✓ match |

For function calls, you typically want `strict` because `foo(1, 2)` and `foo(1, 2, 3)` are semantically different calls.

### Example: Object literals (use `partial`)

Pattern: `{ x: $X, y: $Y }`

| Source | strict | partial | field |
|--------|--------|---------|-------|
| `{ x: 1, y: 2 }` | ✓ match | ✓ match | ✓ match |
| `{ x: 1, y: 2, z: 3 }` | ✗ no match | ✓ match | ✓ match |
| `{ y: 2, x: 1 }` | ✗ no match (wrong order) | ✓ match | ✗ no match |

For object literals, you typically want `partial` because property order doesn't matter and you may not care about extra properties.

### Example: Function definitions with decorators (use `field`)

Pattern:
```
function $NAME() { $BODY }
```

| Source | strict | partial | field |
|--------|--------|---------|-------|
| `function foo() { return 1; }` | ✓ match | ✓ match | ✓ match |
| `@decorator function foo() { return 1; }` | ✗ no match | ✓ match | ✓ match |
| `function foo() { return 1; return 2; }` | ✗ no match | ✓ match | ✗ no match |

For function definitions, `field` is ideal: it ignores decorators/attributes (which are in a different tree-sitter field) while still enforcing order within the function body.

### Summary

| Mode | Extra children | Ordering | Best for |
|------|---------------|----------|----------|
| `strict` | Fail | Required | Function calls, arrays, exact structure |
| `partial` | Ignored | Ignored | Object literals, JSX attributes |
| `field` | Ignored (different field) | Required within fields | Definitions with decorators/attributes |

Each metavariable must be declared with a type:
- `single` - matches exactly one AST node (which may be a compound expression)
- `sequence` - matches zero or more AST nodes

**Example with sequence metavar:**
```
@@
match: strict
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }
```

This matches a class with any number of members, binding `$body` to all children.

## Partial Matching

Use `match: partial` to enable subset matching for children. Each pattern child finds any matching source child (unordered), and extra source children are ignored:

```
@@
match: partial
metavar $X: single
@@
{ someField: $X }
```

This matches objects containing `someField`, regardless of other properties. For example, it matches `{ someField: 1, other: 2 }` binding `$X = 1`.

Partial matching applies recursively to all nested structures:

```
@@
match: partial
metavar $X: single
@@
{ f1: { f2: $X } }
```

This matches `{ f1: { f2: 42, f3: "extra" }, other: true }` binding `$X = 42`.

## Nested Patterns with `on $VAR`

Use multiple `@@` sections for nested/scoped matching. Each context pattern restricts where the next pattern can match:

```
@@
match: strict
metavar $class_name: single
metavar $body: single
@@
class $class_name { $body }

@@
match: strict
metavar $msg: single
@@
console.log($msg)
```

This finds `console.log` calls only inside class bodies, not at the top level.

Use `on $VAR` to match directly against a previously-bound node instead of traversing its subtree:

```
@@
match: strict
metavar $OBJ: single
@@
foo($OBJ)

@@
match: partial
on $OBJ
metavar $X: single
@@
{ someField: $X }
```

This matches `foo({ someField: 1, other: 2 })`:
- Section 1 matches `foo($OBJ)`, binding `$OBJ` to `{ someField: 1, other: 2 }`
- Section 2 uses `on $OBJ` to match directly against that object (not searching within it)
- With `match: partial`, it extracts `$X = 1` while ignoring `other: 2`

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

## Library API

```ocaml
(* Parse and traverse *)
let tree = Diffract.parse_tree ~language:"typescript" code in
Diffract.Tree.traverse (fun node ->
  Printf.printf "%s: %s\n"
    node.node_type
    (Diffract.Tree.text tree.source node)
) tree.root

(* Pattern matching with concrete syntax *)
let pattern_text = {|@@
match: strict
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
