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

All metavariables used in the pattern body should be declared in the preamble.
The parser validates undeclared metavars for tokens like `$NAME` (uppercase after `$`).

### Ellipsis (`...`) as Sequence Metavar

You can use `...` as a shorthand for an anonymous sequence metavariable. Each
ellipsis is treated as its own sequence binding internally:

```
@@
match: strict
metavar $NAME: single
@@
function $NAME(...) { ... }
```

Notes:
- Each `...` matches zero or more nodes.
- Ellipsis is not replaced when it looks like a spread operator (e.g., `...$x` or `...args`).

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

## Transforms (Semantic Patches)

Patterns can describe code transformations using `-`/`+` line prefixes, similar
to unified diff syntax. Lines prefixed with `- ` appear only in the match; lines
with `+ ` appear only in the replacement. Unprefixed lines (or lines with a
leading space) are **context** that appears in both.

### Basic example

Rename a function call:

```
@@
match: strict
metavar $MSG: single
@@
- console.log($MSG)
+ logger.info($MSG)
```

Applied to `console.log("hello")`, this produces `logger.info("hello")`.
Metavariables carry their bound values from the match side to the replace side.

### Multi-line patterns

Context lines anchor the match while `-`/`+` lines describe the change:

```
@@
match: strict
metavar $NAME: single
metavar $BODY: sequence
@@
- function $NAME() {
-     $BODY
- }
+ const $NAME = () => {
+     $BODY
+ }
```

This converts traditional function declarations into arrow functions.

### CLI usage

```bash
# Preview the diff (no file changes)
diffract --apply --match patch.txt source.ts

# Apply changes in place
diffract --apply --in-place --match patch.txt source.ts

# Apply across a directory
diffract --apply --in-place --match patch.txt --include '*.ts' src/
```

### Strict-mode transforms

With `match: strict`, the entire matched node is replaced with the instantiated
template. This is the simplest mode and works well for renaming function calls,
swapping arguments, or restructuring expressions.

**Swap arguments:**
```
@@
match: strict
metavar $A: single
metavar $B: single
@@
- assertEqual($A, $B)
+ assertEqual($B, $A)
```

### Partial-mode transforms

With `match: partial`, only the matched subset of children is affected. Source
children not mentioned in the pattern are preserved. This is useful for modifying
individual properties in object literals.

**Rename a property:**
```
@@
match: partial
metavar $V: single
@@
- { color: $V }
+ { colour: $V }
```

Source `{ color: "red", size: 10 }` becomes `{ colour: "red", size: 10 }` -
the `size` property is untouched.

**Add a property:**
```
@@
match: partial
metavar $V: single
@@
- { name: $V }
+ { name: $V, id: 0 }
```

Every object that has a `name` property gets an `id: 0` added. Objects without
`name` are left alone.

**Remove a property:**
```
@@
match: partial
metavar $V: single
@@
- { name: $V, deprecated: true }
+ { name: $V }
```

Removes the `deprecated` property from objects that have both `name` and
`deprecated: true`.

### Field-mode transforms

With `match: field`, children are matched by tree-sitter field name. Fields not
mentioned in the pattern are preserved, while changes are applied to the
specific fields that appear in `-`/`+` lines.

**Modify an attribute (same field set):**
```
@@
match: field
@@
<?php
- #[deprecated]
+ #[deprecated(note = "use new API")]
function test() {
    echo "hi";
}
```

This changes the `#[deprecated]` attribute while leaving the function name,
parameters, and body untouched.

**Add a new field (e.g., return type):**
```
@@
match: field
metavar $NAME: single
metavar $BODY: sequence
@@
<?php
- function $NAME() { $BODY }
+ function $NAME(): void { $BODY }
```

This adds a return type to PHP functions. Because this uses field mode, source
attributes (like `#[Route("/")]`) are preserved even though the pattern doesn't
mention them:

```diff
 <?php
 #[Route("/")]
-function foo() { return 1; }
+function foo(): void { return 1; }
```

### How match modes affect transforms

| Mode | What gets replaced | Unmentioned source children |
|------|-------------------|----------------------------|
| `strict` | The entire matched node | N/A (all children must match) |
| `partial` | Individual matched children | Preserved |
| `field` | Children within matched fields | Preserved (other fields kept) |

### Rules

- Every metavariable used on the `+` side must also appear on the `-` or
  context side (so it has a value to substitute).
- Ellipsis (`...`) is only allowed on context or `-` lines so it is bound on
  the match side. Using `...` in a `+`-only line is an error.
- Use `- ...` only with strong anchors on surrounding context lines; otherwise
  it can match too broadly (including empty sequences) and remove more than
  intended.
- Patterns without any `-`/`+` lines are plain search patterns; `transform`
  returns the source unchanged.
- When multiple matches occur in the same file, edits are applied bottom-to-top
  to avoid offset invalidation. Overlapping edits are filtered (first in
  document order wins; ties by start position keep the longest edit).

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

(* Apply a semantic patch *)
let patch = {|@@
match: strict
metavar $MSG: single
@@
- console.log($MSG)
+ logger.info($MSG)|} in
let result = Diffract.Match.transform
  ~language:"typescript"
  ~pattern_text:patch
  ~source_text:code in
if result.edits <> [] then begin
  let diff = Diffract.Match.generate_diff
    ~file_path:"source.ts"
    ~original:result.original_source
    ~transformed:result.transformed_source in
  print_string diff
end
```
