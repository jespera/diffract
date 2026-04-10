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
- Sequence metavars (including `...`) are not supported with `match: partial`.

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

Notes:
- `match: partial` may backtrack to satisfy all bindings when multiple children could match.
- Sequence metavars are rejected in `match: partial`.

## Nested Patterns with `on $VAR`

Use `on $VAR` in a section to match directly against a previously-bound node
instead of traversing the whole source tree. This restricts where the later
section can match:

```
@@
match: strict
metavar $class_name: single
metavar $body: single
@@
class $class_name { $body }

@@
match: strict
on $body
metavar $msg: single
@@
console.log($msg)
```

This finds `console.log` calls only inside class bodies, not at the top level:
section 1 binds `$body` to the class body node; section 2 uses `on $body` to
search within that node only.

`on $VAR` also works when the bound variable is a **sequence** metavar. In that
case the inner pattern is matched against each element of the sequence in turn.

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

## Conjunctive Sibling Sections

When a multi-section pattern has **no** `on $VAR` directives, all sections are
**conjunctive siblings**: each section searches the source file independently,
and the rule fires only if every section finds at least one match. If any section
finds nothing, no transforms are applied and the source is left unchanged.

This is useful when two changes must always happen together — for example,
renaming an import and updating all its call sites.

### Example: rename import and call sites together

```
@@
match: strict
@@
- import { useAppSelector } from "app/hooks";
+ import { useUser } from "app/UserContext";

@@
match: strict
metavar $NAME: single
metavar $PARAM: single
@@
- const $NAME = useAppSelector(($PARAM) => $PARAM.users.user);
+ const { user: $NAME } = useUser();
```

Both sections must match somewhere in the file. If the import is absent the
call sites are left alone; if there are no matching call sites the import is
left alone. When both are present, both transforms are applied in a single pass.

### Guard sections

A section with no `-`/`+` lines acts as a **guard**: it must match somewhere in
the file but contributes no edits. Use this to make a transform conditional on
the presence of some other code.

```
@@
match: strict
@@
- foo()
+ bar()

@@
match: strict
@@
setupFoo()
```

`foo()` is only renamed if `setupFoo()` also appears somewhere in the file.

### Independent metavar scope

Each sibling section has its own independent metavar scope. The same name can
be declared in multiple sections without conflict:

```
@@
match: strict
metavar $X: single
@@
- foo($X)
+ bar($X)

@@
match: strict
metavar $X: single
@@
- baz($X)
+ qux($X)
```

The `$X` in section 1 and the `$X` in section 2 are entirely independent
bindings.

### Mutually exclusive modes

Conjunctive sibling sections (no `on $VAR`) and outer+inner sections (with
`on $VAR`) cannot be mixed in the same pattern. Attempting to do so raises an
error at parse time.

Use `Match.transform_nested` (rather than `Match.transform`) when the pattern
contains two or more `@@` sections.

## Metavariable Scoping

### Global scope across sections (outer+inner mode)

In outer+inner mode (patterns that use `on $VAR`), metavariables share a
**global scope** across all sections. A metavar declared in the outer section
is automatically in scope for all inner sections — you do not need to (and must
not) re-declare it:

```
@@
match: strict
metavar $CLASS: single
metavar $BODY: single
@@
class $CLASS { $BODY }

@@
match: strict
@@
console.log($CLASS)
```

Here the inner section uses `$CLASS` (declared in the outer section) directly in its pattern body without re-declaring it. This matches only `console.log` calls whose argument is the same identifier as the enclosing class name.

### No shadowing (outer+inner mode)

Declaring a metavar in an inner section that was already declared in an outer section is an error:

```
@@
match: strict
metavar $X: single
@@
foo($X)

@@
match: strict
metavar $X: single   (* ERROR: $X was already declared above *)
@@
bar($X)
```

This raises a `Failure` exception at parse time. Use the existing binding from the outer section instead.

### Unification

When the same metavar appears in more than one position in a match pattern, all occurrences must bind to **structurally identical** nodes. This applies both within a single section and across sections:

```
@@
match: strict
metavar $X: single
@@
compare($X, $X)
```

This only matches calls where both arguments are identical (e.g. `compare(a, a)`), not `compare(a, b)`.

Across sections, the outer binding constrains the inner match. In the `console.log($CLASS)` example above, only calls where the argument text matches the previously bound `$CLASS` will match.

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

### Expansion transforms

Expansion transforms let you join or restructure the elements of a sequence
metavar in the replacement. Two capabilities are unlocked:

1. **Verbatim expansion** — join all elements with a separator character.
2. **Per-element transform** — apply a nested patch to each element and join the results.

#### Separator-prefix lines

Instead of `+ `, use a separator character followed by a space as the line
prefix in the replace side. Any punctuation character that is not a reserved
spatch role marker (`-`, `+`, space, tab) and not an identifier character
(`$`, letters, digits, `_`) is accepted. The prefix character IS the join
string between elements, except `~` which stands for newline (`\n`).

Common conventions:

| Prefix | Join string |
|--------|-------------|
| `~ `   | newline     |
| `, `   | `,`         |
| `; `   | `;`         |
| `\| `  | `\|`        |

Other punctuation characters like `!`, `&`, `.`, `/` work the same way — the
character itself becomes the separator. The second character of the line must
be a space, which prevents code lines starting with punctuation (e.g. `($ARGS)`)
from being misidentified as expansion lines.

The variable on the expansion line must be declared as `metavar $VAR: sequence`.

#### Example: splitting a named import

Move one named export to a different package, comma-joining the remaining names:

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

Given `import { Box, Stack, Paper } from "@mui/system";`, the patch produces:

```typescript
import {
Box,Paper
} from "@mui/system";
import { Stack } from "@mui/not.system";
```

`$BEFORE` binds to `[Box,]` and `$AFTER` binds to `[Paper]`; their elements are
gathered together and joined with `,`.

#### Example: converting a match expression to a method chain

Convert `matchStringExhaustive(tag, { A: () => 1, B: () => 2 })` into a fluent
chain `match(tag).with("A", () => 1).with("B", () => 2).exhaustive()`.

This requires a **two-section pattern**: the outer section captures the whole
call and names the sequence of case pairs; the inner section (prefixed with
`on $CASES`) describes how to transform each individual pair.

```
@@
match: strict
metavar $TAG: single
metavar $CASES: sequence
@@
- matchStringExhaustive($TAG, {
-   $CASES
- });
+ match($TAG)
~   $CASES
+   .exhaustive();
@@
match: field
on $CASES
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ .with("$KEY", $VAL)
```

Step by step:
- The outer `@@` section matches the `matchStringExhaustive(...)` call and binds
  `$TAG` to the first argument and `$CASES` to all `key: value` pairs inside the
  object literal.
- `~ $CASES` is an expansion line: each element of `$CASES` is transformed by
  the inner section and the results are joined with newlines.
- The inner `@@` section (with `on $CASES`) applies to each pair: `A: () => 1`
  becomes `.with("A", () => 1)`.

Given:
```typescript
matchStringExhaustive(tag, {
  A: () => 1,
  B: () => 2,
});
```

The result is:
```typescript
match(tag)
.with("A", () => 1)
.with("B", () => 2)
.exhaustive();
```

Use `Match.transform_nested` (instead of `Match.transform`) when the pattern
contains two or more `@@` sections — whether for expansion or conjunctive
sibling transforms.

#### Constraints

- Expansion prefix lines are only valid in the replacement side (like `+ ` lines).
  The second character must be a space.
- An expansion variable must be declared as `sequence` — using a `single`
  metavar on an expansion line is an error.
- Expansion variables must also appear in the match (`-` or context) side.
- Inner sections used for per-element transforms cannot themselves use expansion
  lines.

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
(* Create a context once at program start and reuse it *)
let ctx = Diffract.Context.create () in

(* Parse and traverse *)
let tree = Diffract.parse_tree ~ctx ~language:"typescript" code in
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
  ~ctx
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
  ~ctx
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

(* Apply a multi-section expansion patch — use transform_nested *)
let patch = {|@@
match: strict
metavar $TAG: single
metavar $CASES: sequence
@@
- matchStringExhaustive($TAG, {
-   $CASES
- });
+ match($TAG)
~   $CASES
+   .exhaustive();
@@
match: field
on $CASES
metavar $KEY: single
metavar $VAL: single
@@
- $KEY: $VAL
+ .with("$KEY", $VAL)|} in
let result = Diffract.Match.transform_nested
  ~ctx
  ~language:"typescript"
  ~pattern_text:patch
  ~source_text:code in
print_string result.transformed_source

(* Apply a conjunctive sibling patch — use transform_nested.
   Both sections must match or neither transform is applied. *)
let patch = {|@@
match: strict
@@
- import { useAppSelector } from "app/hooks";
+ import { useUser } from "app/UserContext";

@@
match: strict
metavar $NAME: single
metavar $PARAM: single
@@
- const $NAME = useAppSelector(($PARAM) => $PARAM.users.user);
+ const { user: $NAME } = useUser();|} in
let result = Diffract.Match.transform_nested
  ~ctx
  ~language:"typescript"
  ~pattern_text:patch
  ~source_text:code in
print_string result.transformed_source
```
