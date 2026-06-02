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
- `match: field` - Declaration matching that ignores optional fields the pattern omits (decorators, annotations, modifier groups, return types), by aligning the pattern to a subsequence of the declaration's children. Use for decorated/annotated definitions.

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
- On transform (strict mode), the source `...` captures are **preserved**: a
  `-` line inside an `...`-bracketed pattern removes only the marked part and
  keeps the surrounding siblings (`...` is never emitted literally).
- A bare `...` cannot sit on a `-` or `+` line — it is a match-only construct,
  so it belongs on a context line. (An ellipsis *nested* in a marked
  expression, e.g. `- old(...)` / `+ new()`, is fine: the `-` applies to the
  whole expression, which is replaced wholesale.) A pattern that breaks this is
  rejected with an error.

## Multi-statement Patterns

A pattern body that contains multiple top-level statements (with no enclosing function/class wrapper in the pattern) matches **any consecutive run of source children of the same shape**, regardless of what surrounds them. The pattern fires anywhere a container's child sequence has the named statements as immediate successors:

```
@@
match: strict
metavar $X: single
@@
bar();
foo($X);
```

This matches `bar(); foo(x);` inside any function body, `if`/`else` branch, loop body, lambda body, or other statement-bearing block — at arbitrary nesting depth. The matched region is just those two statements (not the entire enclosing block), so a deletion like:

```
@@
match: strict
@@
  bar();
- foo(x);
```

removes only the `foo(x);` line at every site where it immediately follows `bar();`, leaving surrounding statements untouched.

Notes:
- "Immediate succession" means no statements between the named ones in source. To allow intervening statements, write an explicit `...` between them.
- The semantic is positional: order matters and the statements must be adjacent. To match an unordered set (e.g. members of an object body), use `match: partial`.
- Single-statement patterns are unaffected; they match any AST node of the matching shape, as before.

## Comments

Tree-sitter "extras" — comments, and in some grammars whitespace tokens — are filtered automatically when the pattern doesn't mention them. A pattern matches regardless of where comments appear in the source:

```
@@
match: strict
metavar $A: single
metavar $B: single
metavar $C: single
@@
 { a: $A, b: $B, c: $C }
```

This matches `{ a: 1, b: 2, /* note */ c: 3 }` and `{ a: 1, b: 2, c: 3 }` — comments are transparent.

To require a comment, include one in the pattern body. Source comments at the corresponding position must then match the pattern's comment text exactly:

```
@@
match: strict
metavar $A: single
metavar $B: single
metavar $C: single
@@
 { a: $A, b: $B, /* note */ c: $C }
```

This matches `{ a: 1, b: 2, /* note */ c: 3 }` but not `{ a: 1, b: 2, c: 3 }` — the pattern explicitly demands the comment.

Notes:
- Comments are matched by exact text; metavariables inside comment bodies are not interpolated.
- The behavior applies uniformly to `match: strict`, `match: partial`, and `match: field`.
- Whether `//` and `/* */` are interchangeable depends on the grammar — some expose them as the same node type, others as distinct types. A pattern using one form may not match source using the other in grammars that distinguish them.

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

`on $VAR` requires `$VAR` to be a **single** metavar (it scopes the section to
that one bound subtree). To iterate a **sequence** metavar element-by-element,
use `foreach $VAR` instead — the section's pattern is matched against, and its
`-`/`+` transform applied to, each element in turn.

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

`Matcher.find` and `Matcher.transform` handle single- and multi-section
patterns alike — there is no separate multi-section entry point.

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
diffract apply patch.txt source.ts

# Apply changes in place
diffract apply --in-place patch.txt source.ts

# Apply across a directory
diffract apply --in-place --include '*.ts' patch.txt src/
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

`match: field` matches a declaration while ignoring the optional fields the
pattern omits. On transform it is **surgical**: the edit covers only the part
the `-`/`+` lines mark, and the omitted optional fields — decorators,
annotations, modifiers, return types — are preserved.

```
@@
match: field
metavar $NAME: single
metavar $BODY: single
@@
- fun $NAME() { $BODY }
+ fun $NAME() { logCall(); $BODY }
```

against `@Deprecated private fun f() { ... }` keeps `@Deprecated private` and
rewrites the `fun … { … }` part. Marking the *whole* declaration (a body with
no context line) still replaces it whole — and then the ignored fields are
dropped, which the matcher warns about.

### How match modes affect transforms

Transforms are **surgical** in all modes: the edit is localized to the `-`/`+`
lines, and everything else is preserved byte-for-byte. A pattern whose `-`/`+`
cover the *whole* match replaces the whole match (the degenerate single-region
case); a pattern that marks only a sub-part edits only that part.

| Mode | A sub-part `-`/`+` edit | Preserved |
|------|-------------------------|-----------|
| `strict` | edits the marked lines | context, `...`-captured siblings |
| `partial` | edits the marked element(s), separator-aware | tolerated extra elements |
| `field` | edits the marked part of the declaration | ignored optional fields (decorators, modifiers, return types) |

The remaining footgun is marking the **whole container** in partial/field
mode (a body with no context line): that replaces it whole and drops the
tolerated extras / ignored fields, so the matcher warns (`pattern_warnings`).
To edit a sub-part, mark only that part (context lines, and partial's
unlisted elements, are preserved), or use `foreach`/`on`.

**Write the `+` as bare content.** A `+` line is spliced in place over what the
`-` matched, so it should carry only the new content — not the structural
indentation or the element separator the container already supplies. Against
`{ color: red, size: 10 }`, write `+ colour: $v` (not `+   colour: $v,`); the
surrounding indentation and commas are kept by the splice, and restating them
would double them.

### Sequence rendering: `join` and `foreach`

When a `sequence` metavar is referenced in a `+` template, its elements are
rendered and substituted in place. Two independent knobs:

- **`join $VAR by "<sep>"`** (a preamble directive) — the string placed between
  rendered elements (interpreting `\n`, `\t`, `\\`); default is empty.
- **`foreach $VAR`** (a following `@@` section) — a per-element transform: the
  section matches one element and its `-`/`+` lines rewrite it, and the results
  are joined. Without a `foreach`, elements render as their source text.

The sequence metavar must be declared `metavar $VAR: sequence` and appear on the
match side.

#### Example: identity render with a separator

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

#### Example: converting a match expression to a method chain

The outer section captures the call and names the sequence of case pairs; a
`foreach` section describes how to rewrite each pair. The outer template
references `$CASES`, which is replaced by the joined per-element results (here
the default empty join).

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

Given `matchExhaustive(tag, { A: () => 1, B: () => 2 });` the result is
`match(tag).with("A", () => 1).with("B", () => 2).exhaustive();`.

A `foreach` element with an empty replacement (a `-` line, no `+`) deletes the
element and cleans up the adjacent separator — useful for removing a deprecated
property or unused argument from a list.

#### Constraints

- `join`/`foreach` target `sequence` metavars that also appear on the match
  side.
- A `foreach` section is a normal section (its own `match:` mode and metavars);
  it cannot itself contain a nested `foreach`.

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

(* Pattern matching with concrete syntax. `find` returns composite_match list
   (one entry per section per match); print each matched span. *)
let pattern_text = {|@@
match: strict
metavar $obj: single
metavar $method: single
@@
$obj.$method()|} in
let composites =
  Diffract.Matcher.find ~ctx ~language:"typescript" ~pattern_text
    ~source_text:code
in
List.iter
  (fun (c : Diffract.Matcher.composite_match) ->
    List.iter
      (fun (r : Diffract.Matcher.M.match_result) ->
        Printf.printf "match: %s\n"
          (String.sub code r.start_byte (r.end_byte - r.start_byte)))
      c.sections)
  composites

(* Apply a semantic patch. `transform` returns the rewritten source; render a
   diff with Text_diff if it changed. *)
let patch = {|@@
match: strict
metavar $MSG: single
@@
- console.log($MSG)
+ logger.info($MSG)|} in
let transformed =
  Diffract.Matcher.transform ~ctx ~language:"typescript" ~pattern_text:patch
    ~source_text:code
in
if transformed <> code then
  print_string
    (Diffract.Text_diff.generate_diff ~file_path:"source.ts" ~original:code
       ~transformed)

(* Multi-section, foreach, and conjunctive patches all go through the same
   `transform` — there is no separate entry point. *)
let patch = {|@@
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
+ .with("$KEY", $VAL)|} in
print_string
  (Diffract.Matcher.transform ~ctx ~language:"typescript" ~pattern_text:patch
     ~source_text:code)
```
