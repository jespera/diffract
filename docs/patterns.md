# Pattern File Format

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
metavar $class_name: single
metavar $body: single
@@
class $class_name { $body }

@@
metavar $msg: single
@@
console.log($msg)
```

This finds `console.log` calls only inside class bodies, not at the top level.

Use `on $VAR` to match directly against a previously-bound node instead of traversing its subtree:

```
@@
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
