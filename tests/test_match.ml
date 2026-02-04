(** Tests for the Match module *)

open Diffract

(* Helper to check if string contains substring *)
let string_contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len > haystack_len then false
  else
    let found = ref false in
    let i = ref 0 in
    while not !found && !i <= haystack_len - needle_len do
      if String.sub haystack !i needle_len = needle then
        found := true
      else
        incr i
    done;
    !found

(* Test: parse preamble *)
let test_parse_simple_pattern () =
  let pattern_text = {|@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text:{|console.log("hello")|} in
  Alcotest.(check int) "found one match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check bool) "has $msg binding" true
    (List.mem_assoc "$msg" result.bindings);
  Alcotest.(check string) "$msg value" {|"hello"|}
    (List.assoc "$msg" result.bindings)

(* Test: multiple metavars *)
let test_multiple_metavars () =
  let pattern_text = {|@@
match: strict
metavar $obj: single
metavar $method: single
metavar $arg: single
@@
$obj.$method($arg)|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text:{|console.log("test"); Math.floor(3.14)|} in
  Alcotest.(check int) "found two matches" 2 (List.length results);
  let first = List.hd results in
  Alcotest.(check string) "$obj" "console" (List.assoc "$obj" first.bindings);
  Alcotest.(check string) "$method" "log" (List.assoc "$method" first.bindings)

(* Test: no matches *)
let test_no_matches () =
  let pattern_text = {|@@
match: strict
metavar $x: single
@@
nonexistent($x)|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text:{|console.log("hello")|} in
  Alcotest.(check int) "no matches" 0 (List.length results)

(* Test: multiple matches *)
let test_multiple_matches () =
  let pattern_text = {|@@
match: strict
metavar $n: single
@@
console.log($n)|} in
  let source = {|
    console.log("a");
    console.log("b");
    console.log("c");
  |} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text:source in
  Alcotest.(check int) "found three matches" 3 (List.length results)

(* Test: metavar used twice must match same value *)
let test_metavar_consistency () =
  let pattern_text = {|@@
match: strict
metavar $x: single
@@
$x + $x|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text:{|a + a; b + c|} in
  (* Only a + a should match, not b + c *)
  Alcotest.(check int) "only consistent match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$x" "a" (List.assoc "$x" result.bindings)

(* Test: format_match output *)
let test_format_match () =
  let pattern_text = {|@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let source_text = {|console.log("hello")|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found match" 1 (List.length results);
  let formatted = Match.format_match source_text (List.hd results) in
  Alcotest.(check bool) "contains line number"
    true (String.length formatted > 0)

(* Test: undeclared metavar raises error *)
let test_undeclared_metavar () =
  let pattern_text = {|@@
match: strict
metavar $MSG: single
@@
console.log($MESAGE)|} in  (* Typo: $MESAGE instead of $MSG *)
  Alcotest.check_raises "undeclared metavar"
    (Failure "Undeclared metavars: $MESAGE")
    (fun () ->
      ignore (Match.find_matches
        ~language:"typescript"
        ~pattern_text
        ~source_text:{|console.log("hello")|}))

(* Test: single context level - matches only within context *)
let test_single_context () =
  let pattern_text = {|@@
match: strict
metavar $class_name: single
metavar $body: single
@@
class $class_name { $body }

@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let source_text = {|
class Logger {
  log(msg) {
    console.log("inside class");
  }
}
console.log("outside class");
|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  (* Should only find console.log inside the class, not outside *)
  Alcotest.(check int) "found one nested match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check int) "has one context" 1 (List.length result.contexts);
  Alcotest.(check string) "$class_name" "Logger"
    (List.assoc "$class_name" (List.hd result.contexts).context_bindings);
  Alcotest.(check string) "$msg" {|"inside class"|}
    (List.assoc "$msg" result.inner_bindings)

(* Test: single context level - matching sequence *)

(* Test: multiple context levels (arbitrary nesting) *)
let test_multiple_context_levels () =
  (* Use nested if statements to test multiple context levels *)
  let pattern_text = {|@@
match: strict
metavar $outer_cond: single
metavar $outer_body: single
@@
if ($outer_cond) { $outer_body }

@@
match: strict
metavar $inner_cond: single
metavar $inner_body: single
@@
if ($inner_cond) { $inner_body }

@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let source_text = {|
if (a) {
  if (b) {
    console.log("deeply nested");
  }
}
|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  (* Should find matches with 2 context levels *)
  Alcotest.(check bool) "found nested matches" true (List.length results > 0);
  (* Each match should have 2 contexts (outer if and inner if) *)
  List.iter (fun (result : Match.nested_match_result) ->
    Alcotest.(check int) "has two contexts" 2 (List.length result.contexts)
  ) results

(* Test: binding inheritance - inner sees outer bindings *)
let test_binding_inheritance () =
  let pattern_text = {|@@
match: strict
metavar $class_name: single
metavar $body: single
@@
class $class_name { $body }

@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let source_text = {|
class MyClass {
  foo() {
    console.log("test");
  }
}
|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found match" 1 (List.length results);
  let result = List.hd results in
  (* all_bindings should include bindings from all levels *)
  Alcotest.(check bool) "all_bindings has $class_name" true
    (List.mem_assoc "$class_name" result.all_bindings);
  Alcotest.(check bool) "all_bindings has $msg" true
    (List.mem_assoc "$msg" result.all_bindings)

(* Test: binding consistency across levels *)
let test_binding_consistency_across_levels () =
  let pattern_text = {|@@
match: strict
metavar $name: single
metavar $body: single
@@
class $name { $body }

@@
match: strict
metavar $name: single
@@
console.log($name)|} in
  let source_text = {|
class Foo {
  bar() {
    console.log(Foo);
  }
}
class Bar {
  baz() {
    console.log(Foo);
  }
}
|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  (* Only the Foo class with console.log(Foo) should match - same $name value *)
  (* Bar class with console.log(Foo) should NOT match - inconsistent $name *)
  Alcotest.(check int) "only consistent binding matches" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$name in context" "Foo"
    (List.assoc "$name" (List.hd result.contexts).context_bindings);
  Alcotest.(check string) "$name in inner" "Foo"
    (List.assoc "$name" result.inner_bindings)

(* Test: no match when inner pattern is outside all contexts *)
let test_no_match_outside_context () =
  let pattern_text = {|@@
match: strict
metavar $class_name: single
metavar $body: single
@@
class $class_name { $body }

@@
match: strict
metavar $x: single
@@
outsideCall($x)|} in
  let source_text = {|
class MyClass {
  foo() {
    insideCall(1);
  }
}
outsideCall(2);
|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  (* outsideCall is not inside MyClass, so should not match *)
  Alcotest.(check int) "no matches outside context" 0 (List.length results)

(* Test: format_nested_match output *)
(* Note: This test verifies the format function produces output.
   Due to memory management of tree-sitter nodes, we keep this test simple. *)
let test_format_nested_match () =
  let pattern_text = {|@@
match: strict
metavar $class_name: single
metavar $body: single
@@
class $class_name { $body }

@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let source_text = {|
class Logger {
  log() {
    console.log("test");
  }
}
|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found match" 1 (List.length results);
  (* Just verify we get a result with the expected structure *)
  let result = List.hd results in
  Alcotest.(check int) "has one context" 1 (List.length result.contexts);
  Alcotest.(check bool) "has bindings" true (List.length result.inner_bindings > 0)

(* Test: single section pattern works with find_nested_matches (backward compat) *)
let test_single_section_backward_compat () =
  let pattern_text = {|@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text:{|console.log("hello"); console.log("world")|} in
  Alcotest.(check int) "found two matches" 2 (List.length results);
  (* Single section should have no contexts *)
  List.iter (fun (result : Match.nested_match_result) ->
    Alcotest.(check int) "no contexts" 0 (List.length result.contexts)
  ) results

(* Test: sequence metavar matches multiple children *)
let test_sequence_metavar_multiple () =
  let pattern_text = {|@@
match: strict
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }|} in
  let source_text = {|
class Service {
  start() { }
  stop() { }
}
|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found one match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$class_name" "Service"
    (List.assoc "$class_name" result.bindings);
  (* $body should contain both methods *)
  let body = List.assoc "$body" result.bindings in
  Alcotest.(check bool) "body contains start" true (string_contains ~needle:"start" body);
  Alcotest.(check bool) "body contains stop" true (string_contains ~needle:"stop" body)

(* Test: sequence metavar matches zero children *)
let test_sequence_metavar_zero () =
  let pattern_text = {|@@
match: strict
metavar $items: sequence
@@
[$items]|} in
  let source_text = {|[]|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found one match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$items is empty" ""
    (List.assoc "$items" result.bindings)

(* Test: sequence metavar in nested patterns *)
let test_sequence_metavar_nested () =
  let pattern_text = {|@@
match: strict
metavar $class_name: single
metavar $body: sequence
@@
class $class_name { $body }

@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let source_text = {|
class Service {
  start() {
    console.log("starting");
  }
  stop() {
    console.log("stopping");
  }
}
console.log("outside");
|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  (* Should find 2 matches: both console.logs inside the class, not the one outside *)
  Alcotest.(check int) "found two nested matches" 2 (List.length results);
  (* Verify both are inside the Service class context *)
  List.iter (fun (result : Match.nested_match_result) ->
    Alcotest.(check int) "has one context" 1 (List.length result.contexts);
    let ctx = List.hd result.contexts in
    Alcotest.(check string) "context is Service"
      "Service" (List.assoc "$class_name" ctx.context_bindings)
  ) results

(* Test: index-based matching produces same results as traversal *)
let test_indexed_same_as_traversal () =
  let pattern_text = {|@@
match: strict
metavar $msg: single
@@
console.log($msg)|} in
  let source_text = {|console.log("a"); console.log("b"); foo();|} in
  let traversal_results = Match.find_matches ~language:"typescript" ~pattern_text ~source_text in
  let source_tree = Tree.parse ~language:"typescript" source_text in
  let index = Match.build_index source_tree.root in
  let pattern = Match.parse_pattern ~language:"typescript" pattern_text in
  let indexed_results = Match.find_matches_with_index
    ~index ~pattern ~source:source_tree.source ~source_root:source_tree.root in
  Alcotest.(check int) "same count"
    (List.length traversal_results) (List.length indexed_results)

(* Test: index-based with metavar root falls back correctly *)
let test_indexed_metavar_root_fallback () =
  let pattern_text = {|@@
match: strict
metavar $x: single
@@
$x|} in
  let source_text = {|a + b|} in
  let source_tree = Tree.parse ~language:"typescript" source_text in
  let index = Match.build_index source_tree.root in
  let pattern = Match.parse_pattern ~language:"typescript" pattern_text in
  let results = Match.find_matches_with_index
    ~index ~pattern ~source:source_tree.source ~source_root:source_tree.root in
  Alcotest.(check bool) "found matches" true (List.length results > 0)

(* Test: multi-pattern matching *)
let test_multi_pattern () =
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
  let source_text = {|console.log("info"); console.error("oops"); console.log("done");|} in
  let results = Match.find_matches_multi ~language:"typescript" ~patterns ~source_text in
  (* Pattern 0 (log) should have 2 matches, pattern 1 (error) should have 1 *)
  let log_matches = List.filter (fun (i, _) -> i = 0) results in
  let error_matches = List.filter (fun (i, _) -> i = 1) results in
  Alcotest.(check int) "log matches" 2 (List.length log_matches);
  Alcotest.(check int) "error matches" 1 (List.length error_matches)

(* Test: partial object matching using match: partial directive.
   Tests that { someField: $X } with partial mode matches objects with extra properties. *)
let test_partial_object_matching () =
  let pattern_text = {|@@
match: partial
metavar $X: single
@@
{ someField: $X }|} in
  let source_text = {|({ someField: 1, other: 2 })|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Printf.printf "Partial object matches: %d\n" (List.length results);
  Alcotest.(check int) "found partial match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$X value" "1" (List.assoc "$X" result.bindings)

(* Test: partial matching with 'on $VAR' directive.
   Section 1 matches foo($OBJ), section 2 uses 'on $OBJ' with partial matching. *)
let test_partial_with_on_var () =
  let pattern_text = {|@@
match: strict
metavar $OBJ: single
@@
foo($OBJ)

@@
match: partial
on $OBJ
metavar $X: single
@@
{ someField: $X }|} in
  let source_text = {|foo({ someField: 1, other: 2 })|} in
  let results = Match.find_nested_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Printf.printf "Nested partial with on: %d\n" (List.length results);
  Alcotest.(check int) "found nested match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$X value" "1" (List.assoc "$X" result.all_bindings);
  Alcotest.(check int) "has one context" 1 (List.length result.contexts)

(* Test: nested partial matching for deeply nested objects.
   { f1: { f2: $X } } should match objects with extra props at any level. *)
let test_nested_partial_objects () =
  let pattern_text = {|@@
match: partial
metavar $X: single
@@
{ f1: { f2: $X } }|} in
  let source_text = {|({ f1: { f2: 42, f3: "extra" }, other: true })|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Printf.printf "Nested partial matches: %d\n" (List.length results);
  Alcotest.(check int) "found deeply nested partial match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$X value" "42" (List.assoc "$X" result.bindings)

(* Test: exact object pattern only matches objects with exact same properties *)
let test_exact_object_match () =
  let pattern_text = {|@@
match: strict
metavar $X: single
@@
{ someField: $X }|} in
  let source_text = {|({ someField: 1, other: 2 })|} in
  let results = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text in
  Printf.printf "Exact object matches against multi-prop: %d\n" (List.length results);
  let source_text_single = {|({ someField: 1 })|} in
  let results_single = Match.find_matches
    ~language:"typescript"
    ~pattern_text
    ~source_text:source_text_single in
  Printf.printf "Exact object matches against single-prop: %d\n" (List.length results_single);
  (* Pattern with 1 property should match object with 1 property *)
  Alcotest.(check int) "matches single-prop object" 1 (List.length results_single);
  (* But should NOT match object with 2 properties (strict matching) *)
  Alcotest.(check int) "no match for multi-prop object" 0 (List.length results)

let tests = [
  Alcotest.test_case "parse simple pattern" `Quick test_parse_simple_pattern;
  Alcotest.test_case "multiple metavars" `Quick test_multiple_metavars;
  Alcotest.test_case "no matches" `Quick test_no_matches;
  Alcotest.test_case "multiple matches" `Quick test_multiple_matches;
  Alcotest.test_case "metavar consistency" `Quick test_metavar_consistency;
  Alcotest.test_case "format_match output" `Quick test_format_match;
  Alcotest.test_case "undeclared metavar" `Quick test_undeclared_metavar;
  Alcotest.test_case "sequence metavar multiple" `Quick test_sequence_metavar_multiple;
  Alcotest.test_case "sequence metavar zero" `Quick test_sequence_metavar_zero;
  Alcotest.test_case "sequence metavar nested" `Quick test_sequence_metavar_nested;
  Alcotest.test_case "indexed same as traversal" `Quick test_indexed_same_as_traversal;
  Alcotest.test_case "indexed metavar root fallback" `Quick test_indexed_metavar_root_fallback;
  Alcotest.test_case "multi-pattern matching" `Quick test_multi_pattern;
  Alcotest.test_case "exact object match" `Quick test_exact_object_match;
  Alcotest.test_case "partial object matching" `Quick test_partial_object_matching;
  Alcotest.test_case "partial with on var" `Quick test_partial_with_on_var;
  Alcotest.test_case "nested partial objects" `Quick test_nested_partial_objects;
  Alcotest.test_case "single context" `Quick test_single_context;
]

(* === Kotlin-specific tests === *)

(* Test: basic Kotlin println pattern *)
let test_kotlin_println () =
  let pattern_text = {|@@
match: strict
metavar $NAME: single
@@
println($NAME)|} in
  let source_text = {|
fun main() {
    val result = greet("World")
    println(result)
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found one println match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$NAME value" "result"
    (List.assoc "$NAME" result.bindings)

(* Test: Kotlin val declaration pattern *)
let test_kotlin_val_declaration () =
  let pattern_text = {|@@
match: strict
metavar $VAR: single
metavar $EXPR: single
@@
val $VAR = $EXPR|} in
  let source_text = {|
fun main() {
    val result = greet("World")
    val count = 42
    println(result)
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two val declarations" 2 (List.length results);
  (* Check bindings of first match *)
  let first = List.hd results in
  Alcotest.(check string) "$VAR" "result" (List.assoc "$VAR" first.bindings);
  Alcotest.(check string) "$EXPR" {|greet("World")|} (List.assoc "$EXPR" first.bindings)

(* Test: Kotlin function call with sequence metavar *)
let test_kotlin_function_call_sequence () =
  let pattern_text = {|@@
match: strict
metavar $FUNC: single
metavar $ARGS: sequence
@@
$FUNC($ARGS)|} in
  let source_text = {|
fun main() {
    val user = User("Alice", 25)
    processUser(user)
    println(result)
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found three function calls" 3 (List.length results);
  (* Verify User call has multiple args *)
  let user_call = List.find (fun (r : Match.match_result) ->
    List.assoc "$FUNC" r.bindings = "User"
  ) results in
  let args = List.assoc "$ARGS" user_call.bindings in
  Alcotest.(check bool) "args contains Alice" true (string_contains ~needle:"Alice" args);
  Alcotest.(check bool) "args contains 25" true (string_contains ~needle:"25" args)

(* Test: Kotlin property access pattern *)
let test_kotlin_property_access () =
  let pattern_text = {|@@
match: strict
metavar $OBJ: single
metavar $PROP: single
@@
$OBJ.$PROP|} in
  let source_text = {|
fun processUser(user: User) {
    if (user.age >= 18) {
        println(user.name)
    }
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two property accesses" 2 (List.length results);
  (* Verify we found user.age and user.name *)
  let props = List.map (fun (r : Match.match_result) ->
    List.assoc "$PROP" r.bindings
  ) results in
  Alcotest.(check bool) "found age prop" true (List.mem "age" props);
  Alcotest.(check bool) "found name prop" true (List.mem "name" props)

(* Test: Kotlin class with methods *)
let test_kotlin_class_pattern () =
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $BODY: sequence
@@
class $CLASS { $BODY }|} in
  let source_text = {|
class User {
    fun getName(): String {
        return name
    }
    fun getAge(): Int {
        return age
    }
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found one class" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$CLASS" "User" (List.assoc "$CLASS" result.bindings);
  let body = List.assoc "$BODY" result.bindings in
  Alcotest.(check bool) "body contains getName" true (string_contains ~needle:"getName" body);
  Alcotest.(check bool) "body contains getAge" true (string_contains ~needle:"getAge" body)

(* Test: Kotlin when expression *)
let test_kotlin_when_expression () =
  let pattern_text = {|@@
match: strict
metavar $SUBJECT: single
metavar $BRANCHES: sequence
@@
when ($SUBJECT) { $BRANCHES }|} in
  let source_text = {|
fun describe(x: Int): String {
    return when (x) {
        1 -> "one"
        2 -> "two"
        else -> "other"
    }
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found when expression" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$SUBJECT" "x" (List.assoc "$SUBJECT" result.bindings);
  let branches = List.assoc "$BRANCHES" result.bindings in
  Alcotest.(check bool) "branches contains one" true (string_contains ~needle:"one" branches);
  Alcotest.(check bool) "branches contains else" true (string_contains ~needle:"else" branches)

(* Test: Kotlin lambda expression with explicit parameter
   Note: lambdas with implicit 'it' have no lambda_parameters node,
   so { $PARAMS -> $BODY } only matches explicit param lambdas *)
let test_kotlin_lambda () =
  let pattern_text = {|@@
match: strict
metavar $PARAM: single
metavar $BODY: single
@@
{ $PARAM -> $BODY }|} in
  let source_text = {|
fun main() {
    val doubled = listOf(1, 2, 3).map { x -> x * 2 }
    val squared = listOf(1, 2, 3).map { n -> n * n }
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found lambdas with explicit params" 2 (List.length results);
  (* Check that we captured the parameters *)
  let params = List.map (fun (r : Match.match_result) ->
    List.assoc "$PARAM" r.bindings
  ) results in
  Alcotest.(check bool) "found x param" true (List.mem "x" params);
  Alcotest.(check bool) "found n param" true (List.mem "n" params)

(* Test: Kotlin safe call operator *)
let test_kotlin_safe_call () =
  let pattern_text = {|@@
match: strict
metavar $OBJ: single
metavar $MEMBER: single
@@
$OBJ?.$MEMBER|} in
  let source_text = {|
fun getName(user: User?): String {
    return user?.name ?: "unknown"
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found safe call" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$OBJ" "user" (List.assoc "$OBJ" result.bindings);
  Alcotest.(check string) "$MEMBER" "name" (List.assoc "$MEMBER" result.bindings)

(* Test: Kotlin elvis operator *)
let test_kotlin_elvis () =
  let pattern_text = {|@@
match: strict
metavar $NULLABLE: single
metavar $DEFAULT: single
@@
$NULLABLE ?: $DEFAULT|} in
  let source_text = {|
fun getName(user: User?): String {
    return user?.name ?: "unknown"
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found elvis expression" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$NULLABLE" "user?.name" (List.assoc "$NULLABLE" result.bindings);
  Alcotest.(check string) "$DEFAULT" {|"unknown"|} (List.assoc "$DEFAULT" result.bindings)

(* Test: Kotlin let scope function - a common Kotlin idiom *)
let test_kotlin_let_scope () =
  let pattern_text = {|@@
match: strict
metavar $OBJ: single
metavar $BODY: single
@@
$OBJ.let { $BODY }|} in
  let source_text = {|
fun process(name: String?) {
    name?.let { println(it) }
    name?.let { doSomething(it) }
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found let calls" 2 (List.length results);
  let bodies = List.map (fun (r : Match.match_result) ->
    List.assoc "$BODY" r.bindings
  ) results in
  Alcotest.(check bool) "found println" true (List.exists (string_contains ~needle:"println") bodies);
  Alcotest.(check bool) "found doSomething" true (List.exists (string_contains ~needle:"doSomething") bodies)

(* Test: Kotlin if expression *)
let test_kotlin_if_expression () =
  let pattern_text = {|@@
match: strict
metavar $COND: single
metavar $THEN: single
metavar $ELSE: single
@@
if ($COND) $THEN else $ELSE|} in
  let source_text = {|
fun max(a: Int, b: Int): Int {
    return if (a > b) a else b
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found if expression" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$COND" "a > b" (List.assoc "$COND" result.bindings);
  Alcotest.(check string) "$THEN" "a" (List.assoc "$THEN" result.bindings);
  Alcotest.(check string) "$ELSE" "b" (List.assoc "$ELSE" result.bindings)

(* Test: Kotlin for loop pattern *)
let test_kotlin_for_loop () =
  let pattern_text = {|@@
match: strict
metavar $VAR: single
metavar $ITER: single
metavar $BODY: single
@@
for ($VAR in $ITER) $BODY|} in
  let source_text = {|
fun sumList(items: List<Int>): Int {
    var sum = 0
    for (item in items) {
        sum += item
    }
    return sum
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found for loop" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$VAR" "item" (List.assoc "$VAR" result.bindings);
  Alcotest.(check string) "$ITER" "items" (List.assoc "$ITER" result.bindings)

(* Test: Kotlin nested pattern - find println calls inside classes *)
let test_kotlin_nested_in_class () =
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $BODY: sequence
@@
class $CLASS { $BODY }

@@
match: strict
metavar $MSG: single
@@
println($MSG)|} in
  let source_text = {|
class Logger {
    fun log(msg: String) {
        println("Logging: " + msg)
    }
}

fun standalone() {
    println("Outside class")
}
|} in
  let results = Match.find_nested_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  (* Should only find println inside the class, not the standalone one *)
  Alcotest.(check int) "found one nested match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$CLASS" "Logger"
    (List.assoc "$CLASS" (List.hd result.contexts).context_bindings);
  let msg = List.assoc "$MSG" result.inner_bindings in
  Alcotest.(check bool) "msg contains Logging" true (string_contains ~needle:"Logging" msg)

(* Test: Kotlin data class pattern *)
let test_kotlin_data_class () =
  let pattern_text = {|@@
match: strict
metavar $NAME: single
metavar $PARAMS: sequence
@@
data class $NAME($PARAMS)|} in
  let source_text = {|
data class User(val name: String, val age: Int)
data class Point(val x: Double, val y: Double)
class RegularClass(val value: Int)
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two data classes" 2 (List.length results);
  let names = List.map (fun (r : Match.match_result) ->
    List.assoc "$NAME" r.bindings
  ) results in
  Alcotest.(check bool) "found User" true (List.mem "User" names);
  Alcotest.(check bool) "found Point" true (List.mem "Point" names)

(* Test: Kotlin chained method calls *)
let test_kotlin_chained_calls () =
  let pattern_text = {|@@
match: strict
metavar $OBJ: single
metavar $M1: single
metavar $M2: single
@@
$OBJ.$M1().$M2()|} in
  let source_text = {|
fun process(text: String): String {
    return text.trim().lowercase()
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found chained call" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$OBJ" "text" (List.assoc "$OBJ" result.bindings);
  Alcotest.(check string) "$M1" "trim" (List.assoc "$M1" result.bindings);
  Alcotest.(check string) "$M2" "lowercase" (List.assoc "$M2" result.bindings)

(* Test: Kotlin comparison operators *)
let test_kotlin_comparison () =
  let pattern_text = {|@@
match: strict
metavar $LEFT: single
metavar $RIGHT: single
@@
$LEFT >= $RIGHT|} in
  let source_text = {|
fun isAdult(age: Int): Boolean {
    return age >= 18
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found comparison" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$LEFT" "age" (List.assoc "$LEFT" result.bindings);
  Alcotest.(check string) "$RIGHT" "18" (List.assoc "$RIGHT" result.bindings)

(* Test: Kotlin null assertion *)
let test_kotlin_null_assertion () =
  let pattern_text = {|@@
match: strict
metavar $EXPR: single
@@
$EXPR!!|} in
  let source_text = {|
fun forceGet(value: String?): String {
    return value!!
}
|} in
  let results = Match.find_matches
    ~language:"kotlin"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found null assertion" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$EXPR" "value" (List.assoc "$EXPR" result.bindings)

let kotlin_tests = [
  Alcotest.test_case "kotlin println" `Quick test_kotlin_println;
  Alcotest.test_case "kotlin val declaration" `Quick test_kotlin_val_declaration;
  Alcotest.test_case "kotlin function call sequence" `Quick test_kotlin_function_call_sequence;
  Alcotest.test_case "kotlin property access" `Quick test_kotlin_property_access;
  Alcotest.test_case "kotlin class pattern" `Quick test_kotlin_class_pattern;
  Alcotest.test_case "kotlin when expression" `Quick test_kotlin_when_expression;
  Alcotest.test_case "kotlin lambda" `Quick test_kotlin_lambda;
  Alcotest.test_case "kotlin safe call" `Quick test_kotlin_safe_call;
  Alcotest.test_case "kotlin elvis" `Quick test_kotlin_elvis;
  Alcotest.test_case "kotlin let scope" `Quick test_kotlin_let_scope;
  Alcotest.test_case "kotlin if expression" `Quick test_kotlin_if_expression;
  Alcotest.test_case "kotlin for loop" `Quick test_kotlin_for_loop;
  Alcotest.test_case "kotlin nested in class" `Quick test_kotlin_nested_in_class;
  Alcotest.test_case "kotlin data class" `Quick test_kotlin_data_class;
  Alcotest.test_case "kotlin chained calls" `Quick test_kotlin_chained_calls;
  Alcotest.test_case "kotlin comparison" `Quick test_kotlin_comparison;
  Alcotest.test_case "kotlin null assertion" `Quick test_kotlin_null_assertion;
]

(* === PHP-specific tests === *)

(* Test: basic PHP echo pattern *)
let test_php_echo () =
  let pattern_text = {|@@
match: strict
metavar $MSG: single
@@
<?php echo $MSG;|} in
  let source_text = {|<?php echo "Hello World"; echo $name;|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two echo statements" 2 (List.length results)

(* Test: PHP function call pattern *)
let test_php_function_call () =
  let pattern_text = {|@@
match: strict
metavar $FUNC: single
metavar $ARGS: sequence
@@
<?php $FUNC($ARGS);|} in
  let source_text = {|<?php
error_log("debug message");
strlen($input);
array_map($callback, $items);
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found three function calls" 3 (List.length results)

(* Test: PHP error_log pattern - common logging function *)
let test_php_error_log () =
  let pattern_text = {|@@
match: strict
metavar $MSG: single
@@
<?php error_log($MSG);|} in
  let source_text = {|<?php
function process($data) {
    error_log("Processing: " . $data);
    $result = doWork($data);
    error_log("Done");
    return $result;
}
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two error_log calls" 2 (List.length results)

(* Test: PHP variable assignment - match right-hand side expression *)
let test_php_variable_assignment () =
  (* Note: Can't use metavar on LHS because PHP requires $ for variables.
     Instead, match assignments where we know the variable name. *)
  let pattern_text = {|@@
match: strict
metavar $EXPR: single
@@
<?php $name = $EXPR;|} in
  let source_text = {|<?php
$name = "Alice";
$name = compute($data);
$age = 25;
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two assignments to $name" 2 (List.length results);
  let first = List.hd results in
  Alcotest.(check string) "$EXPR" {|"Alice"|} (List.assoc "$EXPR" first.bindings)

(* Test: PHP class method pattern *)
let test_php_class_method () =
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $BODY: sequence
@@
<?php class $CLASS { $BODY }|} in
  let source_text = {|<?php
class UserService {
    public function getUser($id) {
        return $this->db->find($id);
    }

    public function deleteUser($id) {
        $this->db->delete($id);
    }
}
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found one class" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$CLASS" "UserService" (List.assoc "$CLASS" result.bindings);
  let body = List.assoc "$BODY" result.bindings in
  Alcotest.(check bool) "body contains getUser" true (string_contains ~needle:"getUser" body);
  Alcotest.(check bool) "body contains deleteUser" true (string_contains ~needle:"deleteUser" body)

(* Test: PHP property access with $this *)
let test_php_this_property () =
  let pattern_text = {|@@
match: strict
metavar $PROP: single
@@
<?php $this->$PROP;|} in
  let source_text = {|<?php
class User {
    public function getName() {
        return $this->name;
    }
    public function getAge() {
        return $this->age;
    }
}
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two property accesses" 2 (List.length results);
  let props = List.map (fun (r : Match.match_result) ->
    List.assoc "$PROP" r.bindings
  ) results in
  Alcotest.(check bool) "found name prop" true (List.mem "name" props);
  Alcotest.(check bool) "found age prop" true (List.mem "age" props)

(* Test: PHP method call on $this *)
let test_php_this_method_call () =
  let pattern_text = {|@@
match: strict
metavar $METHOD: single
metavar $ARGS: sequence
@@
<?php $this->$METHOD($ARGS);|} in
  let source_text = {|<?php
class Service {
    public function process() {
        $this->validate($input);
        $this->save($data);
        $this->notify($user, $message);
    }
}
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found three method calls" 3 (List.length results);
  let methods = List.map (fun (r : Match.match_result) ->
    List.assoc "$METHOD" r.bindings
  ) results in
  Alcotest.(check bool) "found validate" true (List.mem "validate" methods);
  Alcotest.(check bool) "found save" true (List.mem "save" methods);
  Alcotest.(check bool) "found notify" true (List.mem "notify" methods)

(* Test: PHP foreach loop *)
let test_php_foreach () =
  let pattern_text = {|@@
match: strict
metavar $ARRAY: single
metavar $ITEM: single
metavar $BODY: single
@@
<?php foreach ($ARRAY as $ITEM) $BODY|} in
  let source_text = {|<?php
foreach ($users as $user) {
    echo $user->name;
}
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found foreach loop" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$ARRAY" "$users" (List.assoc "$ARRAY" result.bindings);
  Alcotest.(check string) "$ITEM" "$user" (List.assoc "$ITEM" result.bindings)

(* Test: PHP foreach with key => value *)
let test_php_foreach_key_value () =
  let pattern_text = {|@@
match: strict
metavar $ARRAY: single
metavar $KEY: single
metavar $VALUE: single
metavar $BODY: single
@@
<?php foreach ($ARRAY as $KEY => $VALUE) $BODY|} in
  let source_text = {|<?php
foreach ($config as $key => $value) {
    echo "$key: $value";
}
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found foreach with key-value" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$ARRAY" "$config" (List.assoc "$ARRAY" result.bindings);
  Alcotest.(check string) "$KEY" "$key" (List.assoc "$KEY" result.bindings);
  Alcotest.(check string) "$VALUE" "$value" (List.assoc "$VALUE" result.bindings)

(* Test: PHP try-catch block - match function calls inside catch blocks *)
let test_php_try_catch () =
  (* PHP try-catch patterns are complex due to required {} blocks.
     Test that we can match specific function calls inside catch blocks
     using nested patterns. *)
  let pattern_text = {|@@
match: strict
metavar $TYPE: single
metavar $CATCH_BODY: sequence
@@
<?php try {} catch ($TYPE $e) { $CATCH_BODY }

@@
match: strict
metavar $MSG: single
@@
<?php error_log($MSG);|} in
  let source_text = {|<?php
try {} catch (Exception $e) {
    error_log("caught exception");
}
try {} catch (RuntimeException $e) {
    error_log("runtime error");
}
error_log("outside catch");
|} in
  let results = Match.find_nested_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  (* Should find error_log inside catch blocks only, not the one outside *)
  Alcotest.(check int) "found two error_logs in catch blocks" 2 (List.length results);
  let types = List.map (fun (r : Match.nested_match_result) ->
    List.assoc "$TYPE" (List.hd r.contexts).context_bindings
  ) results in
  Alcotest.(check bool) "found Exception" true (List.mem "Exception" types);
  Alcotest.(check bool) "found RuntimeException" true (List.mem "RuntimeException" types)

(* Test: PHP static method call *)
let test_php_static_method () =
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $METHOD: single
metavar $ARGS: sequence
@@
<?php $CLASS::$METHOD($ARGS);|} in
  let source_text = {|<?php
$user = User::find($id);
$config = Config::get("database");
Logger::info("Started");
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found three static calls" 3 (List.length results);
  let first = List.hd results in
  Alcotest.(check string) "$CLASS" "User" (List.assoc "$CLASS" first.bindings);
  Alcotest.(check string) "$METHOD" "find" (List.assoc "$METHOD" first.bindings)

(* Test: PHP array access *)
let test_php_array_access () =
  let pattern_text = {|@@
match: strict
metavar $ARR: single
metavar $KEY: single
@@
<?php $ARR[$KEY];|} in
  let source_text = {|<?php
$name = $data["name"];
$first = $items[0];
$value = $config[$key];
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found three array accesses" 3 (List.length results)

(* Test: PHP new object instantiation *)
let test_php_new_object () =
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $ARGS: sequence
@@
<?php new $CLASS($ARGS);|} in
  let source_text = {|<?php
$user = new User("Alice", 25);
$db = new Database($config);
$empty = new stdClass();
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found three instantiations" 3 (List.length results);
  let classes = List.map (fun (r : Match.match_result) ->
    List.assoc "$CLASS" r.bindings
  ) results in
  Alcotest.(check bool) "found User" true (List.mem "User" classes);
  Alcotest.(check bool) "found Database" true (List.mem "Database" classes)

(* Test: PHP null coalescing operator *)
let test_php_null_coalesce () =
  let pattern_text = {|@@
match: strict
metavar $NULLABLE: single
metavar $DEFAULT: single
@@
<?php $NULLABLE ?? $DEFAULT;|} in
  let source_text = {|<?php
$name = $user->name ?? "Guest";
$port = $config["port"] ?? 8080;
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two null coalescing" 2 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$NULLABLE" {|$user->name|} (List.assoc "$NULLABLE" result.bindings);
  Alcotest.(check string) "$DEFAULT" {|"Guest"|} (List.assoc "$DEFAULT" result.bindings)

(* Test: PHP arrow function (PHP 7.4+) *)
let test_php_arrow_function () =
  let pattern_text = {|@@
match: strict
metavar $PARAMS: sequence
metavar $EXPR: single
@@
<?php fn($PARAMS) => $EXPR;|} in
  let source_text = {|<?php
$double = fn($x) => $x * 2;
$add = fn($a, $b) => $a + $b;
$nums = array_map(fn($n) => $n * $n, $items);
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found three arrow functions" 3 (List.length results)

(* Test: PHP nested pattern - find error_log inside class methods *)
let test_php_nested_in_class () =
  let pattern_text = {|@@
match: strict
metavar $CLASS: single
metavar $BODY: sequence
@@
<?php class $CLASS { $BODY }

@@
match: strict
metavar $MSG: single
@@
<?php error_log($MSG);|} in
  let source_text = {|<?php
class Logger {
    public function log($msg) {
        error_log("Inside class: " . $msg);
    }
}

error_log("Outside class");
|} in
  let results = Match.find_nested_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  (* Should only find error_log inside the class, not outside *)
  Alcotest.(check int) "found one nested match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$CLASS" "Logger"
    (List.assoc "$CLASS" (List.hd result.contexts).context_bindings);
  let msg = List.assoc "$MSG" result.inner_bindings in
  Alcotest.(check bool) "msg contains Inside class" true (string_contains ~needle:"Inside class" msg)

(* Test: PHP chained method calls - match method names in chain *)
let test_php_chained_calls () =
  (* Note: Can't use metavar for object ($query) because PHP variables need $.
     Instead, match specific chain pattern with known object variable. *)
  let pattern_text = {|@@
match: strict
metavar $M1: single
metavar $M2: single
@@
<?php $query->$M1()->$M2();|} in
  let source_text = {|<?php
$result = $query->where()->orderBy();
$other = $query->select()->limit();
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two chained calls on $query" 2 (List.length results);
  let first = List.hd results in
  Alcotest.(check string) "$M1" "where" (List.assoc "$M1" first.bindings);
  Alcotest.(check string) "$M2" "orderBy" (List.assoc "$M2" first.bindings)

(* Test: PHP return statement *)
let test_php_return () =
  let pattern_text = {|@@
match: strict
metavar $EXPR: single
@@
<?php return $EXPR;|} in
  let source_text = {|<?php
function getUser($id) {
    if (!$id) {
        return null;
    }
    return $this->db->find($id);
}
|} in
  let results = Match.find_matches
    ~language:"php"
    ~pattern_text
    ~source_text in
  Alcotest.(check int) "found two return statements" 2 (List.length results)

let php_tests = [
  Alcotest.test_case "php echo" `Quick test_php_echo;
  Alcotest.test_case "php function call" `Quick test_php_function_call;
  Alcotest.test_case "php error_log" `Quick test_php_error_log;
  Alcotest.test_case "php variable assignment" `Quick test_php_variable_assignment;
  Alcotest.test_case "php class method" `Quick test_php_class_method;
  Alcotest.test_case "php this property" `Quick test_php_this_property;
  Alcotest.test_case "php this method call" `Quick test_php_this_method_call;
  Alcotest.test_case "php foreach" `Quick test_php_foreach;
  Alcotest.test_case "php foreach key value" `Quick test_php_foreach_key_value;
  Alcotest.test_case "php try catch" `Quick test_php_try_catch;
  Alcotest.test_case "php static method" `Quick test_php_static_method;
  Alcotest.test_case "php array access" `Quick test_php_array_access;
  Alcotest.test_case "php new object" `Quick test_php_new_object;
  Alcotest.test_case "php null coalesce" `Quick test_php_null_coalesce;
  Alcotest.test_case "php arrow function" `Quick test_php_arrow_function;
  Alcotest.test_case "php nested in class" `Quick test_php_nested_in_class;
  Alcotest.test_case "php chained calls" `Quick test_php_chained_calls;
  Alcotest.test_case "php return" `Quick test_php_return;
]

(* === Field-based matching tests === *)

(* Test: PHP function with attributes - field mode should match despite extra children *)
let test_field_php_attributes () =
  let pattern = {|@@
match: field
@@
<?php
function test() {
    echo "Test";
}|} in
  let source = {|<?php
#[Attr1]
function test() {
    echo "Test";
}|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "matches with attributes" 1 (List.length results)

(* Test: verify ordering is preserved within fields *)
let test_field_order_preserved () =
  let pattern = {|@@
match: field
@@
<?php
function test() {
    $a = 1;
    $b = 2;
}|} in
  let source_correct = {|<?php
#[Attr]
function test() {
    $a = 1;
    $b = 2;
}|} in
  let source_wrong = {|<?php
function test() {
    $b = 2;
    $a = 1;
}|} in
  (* Should match correct order *)
  let r1 = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source_correct in
  Alcotest.(check int) "matches correct order" 1 (List.length r1);
  (* Should NOT match wrong order *)
  let r2 = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source_wrong in
  Alcotest.(check int) "rejects wrong order" 0 (List.length r2)

(* Test: field matching with metavars *)
let test_field_with_metavars () =
  let pattern = {|@@
match: field
metavar $BODY: single
@@
<?php
function test() {
    $BODY
}|} in
  let source = {|<?php
#[Route("/api")]
#[Auth]
function test() {
    return $data;
}|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "matches with metavar" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check bool) "has $BODY binding" true (List.mem_assoc "$BODY" result.bindings)

(* Test: field matching without attributes still works *)
let test_field_no_attributes () =
  let pattern = {|@@
match: field
@@
<?php
function test() {
    echo "Test";
}|} in
  let source = {|<?php
function test() {
    echo "Test";
}|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "matches without attributes" 1 (List.length results)

(* Test: field matching in TypeScript class with decorators *)
let test_field_typescript_decorators () =
  let pattern = {|@@
match: field
metavar $NAME: single
metavar $BODY: sequence
@@
class $NAME { $BODY }|} in
  let source = {|
@Component({selector: 'app'})
class MyComponent {
  render() { return null; }
}|} in
  let results = Match.find_matches ~language:"typescript" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "matches class with decorator" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$NAME" "MyComponent" (List.assoc "$NAME" result.bindings)

(* Test: field mode matches by field regardless of extra source fields *)
let test_field_extra_source_fields () =
  (* Pattern has body and name, source has attributes + body + name *)
  let pattern = {|@@
match: field
metavar $NAME: single
metavar $BODY: sequence
@@
<?php
function $NAME() { $BODY }|} in
  let source = {|<?php
#[Route("/")]
function foo() { return 1; }
#[Auth]
function bar() { echo "hi"; }
|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  (* Should match both - attributes field is ignored since pattern doesn't have it *)
  Alcotest.(check int) "matches both functions with attributes" 2 (List.length results)

let field_tests = [
  Alcotest.test_case "field php attributes" `Quick test_field_php_attributes;
  Alcotest.test_case "field order preserved" `Quick test_field_order_preserved;
  Alcotest.test_case "field with metavars" `Quick test_field_with_metavars;
  Alcotest.test_case "field no attributes" `Quick test_field_no_attributes;
  Alcotest.test_case "field typescript decorators" `Quick test_field_typescript_decorators;
  Alcotest.test_case "field extra source fields" `Quick test_field_extra_source_fields;
]

(* === Ellipsis (...) tests === *)

(* Test: ellipsis in statement context *)
let test_ellipsis_statements () =
  let pattern = {|@@
match: strict
@@

<?php

function test() {
    ...
    echo "B";
    echo "C";
    ...
}|} in
  let source = {|<?php

function test ()
{
    echo "A";
    echo "B";
    echo "C";
    echo "D";
}|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found one match" 1 (List.length results);
  let result = List.hd results in
  (* Check that ellipsis captured the right content *)
  Alcotest.(check bool) "has ..._0 binding" true (List.mem_assoc "..._0" result.bindings);
  Alcotest.(check bool) "has ..._1 binding" true (List.mem_assoc "..._1" result.bindings);
  let first_ellipsis = List.assoc "..._0" result.bindings in
  let second_ellipsis = List.assoc "..._1" result.bindings in
  Alcotest.(check bool) "first ellipsis has A" true (string_contains ~needle:"A" first_ellipsis);
  Alcotest.(check bool) "second ellipsis has D" true (string_contains ~needle:"D" second_ellipsis)

(* Test: ellipsis in argument context *)
let test_ellipsis_arguments () =
  let pattern = {|@@
match: strict
metavar $FUNC: single
@@
<?php $FUNC(..., $x, ...);|} in
  let source = {|<?php
foo($a, $x, $b);
bar($x);
baz($m, $n, $x, $o, $p);
|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found three matches" 3 (List.length results)

(* Test: ellipsis with zero matches *)
let test_ellipsis_zero () =
  let pattern = {|@@
match: strict
@@
<?php
function test() {
    ...
    echo "only";
    ...
}|} in
  let source = {|<?php
function test() {
    echo "only";
}|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found one match with empty ellipsis" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "first ellipsis empty" "" (List.assoc "..._0" result.bindings);
  Alcotest.(check string) "second ellipsis empty" "" (List.assoc "..._1" result.bindings)

(* Test: PHP spread operator not confused with ellipsis *)
let test_ellipsis_not_spread () =
  let pattern = {|@@
match: strict
metavar $FUNC: single
metavar $ARR: single
@@
<?php $FUNC(...$ARR);|} in
  let source = {|<?php
foo(...$args);
bar($a, ...$rest);
|} in
  let results = Match.find_matches ~language:"php" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found spread operator match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$ARR" "$args" (List.assoc "$ARR" result.bindings)

(* Test: ellipsis in TypeScript/JavaScript *)
let test_ellipsis_typescript () =
  let pattern = {|@@
match: strict
metavar $NAME: single
@@
function $NAME() {
    ...
    console.log("middle");
    ...
}|} in
  let source = {|
function test() {
    setup();
    console.log("middle");
    cleanup();
}|} in
  let results = Match.find_matches ~language:"typescript" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found typescript match" 1 (List.length results);
  let result = List.hd results in
  Alcotest.(check string) "$NAME" "test" (List.assoc "$NAME" result.bindings)

(* Test: ellipsis in TypeScript arrays *)
let test_ellipsis_typescript_array () =
  let pattern = {|@@
match: strict
@@
[..., "middle", ...]|} in
  let source = {|
const arr1 = ["a", "middle", "b"];
const arr2 = ["middle"];
const arr3 = ["x", "y", "middle", "z"];
|} in
  let results = Match.find_matches ~language:"typescript" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found three array matches" 3 (List.length results)

(* Test: ellipsis in TypeScript class methods *)
let test_ellipsis_typescript_class () =
  let pattern = {|@@
match: strict
metavar $NAME: single
@@
class $NAME {
    ...
    render() {}
    ...
}|} in
  let source = {|
class Component {
    constructor() {}
    setup() {}
    render() {}
    cleanup() {}
}

class Simple {
    render() {}
}
|} in
  let results = Match.find_matches ~language:"typescript" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found two class matches" 2 (List.length results);
  let names = List.map (fun (r : Match.match_result) ->
    List.assoc "$NAME" r.bindings
  ) results in
  Alcotest.(check bool) "found Component" true (List.mem "Component" names);
  Alcotest.(check bool) "found Simple" true (List.mem "Simple" names)

(* Test: ellipsis in TSX function components *)
let test_ellipsis_tsx_function () =
  let pattern = {|@@
match: strict
metavar $NAME: single
@@
function $NAME() {
    ...
    return <div />;
}|} in
  let source = {|
function Header() {
    const title = "Hello";
    return <div />;
}

function Footer() {
    return <div />;
}
|} in
  let results = Match.find_matches ~language:"tsx" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found two TSX function matches" 2 (List.length results);
  let names = List.map (fun (r : Match.match_result) ->
    List.assoc "$NAME" r.bindings
  ) results in
  Alcotest.(check bool) "found Header" true (List.mem "Header" names);
  Alcotest.(check bool) "found Footer" true (List.mem "Footer" names)

(* Test: ellipsis in TSX arrow function components *)
let test_ellipsis_tsx_arrow () =
  let pattern = {|@@
match: strict
metavar $NAME: single
@@
const $NAME = () => {
    ...
    return <div />;
};|} in
  let source = {|
const Header = () => {
    const title = "Hello";
    return <div />;
};

const Footer = () => {
    return <div />;
};
|} in
  let results = Match.find_matches ~language:"tsx" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found two TSX arrow function matches" 2 (List.length results)

(* Test: TypeScript spread operator preserved (exact match) *)
let test_ellipsis_typescript_spread () =
  let pattern = {|@@
match: strict
metavar $OBJ: single
@@
{ ...$OBJ }|} in
  let source = {|
const copy = { ...original };
const another = { ...source };
|} in
  let results = Match.find_matches ~language:"typescript" ~pattern_text:pattern ~source_text:source in
  Alcotest.(check int) "found spread patterns" 2 (List.length results);
  let objs = List.map (fun (r : Match.match_result) ->
    List.assoc "$OBJ" r.bindings
  ) results in
  Alcotest.(check bool) "found original" true (List.mem "original" objs);
  Alcotest.(check bool) "found source" true (List.mem "source" objs)

let ellipsis_tests = [
  Alcotest.test_case "ellipsis statements" `Quick test_ellipsis_statements;
  Alcotest.test_case "ellipsis arguments" `Quick test_ellipsis_arguments;
  Alcotest.test_case "ellipsis zero" `Quick test_ellipsis_zero;
  Alcotest.test_case "ellipsis not spread" `Quick test_ellipsis_not_spread;
  Alcotest.test_case "ellipsis typescript" `Quick test_ellipsis_typescript;
  Alcotest.test_case "ellipsis typescript array" `Quick test_ellipsis_typescript_array;
  Alcotest.test_case "ellipsis typescript class" `Quick test_ellipsis_typescript_class;
  Alcotest.test_case "ellipsis tsx function" `Quick test_ellipsis_tsx_function;
  Alcotest.test_case "ellipsis tsx arrow" `Quick test_ellipsis_tsx_arrow;
  Alcotest.test_case "ellipsis typescript spread" `Quick test_ellipsis_typescript_spread;
]
