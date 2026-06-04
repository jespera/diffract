(** End-to-end matcher tests: full pattern strings (preamble + body) run against
    source text via [Matcher.find]. This is the first layer where a real pattern
    string flows through preamble parse -> tokenize -> match. *)

open Diffract

let ctx = Context.create ()

let find ~language ~pattern ~source =
  Matcher.find ~ctx ~language ~pattern_text:pattern ~source_text:source

(* Most existing tests are single-section. [single] unwraps a composite
   to its only section match (failing the test if the composite has
   anything other than one section). *)
let single (c : Matcher.composite_match) : Matcher.M.match_result =
  match c.sections with
  | [ m ] -> m
  | sections ->
      Alcotest.failf "expected 1-section composite, got %d sections"
        (List.length sections)

(* Walk to the first match of [body]'s tokens and return each token's source
   slice from its recorded span (Stmatch.match_at_spans). Surgical-transforms
   groundwork: exercises per-token span recording through the real cursor. *)
let token_slices ~language ~singles ~body ~source =
  let module M = Matcher.M in
  let module Cur = Tree_sitter_cursor in
  let tokens =
    Tokenize.tokenize ~ctx ~language ~single_metavars:singles
      ~sequence_metavars:[] body
  in
  let tree = Tree.parse ~ctx ~language source in
  let cursor = ref (Cur.of_tree tree) in
  let rec go () =
    match M.match_at_spans tokens (Cur.clone !cursor) with
    | Some (_, _, spans) -> Some spans
    | None ->
        if Cur.move_first_child !cursor || Cur.move_next_subtree !cursor then
          go ()
        else None
  in
  match go () with
  | None -> []
  | Some spans ->
      Array.to_list spans
      |> List.map (fun (s, e) ->
          if s < 0 then "<none>" else String.sub source s (e - s))

let test_match_at_spans () =
  Alcotest.(check (list string))
    "each token's recorded span slices to its source text"
    [ "foo"; "("; "42"; ")" ]
    (token_slices ~language:"typescript" ~singles:[ "$x" ] ~body:"foo($x)"
       ~source:"const a = foo(42);")

(* classify_spatch — the structured body parse match_side/replace_side derive
   from, and the basis surgical transforms build on. *)
let describe_spatch =
  List.map (function
    | Matcher.Ctx s -> ("ctx", s)
    | Matcher.Del s -> ("del", s)
    | Matcher.Add s -> ("add", s))

let test_classify_spatch () =
  Alcotest.(check (list (pair string string)))
    "rename: del then add"
    [ ("del", "foo($x)"); ("add", "bar($x)") ]
    (describe_spatch (Matcher.classify_spatch "- foo($x)\n+ bar($x)"));
  Alcotest.(check (list (pair string string)))
    "context around a removal (one leading space stripped from context)"
    [ ("ctx", " <Foo"); ("del", "  bar={$x}"); ("ctx", " />") ]
    (describe_spatch (Matcher.classify_spatch "  <Foo\n-   bar={$x}\n  />"));
  Alcotest.(check (list (pair string string)))
    "pure-context guard"
    [ ("ctx", "import React") ]
    (describe_spatch (Matcher.classify_spatch "import React"));
  Alcotest.(check (list (pair string string)))
    "removal only"
    [ ("del", "legacy: $v") ]
    (describe_spatch (Matcher.classify_spatch "- legacy: $v"))

(* ========================================================================= *)
(* Preamble parsing                                                          *)
(* ========================================================================= *)

let first_section (p : Matcher.parsed_pattern) : Matcher.section =
  match p.sections with
  | [ s ] -> s
  | _ -> Alcotest.failf "expected a single-section pattern"

let test_parse_preamble_basic () =
  let p =
    Matcher.parse_pattern
      "@@\n\
       match: strict\n\
       metavar $x: single\n\
       metavar $xs: sequence\n\
       @@\n\
       foo($x)"
  in
  let s = first_section p in
  Alcotest.(check bool) "mode strict" true (s.mode = Matcher.Strict);
  Alcotest.(check (list string)) "single" [ "$x" ] s.single_metavars;
  Alcotest.(check (list string)) "sequence" [ "$xs" ] s.sequence_metavars;
  Alcotest.(check string) "body" "foo($x)" s.body;
  Alcotest.(check bool) "global scope" true (s.scope = Matcher.Global)

(* Sigil-free declaration: names without `$` are accepted verbatim. *)
let test_parse_preamble_sigil_free () =
  let p =
    Matcher.parse_pattern "@@\nmatch: strict\nmetavar obj: single\n@@\nfoo(obj)"
  in
  let s = first_section p in
  Alcotest.(check (list string)) "single (no sigil)" [ "obj" ] s.single_metavars

let test_parse_preamble_missing_mode () =
  Alcotest.check_raises "missing mode raises"
    (Failure
       "Section 1 must specify a match mode (match: strict | partial | field)")
    (fun () ->
      ignore (Matcher.parse_pattern "@@\nmetavar $x: single\n@@\nfoo($x)"))

(* ========================================================================= *)
(* End-to-end strict matching                                                *)
(* ========================================================================= *)

let test_find_calls () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)" in
  let results =
    find ~language:"typescript" ~pattern ~source:"foo(1); bar(2); foo(3);"
  in
  Alcotest.(check int) "two foo(_) matches" 2 (List.length results)

(* Sigil and sigil-free patterns produce identical match counts. *)
let test_sigil_free_equivalent () =
  let src = "foo(42); foo(43);" in
  let sigil = "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)" in
  let bare = "@@\nmatch: strict\nmetavar obj: single\n@@\nfoo(obj)" in
  let n1 =
    List.length (find ~language:"typescript" ~pattern:sigil ~source:src)
  in
  let n2 =
    List.length (find ~language:"typescript" ~pattern:bare ~source:src)
  in
  Alcotest.(check int) "sigil count" 2 n1;
  Alcotest.(check int) "sigil-free count matches" n1 n2

(* Ellipsis pattern end-to-end. *)
let test_find_with_ellipsis () =
  let pattern = "@@\nmatch: strict\n@@\nfoo(...)" in
  let results =
    find ~language:"typescript" ~pattern ~source:"foo(1, 2, 3); bar();"
  in
  Alcotest.(check int) "one foo(...) match" 1 (List.length results)

(* Binding extraction end-to-end: the bound subtree's source text. *)
let test_binding_extraction () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)" in
  let src = "foo(c > 0);" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] -> (
      let r = single c in
      match
        List.find_map
          (function
            | Matcher.M.Single { name = "$x"; cursor } -> Some cursor
            | _ -> None)
          r.bindings
      with
      | None -> Alcotest.fail "no binding for $x"
      | Some c ->
          let s, e = Tree_sitter_cursor.byte_range c in
          Alcotest.(check string)
            "$x bound to 'c > 0'" "c > 0"
            (String.sub src s (e - s)))
  | _ -> Alcotest.fail "expected exactly one match"

(* A multi-token pattern matched across a real source. *)
let test_member_call_pattern () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $obj: single\n\
     metavar $arg: single\n\
     @@\n\
     $obj.send($arg)"
  in
  let src = "client.send(payload); other.recv(x); server.send(data);" in
  let results = find ~language:"typescript" ~pattern ~source:src in
  Alcotest.(check int) "two .send() calls" 2 (List.length results)

(* Cross-language: a Kotlin pattern. This also demonstrates why the
   sigil-free design matters — in Kotlin, a `$`-prefixed name like `$v`
   does not tokenize to a single `$v` leaf (the `$` is string-template
   syntax and is dropped under error recovery), so a declared name `$v`
   would never match. A plain identifier name works across all languages. *)
let test_kotlin_pattern () =
  let pattern = "@@\nmatch: strict\nmetavar V: single\n@@\nval x = V" in
  let src = "fun main() { val x = 1; val y = 2 }" in
  let results = find ~language:"kotlin" ~pattern ~source:src in
  Alcotest.(check int) "one `val x = _` match" 1 (List.length results)

(* ========================================================================= *)
(* Match-side extraction (spatch -/+/context) and fragment patterns          *)
(* ========================================================================= *)

(* The match side of a transform body: "- " content kept, "+ " dropped,
   context lines space-stripped. *)
let test_match_side_extraction () =
  let body = "- foo($x)\n+ bar($x)" in
  Alcotest.(check string)
    "minus kept, plus dropped" "foo($x)" (Matcher.match_side body)

let test_match_side_context () =
  let body = " function f() {\n-   old()\n+   new()\n }" in
  Alcotest.(check string)
    "context + minus, plus dropped" "function f() {\n  old()\n}"
    (Matcher.match_side body)

(* The replace side mirrors match_side: "+ " kept, "- " dropped. *)
let test_replace_side_extraction () =
  let body = "- foo($x)\n+ bar($x)" in
  Alcotest.(check (option string))
    "plus kept, minus dropped" (Some "bar($x)")
    (Matcher.replace_side body)

(* A match-only body (no - / + lines) has no replace side. *)
let test_replace_side_match_only () =
  Alcotest.(check (option string))
    "pure context => None" None
    (Matcher.replace_side "foo($x)")

(* A body with - lines and no + is a removal: the replace side is the body
   with the - lines dropped (here, empty). *)
let test_replace_side_removal () =
  Alcotest.(check (option string))
    "- only => Some empty (removal)" (Some "")
    (Matcher.replace_side "- foo($x)");
  (* - line within context: context kept, - dropped. *)
  Alcotest.(check (option string))
    "removal within context" (Some "function f() {\n}")
    (Matcher.replace_side " function f() {\n-   old();\n }")

(* ========================================================================= *)
(* End-to-end transforms (the + side)                                        *)
(* ========================================================================= *)

let transform ~language ~pattern ~source =
  Matcher.transform ~ctx ~language ~pattern_text:pattern ~source_text:source

(* Simple rename: every foo($x) becomes bar($x), binding substituted. *)
let test_transform_rename () =
  let pattern =
    "@@\nmatch: strict\nmetavar $x: single\n@@\n- foo($x)\n+ bar($x)"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"foo(1); foo(2);"
  in
  Alcotest.(check string) "both calls renamed" "bar(1); bar(2);" out

(* A metavar name must only be substituted as a whole token, never as a
   substring of a literal identifier in the replacement. Critical for
   sigil-free single-letter metavars (Kotlin/PHP): `a` and `b` must not be
   replaced inside the literal `fallback`. Regression for instantiate_template. *)
let test_transform_metavar_not_substring () =
  Alcotest.(check string)
    "a/b not substituted inside `fallback`"
    "fun g() { val x = maybe ?: fallback(dflt) }"
    (transform ~language:"kotlin"
       ~pattern:
         "@@\n\
          match: strict\n\
          metavar a: single\n\
          metavar b: single\n\
          @@\n\
          - a ?: b\n\
          + a ?: fallback(b)"
       ~source:"fun g() { val x = maybe ?: dflt }")

(* ...but a sigil-free metavar in a Kotlin string template ("$x") still
   substitutes — the `$` is a boundary, not an identifier char. *)
let test_transform_metavar_in_string_template () =
  Alcotest.(check string)
    "x substitutes after $ in template" "fun g() { logger.info(\"v=$count\") }"
    (transform ~language:"kotlin"
       ~pattern:
         "@@\n\
          match: strict\n\
          metavar x: single\n\
          @@\n\
          - log(\"v=$x\")\n\
          + logger.info(\"v=$x\")"
       ~source:"fun g() { log(\"v=$count\") }")

(* A complete-statement-shaped pattern must match a call even when the
   pattern omits the terminator: PHP parses `mysql_query($x)` as an
   expression_statement with a zero-width inserted `;`, and the tokenizer
   must drop that phantom node rather than emit an unmatchable `Concrete ""`.
   Regression for the missing-node tokenizer fix. *)
let test_transform_php_statement_shaped_call () =
  Alcotest.(check string)
    "both PHP calls rewritten, missing `;` not leaked"
    "mysqli_query($sql); mysqli_query($other); keep($z);"
    (transform ~language:"php"
       ~pattern:
         "@@\n\
          match: strict\n\
          metavar x: single\n\
          @@\n\
          - mysql_query(x)\n\
          + mysqli_query(x)"
       ~source:"mysql_query($sql); mysql_query($other); keep($z);")

(* A trailing newline in the pattern body (every pattern *file* ends with
   one) must not leak into the replacement: the empty final line is a
   delimiter artifact, not a context line. Regression for the CLI apply path. *)
let test_transform_trailing_newline_in_pattern () =
  let pattern =
    "@@\nmatch: strict\nmetavar $x: single\n@@\n- foo($x)\n+ bar($x)\n"
  in
  Alcotest.(check string)
    "no stray newline in output" "const a = bar(1);"
    (transform ~language:"typescript" ~pattern ~source:"const a = foo(1);")

(* Two bindings, reordered in the output. *)
let test_transform_swap_operands () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $a: single\n\
     metavar $b: single\n\
     @@\n\
     - $a + $b\n\
     + $b + $a"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"const z = foo + bar;"
  in
  Alcotest.(check string) "operands swapped" "const z = bar + foo;" out

(* A match-only pattern leaves the source untouched. *)
let test_transform_match_only_noop () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)" in
  let src = "foo(1); foo(2);" in
  Alcotest.(check string)
    "no - / + lines => unchanged" src
    (transform ~language:"typescript" ~pattern ~source:src)

(* Removal: a `-` line with no `+` removes the matched span (spatch
   convention). Here the whole matched statement is deleted. *)
let test_transform_removal_statement () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\n- debug($x);" in
  let out =
    transform ~language:"typescript" ~pattern ~source:"debug(1); keep(2);"
  in
  Alcotest.(check string) "debug statement removed" " keep(2);" out

(* Removal within context: context lines are kept, only the `-` line goes. *)
let test_transform_removal_in_context () =
  let pattern = "@@\nmatch: strict\n@@\n function f() {\n-   old();\n }" in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"function f() {\n  old();\n}"
  in
  Alcotest.(check string) "old() removed, function kept" "function f() {\n}" out

(* Surgical removal: a `-` on a sub-part of an `...`-bracketed match removes
   only that part; the siblings captured by `...` are preserved verbatim (not
   dropped, and `...` is not emitted literally). This is the headline
   surgical-transform fix. *)
let test_transform_surgical_ellipsis_removal () =
  let pattern =
    "@@\nmatch: strict\n@@\n function f() {\n   ...\n-   old();\n   ...\n }"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"function f() {\n  a();\n  old();\n  b();\n}"
  in
  Alcotest.(check string)
    "only old() removed, a()/b() kept" "function f() {\n  a();\n  b();\n}" out

(* Surgical replacement: a `-`/`+` pair on a sub-part replaces only that part,
   leaving the `...`-captured siblings untouched. *)
let test_transform_surgical_ellipsis_replace () =
  (* The `-` span is `old();` (no indentation); the `+` content is the bare
     replacement, so the source's existing indentation is preserved in place. *)
  let pattern =
    "@@\n\
     match: strict\n\
     @@\n\
    \ function f() {\n\
    \   ...\n\
     -   old();\n\
     + renamed();\n\
    \   ...\n\
    \ }"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"function f() {\n  a();\n  old();\n  b();\n}"
  in
  Alcotest.(check string)
    "old() replaced, a()/b() kept"
    "function f() {\n  a();\n  renamed();\n  b();\n}" out

(* The original headline case: removing one JSX attribute from an element with
   others. The strict `...` idiom captures the surrounding attributes as
   context; surgical editing removes only the marked one and keeps the rest. *)
let test_transform_surgical_jsx_attribute () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
    \ <Foo\n\
    \   ...\n\
     -   bar={$x}\n\
    \   ...\n\
    \ />"
  in
  let out =
    transform ~language:"tsx" ~pattern
      ~source:"const e = (\n  <Foo\n    bar={x}\n    baz={y}\n  />\n);"
  in
  Alcotest.(check string)
    "bar attribute removed, baz kept"
    "const e = (\n  <Foo\n    baz={y}\n  />\n);" out

(* Partial-mode surgical removal: marking one object property for removal
   deletes only it (with its separator) and preserves the tolerated extras —
   no longer the whole-span drop. *)
let test_transform_surgical_partial_property () =
  let pattern =
    "@@\nmatch: partial\nmetavar $v: single\n@@\n {\n-   color: $v,\n }"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"const s = { color: red, size: 10 };"
  in
  Alcotest.(check string)
    "color removed with its comma, size kept" "const s = { size: 10 };" out

(* Partial-mode surgical removal of a JSX attribute keeps the sibling
   attribute (the tolerated extra), closing the whitespace gap. *)
let test_transform_surgical_partial_jsx () =
  let pattern =
    "@@\nmatch: partial\nmetavar $x: single\n@@\n <Foo\n-   bar={$x}\n />"
  in
  let out =
    transform ~language:"tsx" ~pattern
      ~source:"const e = <Foo bar={x} baz={y} />;"
  in
  Alcotest.(check string)
    "bar removed, baz kept" "const e = <Foo baz={y} />;" out

(* Partial-mode surgical in-place edit: rewrite one property keeping its
   siblings and the container separators. The `+` is the bare element (no
   structural indentation, no trailing separator — the container manages
   those), so it splices in cleanly. *)
let test_transform_surgical_partial_inplace () =
  let pattern =
    "@@\n\
     match: partial\n\
     metavar $v: single\n\
     @@\n\
    \ {\n\
     -   color: $v,\n\
     + colour: $v\n\
    \ }"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"const s = { color: red, size: 10 };"
  in
  Alcotest.(check string)
    "color renamed in place, size + commas intact"
    "const s = { colour: red, size: 10 };" out

(* Field-mode surgical rewrite preserves the optional fields the pattern omits
   for matching (here a Kotlin annotation + visibility modifier), instead of
   dropping them as the whole-span replace did. *)
let test_transform_surgical_field_modifiers () =
  let pattern =
    "@@\n\
     match: field\n\
     @@\n\
     - fun foo(): Int { return 1 }\n\
     + fun bar(): Int { return 2 }"
  in
  let out =
    transform ~language:"kotlin" ~pattern
      ~source:"@Deprecated\nprivate fun foo(): Int { return 1 }"
  in
  Alcotest.(check string)
    "annotation + modifier kept, body rewritten"
    "@Deprecated\nprivate fun bar(): Int { return 2 }" out

(* Whole-container partial rewrite: marking the entire container (no context
   line) replaces it whole, including its delimiters, and drops the tolerated
   extras (the honest reading of marking the whole thing). The delimiters carry
   no recorded span, so the deletion must extend to the match boundary rather
   than leaving the brackets behind as malformed nested output. *)
let test_transform_whole_container_partial () =
  let pattern =
    "@@\n\
     match: partial\n\
     metavar $V: single\n\
     @@\n\
     - { color: $V }\n\
     + { colour: $V }"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"const s = { color: red, size: 10 };"
  in
  Alcotest.(check string)
    "whole container replaced, not nested" "const s = { colour: red };" out

(* A bare ellipsis on a `-`/`+` line is rejected: it is a match-only construct.
   An ellipsis nested inside a marked expression (replaced wholesale) is fine. *)
let test_reject_bare_ellipsis_on_edit_line () =
  let raises pattern =
    try
      ignore
        (transform ~language:"typescript" ~pattern
           ~source:"function f(){ a(); old(); }");
      false
    with Failure _ -> true
  in
  Alcotest.(check bool)
    "`- ...` rejected" true
    (raises "@@\nmatch: strict\n@@\n- ...\n- old();");
  Alcotest.(check bool)
    "`+ ...` rejected" true
    (raises "@@\nmatch: strict\n@@\n- old();\n+ ...");
  (* nested ellipsis in a replaced expression is allowed (no exception) *)
  Alcotest.(check bool)
    "nested `bar(...)` on a `-` line is allowed" false
    (raises "@@\nmatch: strict\n@@\n- old(...)\n+ new()")

(* Adjacent matches are each rewritten once (non-overlapping). *)
let test_transform_adjacent_matches () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\n- wrap($x)\n+ $x" in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"wrap(a); wrap(b); wrap(c);"
  in
  Alcotest.(check string) "each unwrapped once" "a; b; c;" out

(* Placeholder-boundary safety: $x must not corrupt $xy. *)
let test_transform_placeholder_boundary () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     metavar $xy: single\n\
     @@\n\
     - pair($x, $xy)\n\
     + [$xy, $x]"
  in
  let out = transform ~language:"typescript" ~pattern ~source:"pair(1, 2);" in
  Alcotest.(check string) "$xy and $x substituted independently" "[2, 1];" out

(* A metavar bound to a compound expression substitutes the whole span. *)
let test_transform_compound_binding () =
  let pattern =
    "@@\nmatch: strict\nmetavar $x: single\n@@\n- foo($x)\n+ bar($x)"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"foo(a + b * c);"
  in
  Alcotest.(check string) "compound arg preserved" "bar(a + b * c);" out

(* ========================================================================= *)
(* Multi-section transforms + on-scoping                                     *)
(* ========================================================================= *)

(* Two independent sections, both with transforms; their matched spans are
   disjoint, so both edits apply. *)
let test_transform_two_independent_sections () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     - foo($x)\n\
     + foo2($x)\n\
     @@\n\
     match: strict\n\
     metavar $y: single\n\
     @@\n\
     - bar($y)\n\
     + bar2($y)"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"foo(1); bar(2);"
  in
  Alcotest.(check string) "both sections applied" "foo2(1); bar2(2);" out

(* Guard section (no +) + an on-scoped transform: the inner section
   rewrites inside the bound subtree; the surrounding source is preserved. *)
let test_transform_on_scoped () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $obj: single\n\
     @@\n\
     foo($obj)\n\
     @@\n\
     match: strict\n\
     on $obj\n\
     metavar $m: single\n\
     @@\n\
     - $m.run()\n\
     + $m.execute()"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"foo(client.run());"
  in
  Alcotest.(check string)
    "rewrite scoped inside $obj" "foo(client.execute());" out

(* Cross-section shared metavar: both sections require the same $x, and both
   transform. *)
let test_transform_shared_metavar () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     - check($x)\n\
     + checked($x)\n\
     @@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     - guard($x)\n\
     + guarded($x)"
  in
  (* Same $x in both calls: composite fires, both rewritten. *)
  Alcotest.(check string)
    "shared $x => both rewritten" "checked(a); guarded(a);"
    (transform ~language:"typescript" ~pattern ~source:"check(a); guard(a);");
  (* Different arguments: non-linearity blocks the composite, nothing fires. *)
  Alcotest.(check string)
    "inconsistent $x => unchanged" "check(a); guard(b);"
    (transform ~language:"typescript" ~pattern ~source:"check(a); guard(b);")

(* Nested edits (an outer rewrite whose span contains an inner rewrite) are
   rejected rather than silently corrupting — that composition is a later
   feature. *)
let test_transform_overlap_rejected () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     - outer($x)\n\
     + OUTER($x)\n\
     @@\n\
     match: strict\n\
     metavar $y: single\n\
     @@\n\
     - inner($y)\n\
     + INNER($y)"
  in
  let raised =
    try
      ignore
        (transform ~language:"typescript" ~pattern ~source:"outer(inner(1));");
      false
    with Failure _ -> true
  in
  Alcotest.(check bool)
    "nested transforms raise rather than corrupt" true raised

(* ========================================================================= *)
(* foreach: in-place per-element transforms (map)                            *)
(* ========================================================================= *)

(* on over a sequence metavar is a parse error — must use foreach. *)
let test_on_over_sequence_rejected () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $xs: sequence\n\
     @@\n\
     foo($xs)\n\
     @@\n\
     match: strict\n\
     on $xs\n\
     @@\n\
     bar()"
  in
  let raised =
    try
      ignore (find ~language:"typescript" ~pattern ~source:"foo(1);");
      false
    with Failure _ -> true
  in
  Alcotest.(check bool) "on over sequence rejected" true raised

(* foreach over a single metavar is a parse error — must use on. *)
let test_foreach_over_single_rejected () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     foo($x)\n\
     @@\n\
     match: strict\n\
     foreach $x\n\
     @@\n\
     bar()"
  in
  let raised =
    try
      ignore (find ~language:"typescript" ~pattern ~source:"foo(1);");
      false
    with Failure _ -> true
  in
  Alcotest.(check bool) "foreach over single rejected" true raised

(* In-place map: rename every property of the object passed to foo(),
   leaving the container and separators untouched. *)
let test_foreach_inplace_rename () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $KEY: single\n\
     metavar $VAL: single\n\
     @@\n\
     - $KEY: $VAL\n\
     + $KEY: wrap($VAL)"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"foo({a: 1, b: 2});"
  in
  Alcotest.(check string)
    "each property wrapped, commas/braces intact"
    "foo({a: wrap(1), b: wrap(2)});" out

(* The scope matters: properties of an object NOT passed to foo() are
   untouched. *)
let test_foreach_scoped_to_container () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $KEY: single\n\
     metavar $VAL: single\n\
     @@\n\
     - $KEY: $VAL\n\
     + $KEY: wrap($VAL)"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"foo({a: 1}); bar({c: 3});"
  in
  Alcotest.(check string)
    "only foo's object rewritten" "foo({a: wrap(1)}); bar({c: 3});" out

(* JSX: per-element edit over a self-closing element's attributes, to
   confirm foreach isn't object-specific. *)
let test_foreach_jsx_attributes () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $ATTRS: sequence\n\
     @@\n\
     <Foo $ATTRS />\n\
     @@\n\
     match: strict\n\
     foreach $ATTRS\n\
     metavar $N: single\n\
     metavar $V: single\n\
     @@\n\
     - $N={$V}\n\
     + $N={wrap($V)}"
  in
  let out =
    transform ~language:"tsx" ~pattern ~source:"const e = <Foo a={1} b={2} />;"
  in
  Alcotest.(check string)
    "each attribute value wrapped" "const e = <Foo a={wrap(1)} b={wrap(2)} />;"
    out

(* ========================================================================= *)
(* foreach with a CONCRETE key (contextual tokenization)                     *)
(*                                                                           *)
(* A concrete object key like `color` tokenized standalone parses as a       *)
(* labeled-statement label (statement_identifier), not the source pair's     *)
(* property_identifier, so it would miss. compile_foreach re-tokenizes the   *)
(* element pattern inside its container scaffold (drawn from the outer       *)
(* section) so the key gets the right node-type. Sibling elements are        *)
(* preserved (in-place per-element edit).                                    *)
(* ========================================================================= *)

(* TS: rename only the `color` property, preserving `size`. *)
let test_foreach_concrete_key_object () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $V: single\n\
     @@\n\
     - color: $V\n\
     + colour: $V"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:{|foo({ color: "red", size: 10 });|}
  in
  Alcotest.(check string)
    "only color renamed, size preserved" {|foo({ colour: "red", size: 10 });|}
    out

(* TSX: rename only the `a` attribute, preserving `b`. *)
let test_foreach_concrete_key_jsx () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $ATTRS: sequence\n\
     @@\n\
     <Foo $ATTRS />\n\
     @@\n\
     match: strict\n\
     foreach $ATTRS\n\
     metavar $X: single\n\
     @@\n\
     - a={$X}\n\
     + aa={$X}"
  in
  let out =
    transform ~language:"tsx" ~pattern ~source:"const e = <Foo a={1} b={2} />;"
  in
  Alcotest.(check string)
    "only attribute a renamed, b preserved" "const e = <Foo aa={1} b={2} />;"
    out

(* A property the source doesn't have: nothing matches, source unchanged. *)
let test_foreach_concrete_key_absent () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $V: single\n\
     @@\n\
     - missing: $V\n\
     + found: $V"
  in
  let src = {|foo({ color: "red" });|} in
  Alcotest.(check string)
    "no matching key -> unchanged" src
    (transform ~language:"typescript" ~pattern ~source:src)

(* Cross-language transparency guard: kotlin keys aren't context-sensitive
   (one identifier node-type), so the contextual path must leave them
   working — select one named argument by name and rename it. *)
let test_foreach_concrete_key_kotlin () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar ARGS: sequence\n\
     @@\n\
     f(ARGS)\n\
     @@\n\
     match: strict\n\
     foreach ARGS\n\
     metavar V: single\n\
     @@\n\
     - name = V\n\
     + label = V"
  in
  let out =
    transform ~language:"kotlin" ~pattern ~source:"f(name = x, other = y)"
  in
  Alcotest.(check string)
    "named arg renamed, others preserved" "f(label = x, other = y)" out

(* Fallback guard: an ellipsis in the element pattern disables contextual
   tokenization (offsets would shift); it must degrade gracefully to
   standalone tokenization rather than crash. A wildcard key still works. *)
let test_foreach_ellipsis_falls_back () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $K: single\n\
     @@\n\
     - $K: bar(...)\n\
     + $K: baz()"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"foo({ a: bar(1, 2), b: 3 });"
  in
  Alcotest.(check string)
    "ellipsis element falls back, still rewrites a" "foo({ a: baz(), b: 3 });"
    out

(* ========================================================================= *)
(* foreach element removal with separator cleanup                            *)
(*                                                                           *)
(* A foreach element with an empty replacement (a `-` line, no `+`) is a     *)
(* removal. The edit swallows an adjacent separator so the list stays        *)
(* well-formed: the separator *following* the element, or the *preceding*    *)
(* one if there's none after (last element, no trailing comma). A trailing   *)
(* comma counts as a following separator. Residual cosmetic whitespace is    *)
(* left to a formatter.                                                      *)
(* ========================================================================= *)

let rm_prop key =
  Printf.sprintf
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $V: single\n\
     @@\n\
     - %s: $V"
    key

let test_remove_middle_element () =
  Alcotest.(check string)
    "middle removed, neighbours intact" "foo({ a: 1, c: 3 });"
    (transform ~language:"typescript" ~pattern:(rm_prop "b")
       ~source:"foo({ a: 1, b: 2, c: 3 });")

let test_remove_first_element () =
  Alcotest.(check string)
    "first removed (trailing sep consumed)" "foo({ b: 2, c: 3 });"
    (transform ~language:"typescript" ~pattern:(rm_prop "a")
       ~source:"foo({ a: 1, b: 2, c: 3 });")

let test_remove_last_element () =
  Alcotest.(check string)
    "last removed (leading sep consumed)" "foo({ a: 1, b: 2 });"
    (transform ~language:"typescript" ~pattern:(rm_prop "c")
       ~source:"foo({ a: 1, b: 2, c: 3 });")

let test_remove_only_element () =
  (* No separator to consume; the element goes, leaving an (empty, valid)
     container. The double space is cosmetic — formatter territory. *)
  Alcotest.(check string)
    "only element removed -> empty container" "foo({  });"
    (transform ~language:"typescript" ~pattern:(rm_prop "a")
       ~source:"foo({ a: 1 });")

let rm_arg name =
  Printf.sprintf
    "@@\n\
     match: strict\n\
     metavar ARGS: sequence\n\
     @@\n\
     f(ARGS)\n\
     @@\n\
     match: strict\n\
     foreach ARGS\n\
     metavar V: single\n\
     @@\n\
     - %s = V"
    name

(* Trailing-comma list, remove the only element: the trailing comma is a
   following separator, so it's consumed too — no dangling `f(,)`. *)
let test_remove_kotlin_trailing_only () =
  Alcotest.(check string)
    "trailing comma consumed with only element" "f()"
    (transform ~language:"kotlin" ~pattern:(rm_arg "a") ~source:"f(a = 1,)")

(* Trailing-comma list, remove the last element: the trailing comma is
   consumed, the between-comma survives as the new trailing comma — the
   Kotlin idiom is preserved (the space before `)` is cosmetic). *)
let test_remove_kotlin_trailing_last () =
  Alcotest.(check string)
    "trailing-comma idiom preserved" "f(a = 1, )"
    (transform ~language:"kotlin" ~pattern:(rm_arg "b")
       ~source:"f(a = 1, b = 2,)")

(* ========================================================================= *)
(* Splice: foreach rendered + spliced into a restructured template           *)
(* ========================================================================= *)

(* The headline case: matchExhaustive(...) collapses to a method chain. The
   foreach renders each property to a .with(...) fragment; because the outer
   + references $PROPS, the fragments are concatenated and spliced there,
   replacing the whole call. *)
let test_splice_method_chain () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $TAG: single\n\
     metavar $PROPS: sequence\n\
     @@\n\
     - matchExhaustive($TAG, {\n\
     -   $PROPS\n\
     - });\n\
     + match($TAG)$PROPS.exhaustive();\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $KEY: single\n\
     metavar $VAL: single\n\
     @@\n\
     - $KEY: $VAL\n\
     + .with(\"$KEY\", $VAL)"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"matchExhaustive(tag, {one: \"1\", two: \"2\"});"
  in
  Alcotest.(check string)
    "object collapsed to method chain"
    "match(tag).with(\"one\", \"1\").with(\"two\", \"2\").exhaustive();" out

(* A single-property object splices to a one-link chain. *)
let test_splice_single_element () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $TAG: single\n\
     metavar $PROPS: sequence\n\
     @@\n\
     - matchExhaustive($TAG, { $PROPS });\n\
     + match($TAG)$PROPS.exhaustive();\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $KEY: single\n\
     metavar $VAL: single\n\
     @@\n\
     - $KEY: $VAL\n\
     + .with(\"$KEY\", $VAL)"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"matchExhaustive(tag, { only: 9 });"
  in
  Alcotest.(check string)
    "single link chain" "match(tag).with(\"only\", 9).exhaustive();" out

(* When the sequence is spliced, the foreach does NOT also edit in place —
   the whole construct is replaced once (no overlapping edits). *)
let test_splice_no_inplace_double_apply () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     - wrap({ $PROPS });\n\
     + chain()$PROPS;\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $KEY: single\n\
     metavar $VAL: single\n\
     @@\n\
     - $KEY: $VAL\n\
     + .set($VAL)"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"wrap({a: 1, b: 2});"
  in
  Alcotest.(check string)
    "spliced once, not also in place" "chain().set(1).set(2);" out

(* ========================================================================= *)
(* join directive + identity splice                                          *)
(* ========================================================================= *)

(* Identity splice with a join separator: an array becomes a conjunction.
   No foreach — the elements render as their source text, joined by " && ".
   The commas in the array binding are dropped via is_named. *)
let test_join_identity_conjunction () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $ELEMS: sequence\n\
     join $ELEMS by \" && \"\n\
     @@\n\
     - [$ELEMS]\n\
     + $ELEMS"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"const r = [a, b, c];"
  in
  Alcotest.(check string) "array to conjunction" "const r = a && b && c;" out

(* Precedence escape hatch: a foreach wraps each element in parens before
   the && join, so an operand like (x || y) is isolated. *)
let test_join_precedence_parens () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $ELEMS: sequence\n\
     join $ELEMS by \" && \"\n\
     @@\n\
     - [$ELEMS]\n\
     + $ELEMS\n\
     @@\n\
     match: strict\n\
     foreach $ELEMS\n\
     metavar $E: single\n\
     @@\n\
     - $E\n\
     + ($E)"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"const r = [x || y, z];"
  in
  Alcotest.(check string)
    "operands parenthesized" "const r = (x || y) && (z);" out

(* foreach-splice with a non-empty (comma) join: object properties become a
   new argument list. Each property renders to its value; join is ", ". *)
let test_join_foreach_comma () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     join $PROPS by \", \"\n\
     @@\n\
     - call({ $PROPS })\n\
     + call2($PROPS)\n\
     @@\n\
     match: strict\n\
     foreach $PROPS\n\
     metavar $K: single\n\
     metavar $V: single\n\
     @@\n\
     - $K: $V\n\
     + $V"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"call({a: 1, b: 2});"
  in
  Alcotest.(check string) "object values to arg list" "call2(1, 2);" out

(* ========================================================================= *)
(* Expansion ports: the old column-0 expansion lines re-authored in the new  *)
(* foreach/join surface, validating it covers the same capability. The old   *)
(* `~ $V` (newline) becomes `join $V by "\n"`; `, $V` becomes                 *)
(* `join $V by ", "`; a following `on $V` transform section becomes a        *)
(* `foreach $V` section. (Old syntax in test_match.ml's Expansion group.)    *)
(* ========================================================================= *)

(* Old: `~ $STMTS` newline expansion of a function body into an array. *)
let test_port_verbatim_newline () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $NAME: single\n\
     metavar $STMTS: sequence\n\
     join $STMTS by \"\\n\"\n\
     @@\n\
     - function $NAME() {\n\
     -   $STMTS\n\
     - }\n\
     + const $NAME = [\n\
     + $STMTS\n\
     + ];"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"function setup() {\n  a();\n  b();\n  c();\n}"
  in
  Alcotest.(check string)
    "statements newline-joined into array"
    "const setup = [\na();\nb();\nc();\n];" out

(* Old: `, $STMTS` comma expansion. *)
let test_port_verbatim_comma () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $NAME: single\n\
     metavar $STMTS: sequence\n\
     join $STMTS by \", \"\n\
     @@\n\
     - function $NAME() {\n\
     -   $STMTS\n\
     - }\n\
     + result($NAME, [$STMTS]);"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"function greet() {\n  hello();\n  world();\n}"
  in
  Alcotest.(check string)
    "statements comma-joined" "result(greet, [hello();, world();]);" out

(* Old: empty-sequence expansion produces an empty rendering. *)
let test_port_empty_sequence () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $ITEMS: sequence\n\
     join $ITEMS by \", \"\n\
     @@\n\
     - wrap($ITEMS)\n\
     + [$ITEMS]"
  in
  let out = transform ~language:"typescript" ~pattern ~source:"wrap()" in
  Alcotest.(check string) "empty sequence -> empty brackets" "[]" out

(* Old kotlin: `~ ITEMS` newline expansion, listOf -> buildList. Sigil-free
   metavars are required in kotlin ($ is string-template syntax). *)
let test_port_kotlin_verbatim () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar ITEMS: sequence\n\
     join ITEMS by \"\\n\"\n\
     @@\n\
     - listOf(ITEMS)\n\
     + buildList {\n\
     + ITEMS\n\
     + }"
  in
  let out =
    transform ~language:"kotlin" ~pattern ~source:"listOf(alpha, beta, gamma)"
  in
  Alcotest.(check string)
    "kotlin newline expansion" "buildList {\nalpha\nbeta\ngamma\n}" out

(* Old kotlin: transform expansion — execute(steps) to a Pipeline builder,
   each step wrapped via a foreach. *)
let test_port_kotlin_transform () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar STEPS: sequence\n\
     @@\n\
     - execute(STEPS)\n\
     + Pipeline()STEPS.run()\n\
     @@\n\
     match: strict\n\
     foreach STEPS\n\
     metavar STEP: single\n\
     @@\n\
     - STEP\n\
     + .addStep(STEP)"
  in
  let out =
    transform ~language:"kotlin" ~pattern ~source:"execute(step1, step2, step3)"
  in
  Alcotest.(check string)
    "kotlin pipeline builder"
    "Pipeline().addStep(step1).addStep(step2).addStep(step3).run()" out

(* Old kotlin: `, ITEMS` comma join. *)
let test_port_kotlin_comma () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar ITEMS: sequence\n\
     join ITEMS by \", \"\n\
     @@\n\
     - combine(ITEMS)\n\
     + joined(ITEMS)"
  in
  let out = transform ~language:"kotlin" ~pattern ~source:"combine(a, b, c)" in
  Alcotest.(check string) "kotlin comma join" "joined(a, b, c)" out

(* A fragment pattern (an incomplete construct — a bare binary expression)
   matches a sub-expression inside larger source. This is the universal
   tokenizer's headline strength: the pattern need not be a complete
   top-level construct. *)
let test_fragment_pattern_pure () =
  let pattern =
    "@@\nmatch: strict\nmetavar $a: single\nmetavar $b: single\n@@\n$a + $b"
  in
  let src = "const z = foo + bar; const w = 1 * 2;" in
  let results = find ~language:"typescript" ~pattern ~source:src in
  Alcotest.(check int)
    "matches the `+` sub-expression only" 1 (List.length results)

(* The same fragment, written as a transform-style rule file (the match side
   is extracted from the `- ` line). Exercises match-side extraction and
   fragment matching together. *)
let test_fragment_pattern_in_rule () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $a: single\n\
     metavar $b: single\n\
     @@\n\
     - $a + $b\n\
     + add($a, $b)"
  in
  let src = "const z = foo + bar;" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] ->
      let r = single c in
      let texts =
        List.filter_map
          (function
            | Matcher.M.Single { name; cursor } ->
                let s, e = Tree_sitter_cursor.byte_range cursor in
                Some (name, String.sub src s (e - s))
            | _ -> None)
          r.bindings
      in
      Alcotest.(check (option string))
        "$a bound to foo" (Some "foo")
        (List.assoc_opt "$a" texts);
      Alcotest.(check (option string))
        "$b bound to bar" (Some "bar")
        (List.assoc_opt "$b" texts)
  | _ -> Alcotest.fail "expected exactly one match"

(* find_in_tree matches against a pre-parsed source tree (the entry the CLI
   uses so it can also read Tree.error_count from the same parse). *)
let test_find_in_tree () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)" in
  let tree = Tree.parse ~ctx ~language:"typescript" "foo(1); foo(2);" in
  let results =
    Matcher.find_in_tree ~ctx ~language:"typescript" ~pattern_text:pattern tree
  in
  Alcotest.(check int)
    "two matches against pre-parsed tree" 2 (List.length results)

(* Loading a rule from a file. *)
let test_find_file () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)" in
  let path = Filename.temp_file "diffract_rule" ".pat" in
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc pattern);
  let results =
    Matcher.find_file ~ctx ~language:"typescript" ~pattern_file:path
      ~source_text:"foo(1); foo(2);"
  in
  Sys.remove path;
  Alcotest.(check int) "two matches from file rule" 2 (List.length results)

(* Search finds nested matches: a call nested inside another call's
   arguments is reported in addition to the outer one. *)
let test_nested_calls_found () =
  let pattern = "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)" in
  let src = "foo(foo(1)); foo(2);" in
  let results = find ~language:"typescript" ~pattern ~source:src in
  (* foo(foo(1)) [outer], foo(1) [nested], foo(2) = 3 matches. *)
  Alcotest.(check int) "outer + nested + sibling = 3" 3 (List.length results)

(* ========================================================================= *)
(* End-to-end partial matching                                               *)
(* ========================================================================= *)

(* Helper: pull the source text bound to a single metavar from a result. *)
let bound_text src (r : Matcher.M.match_result) name =
  match
    List.find_map
      (function
        | Matcher.M.Single { name = n; cursor } when n = name -> Some cursor
        | _ -> None)
      r.bindings
  with
  | None -> None
  | Some c ->
      let s, e = Tree_sitter_cursor.byte_range c in
      Some (String.sub src s (e - s))

(* Object literal: pattern claims one key; source has it plus an extra. *)
let test_partial_object_basic () =
  let pattern = "@@\nmatch: partial\nmetavar $x: single\n@@\n{a: $x}" in
  let src = "const o = {a: 1, b: 2};" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] ->
      let r = single c in
      Alcotest.(check (option string))
        "$x = 1" (Some "1") (bound_text src r "$x")
  | results ->
      Alcotest.failf "expected one partial match, got %d" (List.length results)

(* A wildcard KEY in a partial element ({ $K: $V }) must match — the leading
   wildcard has to descend to the pair's key rather than swallow the whole
   pair and strand `: $V`. Regression for the partial set-match descent. *)
let test_partial_wildcard_key () =
  let pattern =
    "@@\nmatch: partial\nmetavar $K: single\nmetavar $V: single\n@@\n{ $K: $V }"
  in
  let src = "const o = { color: \"red\", size: 10 };" in
  match find ~language:"typescript" ~pattern ~source:src with
  | c :: _ ->
      let r = single c in
      Alcotest.(check (option string))
        "$K bound to a key" (Some "color") (bound_text src r "$K");
      Alcotest.(check (option string))
        "$V bound to its value" (Some "\"red\"") (bound_text src r "$V")
  | [] -> Alcotest.fail "expected a match for { $K: $V }"

(* JSX self-closing element: reordered + extra attributes are tolerated. *)
let test_partial_jsx_basic () =
  let pattern =
    "@@\n\
     match: partial\n\
     metavar $p1: single\n\
     metavar $p2: single\n\
     @@\n\
     <Foo a={$p1} b={$p2} />"
  in
  let src = "const e = <Foo b={2} a={1} c={3} />;" in
  match find ~language:"tsx" ~pattern ~source:src with
  | [ c ] ->
      let r = single c in
      Alcotest.(check (option string))
        "$p1 = 1" (Some "1") (bound_text src r "$p1");
      Alcotest.(check (option string))
        "$p2 = 2" (Some "2") (bound_text src r "$p2")
  | results ->
      Alcotest.failf "expected one JSX partial match, got %d"
        (List.length results)

(* Partial pattern requires a key the source doesn't have: no match. *)
let test_partial_missing_key_fails () =
  let pattern =
    "@@\n\
     match: partial\n\
     metavar $x: single\n\
     metavar $y: single\n\
     @@\n\
     {a: $x, b: $y}"
  in
  let src = "const o = {a: 1};" in
  Alcotest.(check int)
    "no match when source lacks key b" 0
    (List.length (find ~language:"typescript" ~pattern ~source:src))

(* A single-section partial pattern whose outer form is a named wrapper
   ([foo({...})]) is not a bracketed container: the matcher could only ever
   yield zero matches, so it is rejected at compile time with a message
   pointing at the multi-section idiom (see the composition test below). *)
let test_partial_composition_rejected () =
  let pattern = "@@\nmatch: partial\nmetavar $x: single\n@@\nfoo({a: $x})" in
  try
    let _ = find ~language:"typescript" ~pattern ~source:"foo({a: 1});" in
    Alcotest.fail "expected non-container partial pattern to raise"
  with Failure msg ->
    Alcotest.(check bool)
      "error names the single-bracketed-container requirement" true
      (let contains hay needle =
         let nl = String.length needle and hl = String.length hay in
         let rec go i =
           i + nl <= hl && (String.sub hay i nl = needle || go (i + 1))
         in
         go 0
       in
       contains msg "single bracketed container" && contains msg "on $VAR")

(* Composition done right: a strict section binds the call's object argument,
   and a partial section scoped [on] that argument subset-matches inside it.
   This expresses "a call to foo whose object arg contains a" — it matches
   only foo({a: ...}), not foo({c: ...}) (no a) and not bar({a: ...}) (wrong
   callee). This is what the rejected single-section form should have been. *)
let test_partial_composition_via_multisection () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar ARG: single\n\
     @@\n\
     foo(ARG)\n\
     @@\n\
     match: partial\n\
     on ARG\n\
     metavar x: single\n\
     @@\n\
     {a: x}"
  in
  let src = "foo({a: 1, b: 2}); foo({c: 3}); bar({a: 9});" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] -> (
      match c.sections with
      | [ call; obj ] ->
          let span (r : Matcher.M.match_result) =
            String.sub src r.start_byte (r.end_byte - r.start_byte)
          in
          Alcotest.(check string)
            "outer section: the foo call" "foo({a: 1, b: 2})" (span call);
          Alcotest.(check string)
            "inner section: the contained object" "{a: 1, b: 2}" (span obj)
      | s -> Alcotest.failf "expected 2 sections, got %d" (List.length s))
  | results ->
      Alcotest.failf "expected exactly one composite match, got %d"
        (List.length results)

(* Non-set-like nodes (statements, calls, expressions) are silently filtered
   out by the structural self-filter — only the object literal node ever
   produces the reported match. *)
let test_partial_no_spurious_match_on_non_container () =
  let pattern = "@@\nmatch: partial\nmetavar $x: single\n@@\n{a: $x}" in
  let src = "if (cond) { doIt(); } const o = {a: 1};" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] ->
      let r = single c in
      let s, e = (r.start_byte, r.end_byte) in
      Alcotest.(check string)
        "match is the object literal, not the if-block" "{a: 1}"
        (String.sub src s (e - s))
  | results ->
      Alcotest.failf "expected exactly one partial match, got %d"
        (List.length results)

(* ========================================================================= *)
(* Multi-section search                                                      *)
(* ========================================================================= *)

(* Helper: get the byte range of section [i] of a composite. *)
let composite_span (c : Matcher.composite_match) i =
  let r = List.nth c.sections i in
  (r.Matcher.M.start_byte, r.Matcher.M.end_byte)

(* Two independent sections; both must match for a composite to fire. *)
let test_multi_section_independent () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     foo($x)\n\
     @@\n\
     match: strict\n\
     metavar $y: single\n\
     @@\n\
     bar($y)"
  in
  let src = "foo(1); bar(2);" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] ->
      Alcotest.(check int)
        "two sections in composite" 2 (List.length c.sections);
      let s1, e1 = composite_span c 0 in
      let s2, e2 = composite_span c 1 in
      Alcotest.(check string)
        "section 1 matches foo(1)" "foo(1)"
        (String.sub src s1 (e1 - s1));
      Alcotest.(check string)
        "section 2 matches bar(2)" "bar(2)"
        (String.sub src s2 (e2 - s2))
  | results ->
      Alcotest.failf "expected exactly one composite, got %d"
        (List.length results)

(* Conjunctive: if one section finds no match, no composite is produced. *)
let test_multi_section_one_section_misses () =
  let pattern =
    "@@\nmatch: strict\n@@\nfoo(1)\n@@\nmatch: strict\n@@\nbar(2)"
  in
  let src = "foo(1); baz(2);" in
  Alcotest.(check int)
    "no composite when bar(2) is absent" 0
    (List.length (find ~language:"typescript" ~pattern ~source:src))

(* Shared metavar names enforce cross-section non-linearity: a $x in both
   sections must bind to the same subtree across the composite. *)
let test_multi_section_shared_metavar_consistent () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     foo($x)\n\
     @@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     bar($x)"
  in
  (* Source: foo(c) and bar(c) — same identifier, so shared $x is satisfied. *)
  let src = "foo(c); bar(c); other();" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] ->
      let r1 = List.nth c.sections 0 in
      let r2 = List.nth c.sections 1 in
      Alcotest.(check (option string))
        "section 1 $x bound to 'c'" (Some "c") (bound_text src r1 "$x");
      Alcotest.(check (option string))
        "section 2 $x bound to 'c'" (Some "c") (bound_text src r2 "$x")
  | results ->
      Alcotest.failf "expected exactly one composite, got %d"
        (List.length results)

(* Shared metavar: if both sections want $x but the source disagrees, the
   cross-section non-linearity rejects the combination and we get zero
   composites. *)
let test_multi_section_shared_metavar_inconsistent () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     foo($x)\n\
     @@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     bar($x)"
  in
  (* foo(a) and bar(b): $x can't be both. *)
  let src = "foo(a); bar(b);" in
  Alcotest.(check int)
    "cross-section non-linearity rejects" 0
    (List.length (find ~language:"typescript" ~pattern ~source:src))

(* on $VAR scopes section 2 to the subtree bound by $obj in section 1.
   The source has a `.run()` member call BOTH inside the foo() call and
   alongside it; with scoping, only the inside one is reachable, so the
   composite count differentiates working scope from leaked scope. *)
let test_multi_section_on_var () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $obj: single\n\
     @@\n\
     foo($obj)\n\
     @@\n\
     match: strict\n\
     on $obj\n\
     metavar $x: single\n\
     @@\n\
     $x.run()"
  in
  let src = "foo(callbacks.run()); helper.run();" in
  match find ~language:"typescript" ~pattern ~source:src with
  | [ c ] ->
      Alcotest.(check int) "two sections" 2 (List.length c.sections);
      let r2 = List.nth c.sections 1 in
      Alcotest.(check (option string))
        "section 2 $x bound to 'callbacks' (the in-scope receiver)"
        (Some "callbacks") (bound_text src r2 "$x")
  | results ->
      Alcotest.failf
        "expected exactly one composite (helper.run() must be out of scope), \
         got %d"
        (List.length results)

(* Parse-time error: same metavar declared with different kinds across
   sections. *)
let test_multi_section_kind_mismatch () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     foo($x)\n\
     @@\n\
     match: strict\n\
     metavar $x: sequence\n\
     @@\n\
     bar($x)"
  in
  Alcotest.check_raises "kind mismatch raises"
    (Failure
       "Metavar '$x' declared as single in section 1 and as sequence in \
        section 2") (fun () ->
      ignore (find ~language:"typescript" ~pattern ~source:"foo(1); bar(2);"))

(* ========================================================================= *)
(* Siblings enumeration (drive's Drive_match_at_all behaviour)               *)
(* ========================================================================= *)

(* A pattern with an ellipsis ([Siblings] wildcard) followed by a metavar
   enumerates every valid argument position for the metavar, not just the
   leftmost. Previously the matcher returned only the leftmost-minimum
   configuration; enumeration finds all three valid bindings. *)
let test_siblings_enumerates_arg_positions () =
  (* Pattern body: [eval(... x, )] — fragment, no closing paren needed.
     Source: [eval(a, b, c, d)]. The pattern's trailing comma requires
     x to be followed by another comma, so x can bind to a, b, or c
     (not d, which has only `)` after it). *)
  let pattern = "@@\nmatch: strict\nmetavar x: single\n@@\neval(... x, " in
  let src = "fun main() { eval(a, b, c, d) }" in
  let results = find ~language:"kotlin" ~pattern ~source:src in
  let x_texts =
    List.map
      (fun (c : Matcher.composite_match) ->
        match c.sections with
        | [ r ] -> Option.value (bound_text src r "x") ~default:"<unbound>"
        | _ -> "<multi-section>")
      results
  in
  (* The matcher should produce one composite per valid x position. *)
  Alcotest.(check (list string))
    "x bindings enumerated for each valid position" [ "a"; "b"; "c" ] x_texts

(* The multi-section composition case from the user's pattern: section 2's
   [on x] constraint filters the section-1 enumeration to bindings where
   the inner pattern matches. The matcher's binding backtracking explores
   all valid x positions and picks the one(s) satisfying [on x]. *)
let test_siblings_enumeration_drives_multi_section () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar x: single\n\
     @@\n\
     eval(... x, \n\
     @@\n\
     match: strict\n\
     on x\n\
     @@\n\
     \"sealed\""
  in
  let src = "fun main() { eval(a, b, \"sealed\", d) }" in
  let results = find ~language:"kotlin" ~pattern ~source:src in
  (* At least one composite should be reported, with x bound to a subtree
     containing "sealed". *)
  Alcotest.(check bool) "at least one composite found" true (results <> []);
  let xs =
    List.map
      (fun (c : Matcher.composite_match) ->
        match c.sections with
        | r :: _ -> Option.value (bound_text src r "x") ~default:"<unbound>"
        | [] -> "<empty>")
      results
  in
  Alcotest.(check bool)
    "x is bound to the string-literal arg in every composite" true
    (List.for_all (fun t -> t = "\"sealed\"") xs)

(* KNOWN BUG — ellipsis context lines fail to match in Kotlin's import
   position. The pattern

     ...
     - import b.B
     ...

   should anchor a single import removal inside an import list (the
   change-summary §3.2 "anchored removal in a flat sequence" shape).
   The tokenizer produces the right token stream
   ([Siblings ..._0; import b.B leaves; Siblings ..._1]) but the body
   re-parses with the [..._k] placeholders as identifier statements,
   which Kotlin's grammar does not permit around imports (only the
   package header may precede them), so the parse degrades and no match
   is found. The same pattern shape matches fine in TypeScript, both at
   top level and inside a function body.

   This test pins the CURRENT (broken) behaviour: 0 matches. When the
   placeholder strategy handles grammar-restricted positions, flip the
   expectation to >= 1 and check the surgical removal. *)
let test_siblings_kotlin_import_position_known_bug () =
  let pattern = "@@\nmatch: strict\n@@\n  ...\n- import b.B\n  ..." in
  let src = "import a.A\nimport b.B\nimport c.C\nclass X\n" in
  let results = find ~language:"kotlin" ~pattern ~source:src in
  Alcotest.(check int)
    "KNOWN BUG: siblings find no match in Kotlin import position" 0
    (List.length results)

(* Parse-time error: on $VAR references an undeclared name. *)
let test_multi_section_on_var_undeclared () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $x: single\n\
     @@\n\
     foo($x)\n\
     @@\n\
     match: strict\n\
     on $undeclared\n\
     @@\n\
     bar(1)"
  in
  Alcotest.check_raises "on undeclared raises"
    (Failure
       "Section 2: 'on $undeclared' references $undeclared, which is not \
        declared in any prior section") (fun () ->
      ignore (find ~language:"typescript" ~pattern ~source:"foo(1); bar(1);"))

(* A partial/field section that replaces the whole matched span warns (it
   silently drops tolerated/ignored content); foreach edits and strict
   transforms do not. *)
let test_pattern_warnings () =
  let warns p = Matcher.pattern_warnings p in
  let nonempty p = warns p <> [] in
  Alcotest.(check bool)
    "partial whole-replace warns" true
    (nonempty
       "@@\n\
        match: partial\n\
        metavar $V: single\n\
        @@\n\
        - { color: $V }\n\
        + { colour: $V }");
  Alcotest.(check bool)
    "field whole-replace warns" true
    (nonempty
       "@@\n\
        match: field\n\
        metavar $m: single\n\
        metavar $b: single\n\
        @@\n\
        - $m() { $b }\n\
        + $m() { log(); $b }");
  Alcotest.(check bool)
    "strict transform does not warn" false
    (nonempty "@@\nmatch: strict\nmetavar $x: single\n@@\n- foo($x)\n+ bar($x)");
  Alcotest.(check bool)
    "partial match-only guard does not warn" false
    (nonempty "@@\nmatch: partial\nmetavar $V: single\n@@\n{ color: $V }");
  Alcotest.(check bool)
    "foreach element edit does not warn" false
    (nonempty
       "@@\n\
        match: strict\n\
        metavar $PROPS: sequence\n\
        @@\n\
        f({ $PROPS })\n\
        @@\n\
        match: field\n\
        foreach $PROPS\n\
        metavar $K: single\n\
        metavar $V: single\n\
        @@\n\
        - $K: $V\n\
        + .with($V)");
  (* Surgical sub-part marking (context delimiters preserved) does NOT warn —
     only whole-container marking drops the tolerated extras. *)
  Alcotest.(check bool)
    "partial sub-part removal does not warn" false
    (nonempty
       "@@\nmatch: partial\nmetavar $v: single\n@@\n {\n-   color: $v,\n }")

(* A metavar declared but never present as a token in the pattern is rejected
   — catches typos and metavars dropped by a malformed surrounding parse
   (e.g. `data class n(p, q)`, where `q` is not valid class-parameter syntax
   and silently vanishes from the tokens). *)
let test_declared_but_absent_metavar () =
  let pattern =
    "@@\nmatch: strict\nmetavar $x: single\nmetavar $y: single\n@@\nfoo($x)"
  in
  match
    try
      ignore (find ~language:"typescript" ~pattern ~source:"foo(1);");
      None
    with Failure m -> Some m
  with
  | None -> Alcotest.fail "expected failure for unused declared metavar $y"
  | Some m ->
      let contains hay needle =
        let nl = String.length needle and hl = String.length hay in
        let rec go i =
          i + nl <= hl && (String.sub hay i nl = needle || go (i + 1))
        in
        go 0
      in
      Alcotest.(check bool)
        "names $y and explains absence" true
        (contains m "$y" && contains m "declared but never appears")

(* The metavar-arity gap (data class n(p, q) dropping `q`) surfaces as the
   same validation error rather than a silent zero-match. *)
let test_declared_absent_data_class () =
  match
    try
      ignore
        (find ~language:"kotlin"
           ~pattern:
             "@@\n\
              match: field\n\
              metavar n: single\n\
              metavar p: single\n\
              metavar q: single\n\
              @@\n\
              data class n(p, q)"
           ~source:"data class User(val a: Int, val b: Int)");
      None
    with Failure m -> Some m
  with
  | None -> Alcotest.fail "expected failure: q dropped by parse"
  | Some m ->
      Alcotest.(check bool)
        "names the dropped metavar q" true
        (String.length m > 0
        &&
        let rec has i =
          i + 1 < String.length m && (String.sub m i 1 = "q" || has (i + 1))
        in
        has 0)

(* ========================================================================= *)
(* Field mode                                                                *)
(*                                                                           *)
(* A field pattern's leaf stream is aligned to a subsequence of a            *)
(* declaration node's children: optional fields the pattern omits            *)
(* (decorators, annotations, modifier groups, return types) are skipped,     *)
(* the ones it addresses are matched fully, with backtracking. No            *)
(* per-language data. See docs/field-mode.md.                                *)
(* ========================================================================= *)

(* The matched source spans of a search, in order. *)
let field_spans ~language ~pattern ~source =
  find ~language ~pattern ~source
  |> List.concat_map (fun (c : Matcher.composite_match) ->
      List.map
        (fun (r : Matcher.M.match_result) ->
          String.sub source r.start_byte (r.end_byte - r.start_byte))
        c.sections)

(* Wildcard-name method body. Metavars are sigil-bearing here (TS/Scala
   tokenize $m as one leaf); PHP/Kotlin use sigil-free names (see those
   tests) because $ is real syntax there. *)
let ts_method body =
  "@@\nmatch: field\nmetavar $m: single\nmetavar $b: single\n@@\n" ^ body

(* TS: a decorated method with a return type matches a bare method pattern;
   decorators are class_body siblings (outside method_definition) so the span
   starts at the name, and the return type is skipped inside the node. *)
let test_field_ts_ignore_decorators () =
  Alcotest.(check (list string))
    "decorators + return type ignored"
    [ "getUsers(): User[] { return x; }" ]
    (field_spans ~language:"typescript" ~pattern:(ts_method "$m() { $b }")
       ~source:"class C { @Get(\"/u\") @Auth getUsers(): User[] { return x; } }")

(* The parameter list is a field the pattern addresses (`()`), so it is
   matched, not skipped: an empty-param pattern must not match a method with
   parameters. This is the case naive leaf-skipping gets wrong. *)
let test_field_params_are_not_skipped () =
  Alcotest.(check (list string))
    "$m() does not match $m(a, b)" []
    (field_spans ~language:"typescript"
       ~pattern:"@@\nmatch: field\nmetavar $m: single\n@@\n$m() { }"
       ~source:"class C { f(a, b) { } }")

(* A field the pattern addresses must match its content: a return type of
   number does not match a source return type of string. The `:` commits to
   the return-type child; the content mismatch inside it fails the match. *)
let test_field_return_type_content_must_match () =
  Alcotest.(check (list string))
    "return type number != string" []
    (field_spans ~language:"typescript"
       ~pattern:"@@\nmatch: field\nmetavar $m: single\n@@\n$m(): number { }"
       ~source:"class C { f(): string { } }")

(* PHP: attribute + visibility modifier + return type are direct children of
   method_declaration, so they are skipped — and the match span includes them
   (they sit inside the matched declaration node). Sigil-free metavars. *)
let test_field_php_ignore_attributes () =
  Alcotest.(check (list string))
    "attribute + visibility + return ignored"
    [ "#[Route(\"/u\")] public function getUsers(): array { return y; }" ]
    (field_spans ~language:"php"
       ~pattern:
         "@@\n\
          match: field\n\
          metavar m: single\n\
          metavar b: single\n\
          @@\n\
          function m() { b }"
       ~source:
         "<?php class C { #[Route(\"/u\")] public function getUsers(): array { \
          return y; } }")

(* Kotlin: the whole `modifiers` group (annotations AND `suspend`) is one
   child, skipped wholesale; the return type follows. Sigil-free metavars. *)
let test_field_kotlin_ignore_modifiers () =
  Alcotest.(check (list string))
    "modifiers group + return ignored"
    [ "@GET(\"/u\") suspend fun getUsers(): List<User> { return l }" ]
    (field_spans ~language:"kotlin"
       ~pattern:
         "@@\n\
          match: field\n\
          metavar m: single\n\
          metavar b: single\n\
          @@\n\
          fun m() { b }"
       ~source:
         "class C { @GET(\"/u\") suspend fun getUsers(): List<User> { return l \
          } }")

(* Scala: annotation + return type are direct children, skipped. *)
let test_field_scala_ignore_annotation () =
  Alcotest.(check (list string))
    "annotation + return ignored"
    [ "@deprecated def getUsers(): List[User] = { l }" ]
    (field_spans ~language:"scala"
       ~pattern:"@@\nmatch: field\nmetavar $m: single\n@@\ndef $m() = { l }"
       ~source:"class C { @deprecated def getUsers(): List[User] = { l } }")

(* Decorator-subset via backtracking: where each annotation is its own child
   (Scala), a concrete annotation in the pattern skips earlier ones to reach
   the match. `@deprecated` skips the leading `@inline`. *)
let test_field_decorator_subset () =
  Alcotest.(check (list string))
    "@deprecated matched among several annotations"
    [ "@inline @deprecated def getUsers() = { l }" ]
    (field_spans ~language:"scala"
       ~pattern:
         "@@\n\
          match: field\n\
          metavar $m: single\n\
          @@\n\
          @deprecated def $m() = { l }"
       ~source:"class C { @inline @deprecated def getUsers() = { l } }")

(* The child-alignment design subsumes the once-anticipated "Tier 3" gap:
   an `async` keyword (an anonymous direct child the pattern omits) is simply
   skipped, like any other unaddressed child. *)
let test_field_async_subsumed () =
  Alcotest.(check (list string))
    "async (anonymous keyword) is skipped"
    [ "async getUsers() { return x; }" ]
    (field_spans ~language:"typescript" ~pattern:(ts_method "$m() { $b }")
       ~source:"class C { async getUsers() { return x; } }")

(* ...and it does not over-match: a pattern with a body cannot match a
   bodyless abstract signature (no statement_block child for `{ $b }`), so
   only the concrete method is found. *)
let test_field_no_overmatch_bodyless () =
  Alcotest.(check (list string))
    "only the method with a body matches" [ "bar() { return 1; }" ]
    (field_spans ~language:"typescript" ~pattern:(ts_method "$m() { $b }")
       ~source:"abstract class C { abstract foo(): void; bar() { return 1; } }")

(* Source-context tokenization: a CONCRETE declaration name (not a metavar)
   works. Standalone, `getUsers` tokenizes as `identifier`; the source's
   method name is `property_identifier`, so without context the (text,
   node_type) comparison misses. The matcher re-tokenizes the pattern spliced
   into the real source at each candidate, so `getUsers` gets the
   property_identifier it has inside the class — and the decorators and return
   type are still skipped by field alignment. *)
let test_field_concrete_name () =
  Alcotest.(check (list string))
    "concrete getUsers matches through decorators + return type"
    [ "getUsers(): User[] { return x; }" ]
    (field_spans ~language:"typescript"
       ~pattern:"@@\nmatch: field\nmetavar $b: single\n@@\ngetUsers() { $b }"
       ~source:"class C { @Get() getUsers(): User[] { return x; } }")

(* The concrete name is still matched precisely — it pins the name, so a
   differently-named method does not match. *)
let test_field_concrete_name_pins () =
  Alcotest.(check (list string))
    "concrete getUsers does not match foo()" []
    (field_spans ~language:"typescript"
       ~pattern:"@@\nmatch: field\nmetavar $b: single\n@@\ngetUsers() { $b }"
       ~source:"class C { @Get() foo(): User[] { return x; } }")

(* The relaxed-alignment probe (which gates the source-context reparse) must
   still select precisely: a concrete name matches only the same-named method
   among several siblings, not the others. *)
let test_field_concrete_name_among_siblings () =
  Alcotest.(check (list string))
    "only m1 matches"
    [ "m1(): B { return b; }" ]
    (field_spans ~language:"typescript"
       ~pattern:"@@\nmatch: field\nmetavar $b: single\n@@\nm1(): B { $b }"
       ~source:
         "class C { @X() m0(): A { return a; } m1(): B { return b; } m2(): C { \
          return c; } }")

(* Source-context tokenization also covers a concrete name in Kotlin (the
   modifiers group, holding the annotation and `suspend`, is still skipped). *)
let test_field_concrete_name_kotlin () =
  Alcotest.(check (list string))
    "concrete fun getUsers matches through modifiers + return"
    [ "@GET(\"/u\") suspend fun getUsers(): List<User> { return l }" ]
    (field_spans ~language:"kotlin"
       ~pattern:"@@\nmatch: field\nmetavar b: single\n@@\nfun getUsers() { b }"
       ~source:
         "class C { @GET(\"/u\") suspend fun getUsers(): List<User> { return l \
          } }")

(* ========================================================================= *)
(* Comments (tree-sitter extras) + cross-language smoke                      *)
(*                                                                           *)
(* Ported from the retired old-matcher corpus: comment transparency, and a   *)
(* Scala end-to-end smoke (the old suite drove find/transform on Scala; the  *)
(* new suite otherwise only field-matches it).                              *)
(* ========================================================================= *)

(* A comment in the source is a tree-sitter "extra" and is ignored when the
   pattern doesn't mention it. *)
let test_comment_in_source_ignored () =
  Alcotest.(check int)
    "block comment in source ignored" 1
    (List.length
       (find ~language:"typescript"
          ~pattern:"@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)"
          ~source:"foo(/* note */ 1);"));
  Alcotest.(check int)
    "line comment after match ignored" 1
    (List.length
       (find ~language:"typescript"
          ~pattern:"@@\nmatch: strict\nmetavar $x: single\n@@\nfoo($x)"
          ~source:"const a = foo(1); // trailing"))

(* Comments are extras on the PATTERN side too: a comment written in the
   pattern is dropped by tokenization, so it is ignored rather than required.
   (This differs from the old matcher, which made an in-pattern comment a
   required match — an intentional simplification: matching "code that has
   this exact comment" is not a goal.) *)
let test_comment_in_pattern_ignored () =
  Alcotest.(check int)
    "in-pattern comment ignored, not required" 1
    (List.length
       (find ~language:"typescript"
          ~pattern:
            "@@\nmatch: strict\nmetavar $x: single\n@@\nfoo(/* keep */ $x)"
          ~source:"foo(1);"))

(* Scala end-to-end smoke: the grammar drives the new matcher's find and
   transform paths, not just field matching. *)
let test_scala_find_smoke () =
  Alcotest.(check int)
    "scala println find" 1
    (List.length
       (find ~language:"scala"
          ~pattern:"@@\nmatch: strict\nmetavar $x: single\n@@\nprintln($x)"
          ~source:"def f() = { println(\"hi\") }"))

let test_scala_transform_smoke () =
  Alcotest.(check string)
    "scala println -> logger.info" "def f() = { logger.info(\"hi\") }"
    (transform ~language:"scala"
       ~pattern:
         "@@\n\
          match: strict\n\
          metavar $x: single\n\
          @@\n\
          - println($x)\n\
          + logger.info($x)"
       ~source:"def f() = { println(\"hi\") }")

let tests =
  let open Alcotest in
  [
    test_case "parse preamble basic" `Quick test_parse_preamble_basic;
    test_case "parse preamble sigil-free" `Quick test_parse_preamble_sigil_free;
    test_case "parse preamble missing mode" `Quick
      test_parse_preamble_missing_mode;
    test_case "find calls" `Quick test_find_calls;
    test_case "sigil-free equivalent" `Quick test_sigil_free_equivalent;
    test_case "match_at_spans: per-token source spans" `Quick
      test_match_at_spans;
    test_case "classify_spatch: per-line roles" `Quick test_classify_spatch;
    test_case "comments: source comment ignored" `Quick
      test_comment_in_source_ignored;
    test_case "comments: in-pattern comment ignored" `Quick
      test_comment_in_pattern_ignored;
    test_case "scala: find smoke" `Quick test_scala_find_smoke;
    test_case "scala: transform smoke" `Quick test_scala_transform_smoke;
    test_case "find with ellipsis" `Quick test_find_with_ellipsis;
    test_case "binding extraction" `Quick test_binding_extraction;
    test_case "member call pattern" `Quick test_member_call_pattern;
    test_case "kotlin pattern" `Quick test_kotlin_pattern;
    test_case "match-side extraction" `Quick test_match_side_extraction;
    test_case "match-side context lines" `Quick test_match_side_context;
    test_case "replace-side extraction" `Quick test_replace_side_extraction;
    test_case "replace-side match-only is None" `Quick
      test_replace_side_match_only;
    test_case "replace-side removal (- only)" `Quick test_replace_side_removal;
    test_case "transform: rename" `Quick test_transform_rename;
    test_case "transform: trailing newline in pattern not leaked" `Quick
      test_transform_trailing_newline_in_pattern;
    test_case "transform: PHP statement-shaped call (missing-node fix)" `Quick
      test_transform_php_statement_shaped_call;
    test_case "transform: metavar not substituted as substring" `Quick
      test_transform_metavar_not_substring;
    test_case "transform: sigil-free metavar in string template" `Quick
      test_transform_metavar_in_string_template;
    test_case "transform: swap operands" `Quick test_transform_swap_operands;
    test_case "transform: match-only no-op" `Quick
      test_transform_match_only_noop;
    test_case "transform: removal (statement)" `Quick
      test_transform_removal_statement;
    test_case "transform: removal within context" `Quick
      test_transform_removal_in_context;
    test_case "transform: surgical `...` sub-removal preserves siblings" `Quick
      test_transform_surgical_ellipsis_removal;
    test_case "transform: surgical `...` sub-replacement preserves siblings"
      `Quick test_transform_surgical_ellipsis_replace;
    test_case "transform: surgical JSX attribute removal keeps siblings" `Quick
      test_transform_surgical_jsx_attribute;
    test_case "transform: surgical partial property removal keeps siblings"
      `Quick test_transform_surgical_partial_property;
    test_case "transform: surgical partial JSX attribute removal" `Quick
      test_transform_surgical_partial_jsx;
    test_case "transform: surgical partial in-place property edit" `Quick
      test_transform_surgical_partial_inplace;
    test_case "transform: surgical field rewrite keeps ignored modifiers" `Quick
      test_transform_surgical_field_modifiers;
    test_case "transform: whole-container partial replaces whole, not nested"
      `Quick test_transform_whole_container_partial;
    test_case "transform: bare ellipsis on -/+ line rejected" `Quick
      test_reject_bare_ellipsis_on_edit_line;
    test_case "transform: adjacent matches" `Quick
      test_transform_adjacent_matches;
    test_case "transform: placeholder boundary" `Quick
      test_transform_placeholder_boundary;
    test_case "transform: compound binding" `Quick
      test_transform_compound_binding;
    test_case "transform: two independent sections" `Quick
      test_transform_two_independent_sections;
    test_case "transform: on-scoped inner rewrite" `Quick
      test_transform_on_scoped;
    test_case "transform: shared metavar across sections" `Quick
      test_transform_shared_metavar;
    test_case "transform: nested edits rejected" `Quick
      test_transform_overlap_rejected;
    test_case "foreach: on over sequence rejected" `Quick
      test_on_over_sequence_rejected;
    test_case "foreach: foreach over single rejected" `Quick
      test_foreach_over_single_rejected;
    test_case "foreach: in-place rename" `Quick test_foreach_inplace_rename;
    test_case "foreach: scoped to its container" `Quick
      test_foreach_scoped_to_container;
    test_case "foreach: JSX attributes" `Quick test_foreach_jsx_attributes;
    test_case "foreach: concrete key (object, preserves siblings)" `Quick
      test_foreach_concrete_key_object;
    test_case "foreach: concrete key (JSX attribute)" `Quick
      test_foreach_concrete_key_jsx;
    test_case "foreach: concrete key absent -> unchanged" `Quick
      test_foreach_concrete_key_absent;
    test_case "foreach: concrete key (kotlin transparency)" `Quick
      test_foreach_concrete_key_kotlin;
    test_case "foreach: ellipsis element falls back" `Quick
      test_foreach_ellipsis_falls_back;
    test_case "remove: middle element" `Quick test_remove_middle_element;
    test_case "remove: first element" `Quick test_remove_first_element;
    test_case "remove: last element" `Quick test_remove_last_element;
    test_case "remove: only element" `Quick test_remove_only_element;
    test_case "remove: kotlin trailing-comma only" `Quick
      test_remove_kotlin_trailing_only;
    test_case "remove: kotlin trailing-comma last" `Quick
      test_remove_kotlin_trailing_last;
    test_case "splice: method chain (matchExhaustive)" `Quick
      test_splice_method_chain;
    test_case "splice: single element" `Quick test_splice_single_element;
    test_case "splice: no in-place double-apply" `Quick
      test_splice_no_inplace_double_apply;
    test_case "join: identity conjunction [a,b,c] => a && b && c" `Quick
      test_join_identity_conjunction;
    test_case "join: precedence parens" `Quick test_join_precedence_parens;
    test_case "join: foreach comma into arg list" `Quick test_join_foreach_comma;
    test_case "port: verbatim newline expansion" `Quick
      test_port_verbatim_newline;
    test_case "port: verbatim comma expansion" `Quick test_port_verbatim_comma;
    test_case "port: empty sequence expansion" `Quick test_port_empty_sequence;
    test_case "port: kotlin verbatim expansion" `Quick test_port_kotlin_verbatim;
    test_case "port: kotlin transform expansion" `Quick
      test_port_kotlin_transform;
    test_case "port: kotlin comma join" `Quick test_port_kotlin_comma;
    test_case "fragment pattern (pure)" `Quick test_fragment_pattern_pure;
    test_case "fragment pattern in rule file" `Quick
      test_fragment_pattern_in_rule;
    test_case "find_in_tree (pre-parsed)" `Quick test_find_in_tree;
    test_case "find from file" `Quick test_find_file;
    test_case "nested calls found" `Quick test_nested_calls_found;
    test_case "partial: object literal with extras" `Quick
      test_partial_object_basic;
    test_case "partial: wildcard key descends" `Quick test_partial_wildcard_key;
    test_case "partial: JSX self-closing reordered + extras" `Quick
      test_partial_jsx_basic;
    test_case "partial: missing key fails" `Quick test_partial_missing_key_fails;
    test_case "partial: non-container pattern is rejected" `Quick
      test_partial_composition_rejected;
    test_case "partial: composition via multi-section + on" `Quick
      test_partial_composition_via_multisection;
    test_case "partial: no spurious match on non-container" `Quick
      test_partial_no_spurious_match_on_non_container;
    test_case "field: TS ignore decorators + return type" `Quick
      test_field_ts_ignore_decorators;
    test_case "field: params are matched, not skipped" `Quick
      test_field_params_are_not_skipped;
    test_case "field: addressed return type content must match" `Quick
      test_field_return_type_content_must_match;
    test_case "field: PHP ignore attributes/visibility/return" `Quick
      test_field_php_ignore_attributes;
    test_case "field: Kotlin ignore modifiers group + return" `Quick
      test_field_kotlin_ignore_modifiers;
    test_case "field: Scala ignore annotation + return" `Quick
      test_field_scala_ignore_annotation;
    test_case "field: decorator-subset via backtracking" `Quick
      test_field_decorator_subset;
    test_case "field: async keyword subsumed (skipped)" `Quick
      test_field_async_subsumed;
    test_case "field: no over-match of bodyless signature" `Quick
      test_field_no_overmatch_bodyless;
    test_case "field: concrete name (source-context tokenization)" `Quick
      test_field_concrete_name;
    test_case "field: concrete name pins the name" `Quick
      test_field_concrete_name_pins;
    test_case "field: concrete name selects among siblings" `Quick
      test_field_concrete_name_among_siblings;
    test_case "field: concrete name in Kotlin" `Quick
      test_field_concrete_name_kotlin;
    test_case "multi-section: independent sections" `Quick
      test_multi_section_independent;
    test_case "multi-section: conjunctive — one section misses" `Quick
      test_multi_section_one_section_misses;
    test_case "multi-section: shared metavar (consistent)" `Quick
      test_multi_section_shared_metavar_consistent;
    test_case "multi-section: shared metavar (inconsistent fails)" `Quick
      test_multi_section_shared_metavar_inconsistent;
    test_case "multi-section: on $VAR scopes section" `Quick
      test_multi_section_on_var;
    test_case "multi-section: kind mismatch across sections" `Quick
      test_multi_section_kind_mismatch;
    test_case "warnings: partial/field whole-span replace" `Quick
      test_pattern_warnings;
    test_case "validation: declared-but-absent metavar" `Quick
      test_declared_but_absent_metavar;
    test_case "validation: data class drops metavar -> error" `Quick
      test_declared_absent_data_class;
    test_case "multi-section: on undeclared metavar" `Quick
      test_multi_section_on_var_undeclared;
    test_case "enumeration: Siblings enumerates arg positions" `Quick
      test_siblings_enumerates_arg_positions;
    test_case "enumeration: drives multi-section on $x" `Quick
      test_siblings_enumeration_drives_multi_section;
    test_case "KNOWN BUG: siblings in Kotlin import position" `Quick
      test_siblings_kotlin_import_position_known_bug;
  ]
