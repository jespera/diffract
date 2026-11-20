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

(* transform_edits exposes the spans+replacements transform would apply,
   without applying them: splicing the edits manually must reproduce the
   transform's own output. *)
let test_transform_edits_spans () =
  let pattern =
    "@@\nmatch: strict\nmetavar $x: single\n@@\n- foo($x)\n+ bar($x)"
  in
  let source = "foo(1); keep(); foo(2);" in
  let edits =
    Matcher.transform_edits ~ctx ~language:"typescript" ~pattern_text:pattern
      ~source_text:source
  in
  Alcotest.(check int) "two edits" 2 (List.length edits);
  let spliced =
    (* apply descending so earlier offsets stay valid *)
    List.fold_left
      (fun src (ed : Matcher.edit) ->
        String.sub src 0 ed.start_byte
        ^ ed.replacement
        ^ String.sub src ed.end_byte (String.length src - ed.end_byte))
      source
      (List.sort
         (fun (a : Matcher.edit) b -> compare b.start_byte a.start_byte)
         edits)
  in
  Alcotest.(check string)
    "splicing the edits equals transform's output"
    (transform ~language:"typescript" ~pattern ~source)
    spliced

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

(* An inline `...` on a `+` line is rejected: the ellipsis is a match-side
   binder and has nothing to bind in a replacement (it used to be emitted
   literally into the output). The valid formulation puts the shared `...` on
   a context line — see the next test. *)
let test_transform_plus_inline_ellipsis_rejected () =
  let raises pattern =
    try
      ignore (transform ~language:"typescript" ~pattern ~source:"f(a, last);");
      false
    with Failure _ -> true
  in
  Alcotest.(check bool)
    "inline `...` on a `+` line rejected" true
    (raises "@@\nmatch: strict\n@@\n- f(..., last)\n+ g(..., last)");
  Alcotest.(check bool)
    "`+`-only inline `...` rejected" true
    (raises "@@\nmatch: strict\n@@\n- old()\n+ renamed(...)")

(* The valid formulation of "change one element, keep the rest": the shared
   `...` sits on a context line of its own and only the changing element is
   marked. The captured siblings are preserved verbatim by the surgical
   transform — here through type arguments. *)
let test_transform_context_ellipsis_in_typeargs () =
  let pattern =
    "@@\n\
     match: strict\n\
     @@\n\
    \  useQuery<\n\
    \      ...\n\
     -     OldUserId\n\
     + NewUserId\n\
    \    >(opts)"
  in
  let out =
    transform ~language:"typescript" ~pattern
      ~source:"useQuery<\"required\", AppUser, OldUserId>(opts);"
  in
  Alcotest.(check string)
    "leading type args preserved"
    "useQuery<\"required\", AppUser, NewUserId>(opts);" out

(* An inline `...` on a `-` line stays legal: it binds, and the captured run
   is deliberately dropped by the rewrite. *)
let test_transform_minus_inline_ellipsis_drops_run () =
  let pattern =
    "@@\nmatch: strict\nmetavar x: single\n@@\n- f(..., x)\n+ g(x)"
  in
  let out =
    transform ~language:"typescript" ~pattern ~source:"f(a, b(1), x);"
  in
  Alcotest.(check string) "captured run dropped" "g(x);" out

(* A spread (`...args`) on a `+` line is an operator, not an ellipsis — it
   is emitted verbatim and not rejected. *)
let test_transform_plus_spread_untouched () =
  let pattern = "@@\nmatch: strict\n@@\n- g(x)\n+ f(...args)" in
  let out = transform ~language:"typescript" ~pattern ~source:"g(x);" in
  Alcotest.(check string) "spread emitted verbatim" "f(...args);" out

(* A [+]-only hunk whose anchor sits at a line boundary is spliced as whole
   lines — own line, newline-terminated, indented like the deeper of the two
   neighbouring lines — instead of glued onto the closing delimiter. The
   append-before-closer form (the spartacus standalone-flag shape). *)
let test_transform_insert_append_before_closer () =
  let pattern =
    "@@\nmatch: strict\n@@\n @Component({\n ...\n+ standalone: false,\n })"
  in
  let source =
    "@Component({\n  selector: 'cx-card',\n  template: '',\n})\nclass C {}\n"
  in
  let out = transform ~language:"typescript" ~pattern ~source in
  Alcotest.(check string)
    "inserted as its own indented line"
    "@Component({\n\
    \  selector: 'cx-card',\n\
    \  template: '',\n\
    \  standalone: false,\n\
     })\n\
     class C {}\n"
    out

(* The prepend mirror: a [+] line directly after the opening line lands at
   the start of the first element's line, indented like it. *)
let test_transform_insert_prepend_after_opener () =
  let pattern =
    "@@\nmatch: strict\n@@\n @Component({\n+ standalone: false,\n ...\n })"
  in
  let source = "@Component({\n  selector: 'x',\n})\nclass C {}\n" in
  let out = transform ~language:"typescript" ~pattern ~source in
  Alcotest.(check string)
    "inserted before the first element"
    "@Component({\n  standalone: false,\n  selector: 'x',\n})\nclass C {}\n" out

(* A multi-line [+] block keeps its relative indentation: the block's common
   indent is stripped and the inferred indent prefixed, so how far the author
   indented under the pattern's margin doesn't leak into the output. *)
let test_transform_insert_multiline_block_reindented () =
  let pattern =
    "@@\nmatch: strict\n@@\n register({\n ...\n+ deps: [\n+   core,\n+ ],\n })"
  in
  let source = "register({\n  name: 'svc',\n});\n" in
  let out = transform ~language:"typescript" ~pattern ~source in
  Alcotest.(check string)
    "block reindented, relative structure kept"
    "register({\n  name: 'svc',\n  deps: [\n    core,\n  ],\n});\n" out

(* An inline anchor (one-line container) keeps the tight zero-width splice:
   no line rendering, the author owns the separators. *)
let test_transform_insert_inline_stays_tight () =
  let pattern = "@@\nmatch: strict\n@@\n cfg({\n ...\n+ b: 2,\n })" in
  let source = "cfg({ a: 1, });\n" in
  let out = transform ~language:"typescript" ~pattern ~source in
  Alcotest.(check string) "tight splice" "cfg({ a: 1, b: 2,});\n" out

(* Same line rendering through a non-TS grammar (kotlin call arguments). *)
let test_transform_insert_kotlin_append () =
  let pattern = "@@\nmatch: strict\n@@\n configure(\n ...\n+ timeout,\n )" in
  let source = "configure(\n    host,\n    port,\n)\n" in
  let out = transform ~language:"kotlin" ~pattern ~source in
  Alcotest.(check string)
    "kotlin argument appended on its own line"
    "configure(\n    host,\n    port,\n    timeout,\n)\n" out

(* A [+] line between two `...` lines has no anchor — the split between the
   two captured runs is arbitrary, so the insertion point would be too.
   Rejected at compile rather than silently placed. *)
let test_transform_insert_between_ellipses_rejected () =
  let raises pattern =
    try
      ignore
        (transform ~language:"typescript" ~pattern ~source:"f({ a: 1, b: 2 });");
      false
    with Failure _ -> true
  in
  Alcotest.(check bool)
    "+ between two ... rejected" true
    (raises "@@\nmatch: strict\n@@\n f({\n ...\n+ x: 1,\n ...\n })")

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
(* on over a sequence: rewrite matches inside each element                  *)
(* ========================================================================= *)

(* `on` takes a sequence metavar as well as a single one: the section is
   searched inside each element the sequence bound, and nowhere else. *)
let test_on_over_sequence_scopes () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $xs: sequence\n\
     @@\n\
     foo($xs)\n\
     @@\n\
     match: strict\n\
     on $xs\n\
     metavar $a: single\n\
     @@\n\
     bar($a)"
  in
  let n source = List.length (find ~language:"typescript" ~pattern ~source) in
  Alcotest.(check int)
    "one per element that contains a match" 2
    (n "foo(bar(1), 2, wrap(bar(3)));");
  Alcotest.(check int)
    "outside the sequence is out of scope" 0 (n "foo(1); bar(2);")

(* The removed directive fails with a pointer to its replacement, rather
   than being read as a line of pattern preamble. *)
let test_foreach_directive_rejected () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $xs: sequence\n\
     @@\n\
     foo($xs)\n\
     @@\n\
     match: strict\n\
     foreach $xs\n\
     @@\n\
     bar()"
  in
  let msg =
    try
      ignore (find ~language:"typescript" ~pattern ~source:"foo(1);");
      ""
    with Failure m -> m
  in
  let has needle =
    let n = String.length needle in
    let rec go i =
      i + n <= String.length msg && (String.sub msg i n = needle || go (i + 1))
    in
    go 0
  in
  Alcotest.(check bool) "names the replacement" true (has "on $xs")

(* A match nested inside an element is found too: `on` searches inside,
   where the old per-element form only tested each element as a whole. *)
let test_on_over_sequence_nested () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     on $PROPS\n\
     metavar $V: single\n\
     @@\n\
     - color: $V\n\
     + colour: $V"
  in
  Alcotest.(check string)
    "nested property renamed" {|foo({ size: 1, style: { colour: "red" } });|}
    (transform ~language:"typescript" ~pattern
       ~source:{|foo({ size: 1, style: { color: "red" } });|})

(* An element that itself matches shadows what is nested in it: transforms
   take the outermost match. *)
let test_on_over_sequence_outermost () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     on $PROPS\n\
     metavar $KEY: single\n\
     metavar $VAL: single\n\
     @@\n\
     - $KEY: $VAL\n\
     + $KEY: wrap($VAL)"
  in
  Alcotest.(check string)
    "outer pair wrapped once" "foo({a: wrap(1), b: wrap({c: 2})});"
    (transform ~language:"typescript" ~pattern ~source:"foo({a: 1, b: {c: 2}});")

(* The block-scoped rewrite: delete a local's declaration and reroute its
   uses in the statements that follow it in the same block, and only there. *)
let test_on_over_sequence_block_scope () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar PRE: sequence\n\
     metavar POST: sequence\n\
     metavar E: single\n\
     @@\n\
    \  {\n\
    \  PRE\n\
     - val admin = E\n\
    \  POST\n\
    \  }\n\
     @@\n\
     match: strict\n\
     on POST\n\
     metavar F: single\n\
     @@\n\
     - admin.F\n\
     + ctx.admin.F"
  in
  Alcotest.(check string)
    "uses after the declaration rerouted; other function untouched"
    "fun t() {\n\
    \    val a = 1\n\
    \    use(ctx.admin.userId)\n\
    \    run { other(ctx.admin.name) }\n\
     }\n\
     fun u() {\n\
    \    use(admin.userId)\n\
     }\n"
    (transform ~language:"kotlin" ~pattern
       ~source:
         "fun t() {\n\
         \    val a = 1\n\
         \    val admin = lookup()\n\
         \    use(admin.userId)\n\
         \    run { other(admin.name) }\n\
          }\n\
          fun u() {\n\
         \    use(admin.userId)\n\
          }\n")

(* In-place map: rename every property of the object passed to foo(),
   leaving the container and separators untouched. *)
let test_on_seq_inplace_rename () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     on $PROPS\n\
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
let test_on_seq_scoped_to_container () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     on $PROPS\n\
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
   confirm `on` over a sequence isn't object-specific. *)
let test_on_seq_jsx_attributes () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $ATTRS: sequence\n\
     @@\n\
     <Foo $ATTRS />\n\
     @@\n\
     match: strict\n\
     on $ATTRS\n\
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
(* on over a sequence with a CONCRETE key                                    *)
(*                                                                           *)
(* A concrete object key like `color` tokenized standalone parses as a       *)
(* labeled-statement label (statement_identifier), not the source pair's     *)
(* property_identifier. Lexical leaf comparison tolerates that, so the bare  *)
(* element pattern matches. Sibling elements are preserved.                  *)
(* ========================================================================= *)

(* TS: rename only the `color` property, preserving `size`. *)
let test_on_seq_concrete_key_object () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     on $PROPS\n\
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
let test_on_seq_concrete_key_jsx () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $ATTRS: sequence\n\
     @@\n\
     <Foo $ATTRS />\n\
     @@\n\
     match: strict\n\
     on $ATTRS\n\
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
let test_on_seq_concrete_key_absent () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     on $PROPS\n\
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
let test_on_seq_concrete_key_kotlin () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar ARGS: sequence\n\
     @@\n\
     f(ARGS)\n\
     @@\n\
     match: strict\n\
     on ARGS\n\
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
let test_on_seq_ellipsis_falls_back () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar $PROPS: sequence\n\
     @@\n\
     foo({ $PROPS })\n\
     @@\n\
     match: strict\n\
     on $PROPS\n\
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
(* Strict deletion beside a sequence binder: separator cleanup               *)
(*                                                                           *)
(* A `-` line between context `...` lines is a removed list element. The     *)
(* `...` beside it captured the neighbouring siblings, separators included,  *)
(* so the adjacent separator leaf is absorbed (following, else preceding)    *)
(* without the author writing it.                                            *)
(* ========================================================================= *)

let strict_rm_prop key =
  Printf.sprintf
    "@@\n\
     match: strict\n\
     metavar $V: single\n\
     @@\n\
    \  foo({\n\
    \  ...\n\
     - %s: $V\n\
    \  ...\n\
    \  })"
    key

let strict_rm_arg = "@@\nmatch: strict\n@@\n  f(\n  ...\n- gone\n  ...\n  )"

let check_rm ~language ~pattern cases =
  List.iter
    (fun (source, expected) ->
      Alcotest.(check string)
        source expected
        (transform ~language ~pattern ~source))
    cases

let test_strict_remove_property () =
  check_rm ~language:"typescript" ~pattern:(strict_rm_prop "b")
    [
      ("foo({ a: 1, b: 2, c: 3 });", "foo({ a: 1, c: 3 });");
      ("foo({ b: 2, c: 3 });", "foo({ c: 3 });");
      ("foo({ a: 1, b: 2 });", "foo({ a: 1 });");
      ("foo({ b: 2 });", "foo({  });");
    ]

let test_strict_remove_argument () =
  check_rm ~language:"typescript" ~pattern:strict_rm_arg
    [
      ("f(a, gone, c);", "f(a, c);");
      ("f(gone, c);", "f(c);");
      ("f(a, gone);", "f(a);");
      ("f(gone);", "f();");
      ("f(gone,);", "f();");
    ]

(* The trailing-comma idiom survives: the between-comma is absorbed, the
   trailing one stays. *)
let test_strict_remove_kotlin_trailing () =
  check_rm ~language:"kotlin" ~pattern:strict_rm_arg
    [
      ("val x = f(a, gone, c)", "val x = f(a, c)");
      ("val x = f(a, gone,)", "val x = f(a, )");
    ]

(* A separator the author already put on a `-` line is not doubled. *)
let test_strict_remove_separator_written () =
  check_rm ~language:"typescript"
    ~pattern:"@@\nmatch: strict\n@@\n  f(\n  ...\n- ,\n- gone\n  ...\n  )"
    [ ("f(a, gone, c);", "f(a, c);"); ("f(a, gone);", "f(a);") ]

(* A named sequence metavar is a binder like `...`; a binder on one side is
   enough. *)
let test_strict_remove_one_sided_binder () =
  check_rm ~language:"typescript"
    ~pattern:
      "@@\n\
       match: strict\n\
       metavar $REST: sequence\n\
       @@\n\
      \  f(\n\
       - gone\n\
      \  $REST\n\
      \  )"
    [ ("f(gone, b, c);", "f(b, c);") ];
  check_rm ~language:"typescript"
    ~pattern:"@@\nmatch: strict\n@@\n  f(\n  ...\n- gone\n  )"
    [ ("f(a, gone);", "f(a);"); ("f(gone);", "f();") ]

(* A concrete context neighbour is kept exactly as written: the pattern says
   to keep that comma. *)
let test_strict_remove_concrete_neighbour_kept () =
  check_rm ~language:"typescript"
    ~pattern:"@@\nmatch: strict\n@@\n  f(a,\n- gone\n  )"
    [ ("f(a, gone);", "f(a, );") ]

(* A statement is not a separated element: its terminator is its own, and
   the deletion stays line-oriented. *)
let test_strict_remove_statement_line () =
  check_rm ~language:"typescript"
    ~pattern:"@@\nmatch: strict\n@@\n  {\n  ...\n- gone();\n  ...\n  }"
    [
      ( "function g() {\n  a();\n  gone();\n  c();\n}\n",
        "function g() {\n  a();\n  c();\n}\n" );
    ]

(* ========================================================================= *)
(* join directive + identity splice                                          *)
(* ========================================================================= *)

(* Identity splice with a join separator: an array becomes a conjunction.
   The elements render as their source text, joined by " && ".
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

(* ========================================================================= *)
(* Expansion ports: the old column-0 expansion lines re-authored in the new  *)
(* join surface, validating it covers the same capability. The old           *)
(* `~ $V` (newline) becomes `join $V by "\n"`; `, $V` becomes                 *)
(* `join $V by ", "`. The old per-element transform expansion (a following  *)
(* `on $V` section rendered into the template) has no successor.            *)
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

(* A repeated metavar whose occurrences sit at different depths. Kotlin wraps
   a lone call argument in a [value_argument] node spanning the same bytes,
   so [x] is wrapped in [f(x)] and bare in [val y = x]; and in [x + 1] the
   repeated name has to step down into the expression it arrives at. *)
let kotlin_repeated_metavar_count ?(decls = "") body =
  let pattern =
    "@@\nmatch: strict\nmetavar X: single\n" ^ decls ^ "@@\n" ^ body
  in
  let source =
    "fun t() {\n    f(x)\n    h()\n    x + 1\n    g(x)\n    val y = x\n}\n"
  in
  List.length (find ~language:"kotlin" ~pattern ~source)

let test_repeated_metavar_wrapped_then_bare () =
  Alcotest.(check int)
    "f(X) ... val Y = X" 1
    (kotlin_repeated_metavar_count ~decls:"metavar Y: single\n"
       "f(X)\n...\nval Y = X")

let test_repeated_metavar_bare_then_wrapped () =
  Alcotest.(check int)
    "X + 1 ... g(X)" 1
    (kotlin_repeated_metavar_count "X + 1\n...\ng(X)")

let test_repeated_metavar_descends_into_expression () =
  Alcotest.(check int)
    "f(X) ... X + 1" 1
    (kotlin_repeated_metavar_count "f(X)\n...\nX + 1")

let test_repeated_metavar_different_value_rejected () =
  Alcotest.(check int)
    "f(X) ... h(X) has no X in h()" 0
    (kotlin_repeated_metavar_count "f(X)\n...\nh(X)")

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
(* Conjunction backtracks across sections                                    *)
(* ========================================================================= *)

(* Section 1 can bind [X] to either argument; only one choice lets section 2
   match. The rule must fire whichever argument that is — committing to the
   first configuration ([X = a]) before looking at section 2 made it fail
   whenever the answer was a later one. *)
let guard_then_edit =
  "@@\n\
   match: strict\n\
   metavar X: single\n\
   @@\n\
  \  f(... X ...)\n\
   @@\n\
   match: strict\n\
   metavar X: single\n\
   @@\n\
   - use(X)\n\
   + used(X)"

let test_conjunction_retries_later_configuration () =
  Alcotest.(check string)
    "second argument" "f(a, b); used(b);"
    (transform ~language:"typescript" ~pattern:guard_then_edit
       ~source:"f(a, b); use(b);");
  Alcotest.(check string)
    "first argument, as before" "f(a, b); used(a);"
    (transform ~language:"typescript" ~pattern:guard_then_edit
       ~source:"f(a, b); use(a);")

(* When several configurations would work, a transform commits the first. *)
let test_conjunction_commits_first_that_works () =
  Alcotest.(check string)
    "only the first is rewritten" "f(a, b); used(a); use(b);"
    (transform ~language:"typescript" ~pattern:guard_then_edit
       ~source:"f(a, b); use(a); use(b);")

let test_conjunction_no_configuration_works () =
  let source = "f(a, b); use(c);" in
  Alcotest.(check string)
    "unchanged" source
    (transform ~language:"typescript" ~pattern:guard_then_edit ~source)

(* Search reports every configuration the later section distinguishes, even
   though they share section 1's span. *)
let test_conjunction_search_keeps_same_span_configurations () =
  let count source =
    List.length (find ~language:"typescript" ~pattern:guard_then_edit ~source)
  in
  Alcotest.(check int) "second argument" 1 (count "f(a, b); use(b);");
  Alcotest.(check int) "both arguments" 2 (count "f(a, b); use(a); use(b);");
  Alcotest.(check int) "neither" 0 (count "f(a, b); use(c);")

(* The retried section carries the edit: the edit must land on the
   configuration that was committed, not on the first one. *)
let test_conjunction_edit_follows_committed_configuration () =
  let pattern =
    "@@\n\
     match: strict\n\
     metavar X: single\n\
     @@\n\
    \  f(...\n\
     - X\n\
     + done(X)\n\
    \  ...)\n\
     @@\n\
     match: strict\n\
     metavar X: single\n\
     @@\n\
    \  use(X)"
  in
  Alcotest.(check string)
    "edit lands on the argument section 2 selects"
    "f(a, done(b), c); use(b); f(done(p), q); use(p); f(m, n);"
    (transform ~language:"typescript" ~pattern
       ~source:"f(a, b, c); use(b); f(p, q); use(p); f(m, n);")

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
  (* [x] is the string-literal argument in every composite — never [a] or
     [b], which have no "sealed" inside. It may be bound either to the literal
     or to its content: a metavar can bind a string's content and carry on
     past the closing quote, and since section 2 reads [x] those are two
     distinct configurations, both reported. (Span de-duplication used to
     hide the second. Whether a code-position metavar should bind string
     content at all is a separate question.) *)
  Alcotest.(check bool)
    "x is bound to the string-literal arg in every composite" true
    (List.for_all (fun t -> t = "\"sealed\"" || t = "sealed") xs)

(* Ellipsis context lines in a grammar-restricted position. The pattern

     ...
   - import b.B
     ...

   anchors a single import removal inside an import list (the
   change-summary §3.2 "anchored removal in a flat sequence" shape).
   An own-line ellipsis is blanked out before parsing rather than
   replaced with an identifier placeholder: an identifier statement is
   illegal between Kotlin imports (only the package header may precede
   them) and degrades the parse so the [import] keyword re-parses as a
   [simple_identifier], failing the type-sensitive leaf comparison.
   With blanking, the body parses cleanly and the [Siblings] wildcard
   is spliced into the token stream at the recorded offset. Inline
   ellipses ([eval(... x,]) keep the placeholder strategy, which they
   need to occupy their list slot. *)
let test_siblings_kotlin_import_position () =
  let pattern = "@@\nmatch: strict\n@@\n  ...\n- import b.B\n  ..." in
  let src = "import a.A\nimport b.B\nimport c.C\nclass X\n" in
  let results = find ~language:"kotlin" ~pattern ~source:src in
  Alcotest.(check bool)
    "siblings match in Kotlin import position" true
    (List.length results > 0);
  (* The transform removes exactly the anchored import, preserving the
     ellipsis-captured neighbours. *)
  Alcotest.(check string)
    "surgical removal of the anchored import"
    "import a.A\nimport c.C\nclass X\n"
    (transform ~language:"kotlin" ~pattern ~source:src)

(* Same bug class, second instance: a Kotlin class body permits only
   member declarations, so the identifier placeholder degraded the parse
   there too. Own-line blanking fixes it identically. *)
let test_siblings_kotlin_class_body_position () =
  let pattern =
    "@@\nmatch: strict\n@@\nclass C {\n    ...\n-     fun b() {}\n    ...\n}"
  in
  let src = "class C {\n    fun a() {}\n    fun b() {}\n    fun c() {}\n}\n" in
  Alcotest.(check string)
    "surgical removal of the anchored member"
    "class C {\n    fun a() {}\n    fun c() {}\n}\n"
    (transform ~language:"kotlin" ~pattern ~source:src)

(* Same bug class, third instance: a Scala match expression permits only
   case clauses in its body. *)
let test_siblings_scala_match_position () =
  let pattern =
    "@@\nmatch: strict\n@@\nx match {\n  ...\n-     case 2 => b\n  ...\n}"
  in
  let src =
    "object O {\n\
    \  def f(x: Int) = x match {\n\
    \    case 1 => a\n\
    \    case 2 => b\n\
    \    case 3 => c\n\
    \  }\n\
     }\n"
  in
  let results = find ~language:"scala" ~pattern ~source:src in
  Alcotest.(check bool)
    "siblings match in Scala case-clause position" true
    (List.length results > 0);
  Alcotest.(check string)
    "surgical removal of the anchored case clause"
    "object O {\n\
    \  def f(x: Int) = x match {\n\
    \    case 1 => a\n\
    \    case 3 => c\n\
    \  }\n\
     }\n"
    (transform ~language:"scala" ~pattern ~source:src)

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
   silently drops tolerated/ignored content); strict transforms do not. *)
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
    "markerless '+' line warns" true
    (nonempty "@@\nmatch: strict\n@@\n f({\n ...\n+standalone,\n })");
  Alcotest.(check bool)
    "markerless '-' in a transform warns" true
    (nonempty "@@\nmatch: strict\n@@\n-old()\n+ renamed()");
  Alcotest.(check bool)
    "column-0 '-' in a pure-match body does not warn" false
    (nonempty "@@\nmatch: strict\n@@\n-x < y");
  Alcotest.(check bool)
    "partial match-only guard does not warn" false
    (nonempty "@@\nmatch: partial\nmetavar $V: single\n@@\n{ color: $V }");
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

(* Lexical comparison closed the role-mismatch papercut: a pattern whose
   fragment parse assigns the wrong role ([React.FC] standalone reads FC as a
   [property_identifier], the source has a [type_identifier]) now matches
   directly — the historical `search --explain` motivating case. text_only
   remains for what lexical still (deliberately) rejects: same text inside a
   string literal. *)
let test_text_only_finds_role_mismatch () =
  let src = "const C: React.FC = x;" in
  let pattern = "@@\nmatch: strict\n@@\nReact.FC" in
  Alcotest.(check int)
    "lexical strict search finds the role-mismatched occurrence" 1
    (List.length (find ~language:"tsx" ~pattern ~source:src));
  (* String interiors stay out of reach of strict search; text-only (the
     --explain fallback) still reports them. A single-token pattern, because
     string content is one leaf — multi-token code text never aligns with it. *)
  let pattern_fc = "@@\nmatch: strict\n@@\nFC" in
  let src_str = {|const s = "FC";|} in
  Alcotest.(check int)
    "strict search does not match inside a string" 0
    (List.length (find ~language:"tsx" ~pattern:pattern_fc ~source:src_str));
  let tree = Tree.parse ~ctx ~language:"tsx" src_str in
  Alcotest.(check int)
    "text-only finds the in-string occurrence" 1
    (List.length
       (Matcher.text_only_find_in_tree ~ctx ~language:"tsx"
          ~pattern_text:pattern_fc tree))

(* PHP heredoc/nowdoc: a quote-LESS string form — [Tree.string_delimited]
   recognizes the [<<<] introducer, so code patterns stay out of heredoc
   interiors just as they stay out of quoted strings. Guards the lexical
   comparison's one hard boundary for the delimiter form quotes don't
   cover. *)
let test_lexical_heredoc_interior_excluded () =
  let src = "<?php\nfunction f() {\n  $x = <<<EOT\nfoo\nEOT;\n  foo();\n}\n" in
  (* php_only parses bare PHP; strip the tag for the source under test. *)
  let src = String.concat "" (String.split_on_char '\r' src) in
  let src =
    match String.index_opt src '\n' with
    | Some i -> String.sub src (i + 1) (String.length src - i - 1)
    | None -> src
  in
  Alcotest.(check int)
    "heredoc interior does not match a code pattern" 1
    (List.length
       (find ~language:"php" ~pattern:"@@\nmatch: strict\n@@\nfoo" ~source:src))

let test_text_only_scoped_to_single_strict () =
  (* The hint is scoped to a single global strict section; a partial pattern
     (or multi-section / on) yields no hint locations. *)
  let src = "const o = {a: 1};" in
  let tree = Tree.parse ~ctx ~language:"typescript" src in
  Alcotest.(check int)
    "partial pattern is out of hint scope" 0
    (List.length
       (Matcher.text_only_find_in_tree ~ctx ~language:"typescript"
          ~pattern_text:"@@\nmatch: partial\nmetavar $x: single\n@@\n{a: $x}"
          tree))

(* required_literals — the search prefilter's needle extraction. Every
   word-like Concrete token of the match IR is verbatim-required in a
   matching source; metavars, ellipsis and [+]-line content must contribute
   nothing. *)
let literals ~language pattern =
  Matcher.required_literals ~ctx ~language ~pattern_text:pattern

let test_required_literals_jsx_partial () =
  Alcotest.(check (list string))
    "identifiers kept (longest first); metavar, ellipsis, punctuation dropped"
    [ "HedebyTable"; "getRowId" ]
    (literals ~language:"tsx"
       "@@\n\
        match: partial\n\
        metavar F: single\n\
        @@\n\
        <HedebyTable<...>  getRowId=F />")

let test_required_literals_plus_line_excluded () =
  Alcotest.(check (list string))
    "match side only: - line kept, + line excluded" [ "oldName" ]
    (literals ~language:"typescript"
       "@@\nmatch: strict\nmetavar $x: single\n@@\n- oldName($x)\n+ newName($x)")

let test_required_literals_multi_section_union () =
  Alcotest.(check (list string))
    "conjunctive sections both contribute" [ "wrapper"; "inner" ]
    (literals ~language:"typescript"
       "@@\n\
        match: strict\n\
        metavar $body: single\n\
        @@\n\
        wrapper($body)\n\
        @@\n\
        match: strict\n\
        on $body\n\
        @@\n\
        inner()")

let test_required_literals_none () =
  Alcotest.(check (list string))
    "all-metavar pattern yields no needles (no prefilter possible)" []
    (literals ~language:"typescript"
       "@@\nmatch: strict\nmetavar $x: single\nmetavar $y: single\n@@\n$x + $y")

let test_source_may_match () =
  let literals = [ "HedebyTable"; "getRowId" ] in
  Alcotest.(check bool)
    "both needles present -> may match" true
    (Matcher.source_may_match ~literals "<HedebyTable getRowId={(r) => r.id} />");
  Alcotest.(check bool)
    "needle at the very end of source found" true
    (Matcher.source_may_match ~literals:[ "getRowId" ] "const f = x.getRowId");
  Alcotest.(check bool)
    "one needle absent -> cannot match" false
    (Matcher.source_may_match ~literals "<HedebyTable rows={rows} />");
  Alcotest.(check bool)
    "needle longer than source -> cannot match" false
    (Matcher.source_may_match ~literals:[ "HedebyTable" ] "short");
  Alcotest.(check bool)
    "no needles -> every source may match" true
    (Matcher.source_may_match ~literals:[] "anything")

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
    test_case "transform_edits: spans splice to transform output" `Quick
      test_transform_edits_spans;
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
    test_case "transform: inline + ellipsis rejected" `Quick
      test_transform_plus_inline_ellipsis_rejected;
    test_case "transform: context ellipsis in type args" `Quick
      test_transform_context_ellipsis_in_typeargs;
    test_case "transform: - inline ellipsis drops run" `Quick
      test_transform_minus_inline_ellipsis_drops_run;
    test_case "transform: + spread untouched" `Quick
      test_transform_plus_spread_untouched;
    test_case "transform: insertion appended before closer on its own line"
      `Quick test_transform_insert_append_before_closer;
    test_case "transform: insertion prepended after opener" `Quick
      test_transform_insert_prepend_after_opener;
    test_case "transform: multi-line insertion block reindented" `Quick
      test_transform_insert_multiline_block_reindented;
    test_case "transform: inline insertion stays tight" `Quick
      test_transform_insert_inline_stays_tight;
    test_case "transform: kotlin insertion appended" `Quick
      test_transform_insert_kotlin_append;
    test_case "transform: + between two ... rejected" `Quick
      test_transform_insert_between_ellipses_rejected;
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
    test_case "on sequence: scopes to the elements" `Quick
      test_on_over_sequence_scopes;
    test_case "on sequence: nested match found" `Quick
      test_on_over_sequence_nested;
    test_case "on sequence: outermost match wins" `Quick
      test_on_over_sequence_outermost;
    test_case "on sequence: block-scoped rewrite" `Quick
      test_on_over_sequence_block_scope;
    test_case "foreach directive rejected" `Quick
      test_foreach_directive_rejected;
    test_case "on sequence: in-place rename" `Quick test_on_seq_inplace_rename;
    test_case "on sequence: scoped to its container" `Quick
      test_on_seq_scoped_to_container;
    test_case "on sequence: JSX attributes" `Quick test_on_seq_jsx_attributes;
    test_case "on sequence: concrete key (object, preserves siblings)" `Quick
      test_on_seq_concrete_key_object;
    test_case "on sequence: concrete key (JSX attribute)" `Quick
      test_on_seq_concrete_key_jsx;
    test_case "on sequence: concrete key absent -> unchanged" `Quick
      test_on_seq_concrete_key_absent;
    test_case "on sequence: concrete key (kotlin transparency)" `Quick
      test_on_seq_concrete_key_kotlin;
    test_case "on sequence: ellipsis element falls back" `Quick
      test_on_seq_ellipsis_falls_back;
    test_case "strict remove: object property" `Quick
      test_strict_remove_property;
    test_case "strict remove: call argument" `Quick test_strict_remove_argument;
    test_case "strict remove: kotlin trailing comma" `Quick
      test_strict_remove_kotlin_trailing;
    test_case "strict remove: separator already written" `Quick
      test_strict_remove_separator_written;
    test_case "strict remove: one-sided binder" `Quick
      test_strict_remove_one_sided_binder;
    test_case "strict remove: concrete neighbour kept" `Quick
      test_strict_remove_concrete_neighbour_kept;
    test_case "strict remove: statement stays line-oriented" `Quick
      test_strict_remove_statement_line;
    test_case "join: identity conjunction [a,b,c] => a && b && c" `Quick
      test_join_identity_conjunction;
    test_case "port: verbatim newline expansion" `Quick
      test_port_verbatim_newline;
    test_case "port: verbatim comma expansion" `Quick test_port_verbatim_comma;
    test_case "port: empty sequence expansion" `Quick test_port_empty_sequence;
    test_case "port: kotlin verbatim expansion" `Quick test_port_kotlin_verbatim;
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
    test_case "repeated metavar: wrapped then bare" `Quick
      test_repeated_metavar_wrapped_then_bare;
    test_case "repeated metavar: bare then wrapped" `Quick
      test_repeated_metavar_bare_then_wrapped;
    test_case "repeated metavar: descends into an expression" `Quick
      test_repeated_metavar_descends_into_expression;
    test_case "repeated metavar: different value rejected" `Quick
      test_repeated_metavar_different_value_rejected;
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
    test_case "conjunction: retries a later configuration" `Quick
      test_conjunction_retries_later_configuration;
    test_case "conjunction: commits the first that works" `Quick
      test_conjunction_commits_first_that_works;
    test_case "conjunction: no configuration works" `Quick
      test_conjunction_no_configuration_works;
    test_case "conjunction: search keeps same-span configurations" `Quick
      test_conjunction_search_keeps_same_span_configurations;
    test_case "conjunction: edit follows the committed configuration" `Quick
      test_conjunction_edit_follows_committed_configuration;
    test_case "enumeration: Siblings enumerates arg positions" `Quick
      test_siblings_enumerates_arg_positions;
    test_case "enumeration: drives multi-section on $x" `Quick
      test_siblings_enumeration_drives_multi_section;
    test_case "siblings: own-line ellipsis in Kotlin import position" `Quick
      test_siblings_kotlin_import_position;
    test_case "siblings: own-line ellipsis in Kotlin class body" `Quick
      test_siblings_kotlin_class_body_position;
    test_case "siblings: own-line ellipsis in Scala match" `Quick
      test_siblings_scala_match_position;
    test_case "explain: text-only finds role-mismatched occurrence" `Quick
      test_text_only_finds_role_mismatch;
    test_case "explain: hint scoped to single strict section" `Quick
      test_text_only_scoped_to_single_strict;
    test_case "lexical: heredoc interior excluded" `Quick
      test_lexical_heredoc_interior_excluded;
    test_case "required_literals: JSX partial pattern" `Quick
      test_required_literals_jsx_partial;
    test_case "required_literals: + line excluded" `Quick
      test_required_literals_plus_line_excluded;
    test_case "required_literals: multi-section union (incl. on $VAR)" `Quick
      test_required_literals_multi_section_union;
    test_case "required_literals: all-metavar pattern -> none" `Quick
      test_required_literals_none;
    test_case "source_may_match: needle presence decides" `Quick
      test_source_may_match;
  ]
