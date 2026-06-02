(** Tokenizer tests: pattern body text -> Stmatch.pattern_token list.

    Covers sigil-free metavar detection (a leaf is a metavar iff its text equals
    a declared name), concrete-token classification with observed node types,
    extras filtering, and a round-trip through the matcher against real source.
*)

open Diffract

let ctx = Context.create ()

(* Regression: bare-keyword/punctuation-only fragments like [} else {]
   parse to a single ERROR node carrying [is_extra=true]; the tokenizer
   must descend into it (not skip it as a comment-like extra) so the
   leaves come out. *)
let test_bare_keyword_fragment () =
  let tokens =
    Tokenize.tokenize ~ctx ~language:"typescript" ~single_metavars:[]
      ~sequence_metavars:[] "} else {"
  in
  let texts =
    List.filter_map
      (function Stmatch.Concrete { text; _ } -> Some text | _ -> None)
      tokens
  in
  Alcotest.(check (list string))
    "fragment '} else {' yields [\"}\", \"else\", \"{\"]" [ "}"; "else"; "{" ]
    texts

let test_bare_punctuation_fragment () =
  let tokens =
    Tokenize.tokenize ~ctx ~language:"typescript" ~single_metavars:[]
      ~sequence_metavars:[] "for ("
  in
  let texts =
    List.filter_map
      (function Stmatch.Concrete { text; _ } -> Some text | _ -> None)
      tokens
  in
  Alcotest.(check (list string))
    "fragment 'for (' yields [\"for\", \"(\"]" [ "for"; "(" ] texts

let tokenize ?(single = []) ?(sequence = []) ~language body =
  Tokenize.tokenize ~ctx ~language ~single_metavars:single
    ~sequence_metavars:sequence body

(* Render a token to a compact string for easy assertion:
     C(text:node_type)  concrete
     S(name)            subtree (single) wildcard
     Q(name)            siblings (sequence) wildcard *)
let show_token : Stmatch.pattern_token -> string = function
  | Concrete { text; node_type } -> Printf.sprintf "C(%s:%s)" text node_type
  | Subtree { name } -> Printf.sprintf "S(%s)" (Option.value name ~default:"_")
  | Siblings { name } -> Printf.sprintf "Q(%s)" (Option.value name ~default:"_")

let show tokens = List.map show_token tokens

(* ========================================================================= *)
(* Tokenization structure                                                    *)
(* ========================================================================= *)

(* `foo($x)` with `$x` declared single — the sigil happens to be present but
   is not special; `$x` is just the declared name. *)
let test_call_with_sigil_metavar () =
  let toks = tokenize ~language:"typescript" ~single:[ "$x" ] "foo($x)" in
  Alcotest.(check (list string))
    "foo($x)"
    [ "C(foo:identifier)"; "C((:()"; "S($x)"; "C():))" ]
    (show toks)

(* `foo(obj)` with `obj` declared single — sigil-free. Structurally identical
   to the sigil version; only the bound name differs. *)
let test_call_with_sigil_free_metavar () =
  let toks = tokenize ~language:"typescript" ~single:[ "obj" ] "foo(obj)" in
  Alcotest.(check (list string))
    "foo(obj)"
    [ "C(foo:identifier)"; "C((:()"; "S(obj)"; "C():))" ]
    (show toks)

(* An identifier that is NOT a declared metavar stays a concrete token, even
   if it textually contains a declared name. `obj` is declared, but `object`
   is a different leaf and must remain concrete. *)
let test_non_metavar_identifier_is_concrete () =
  let toks = tokenize ~language:"typescript" ~single:[ "obj" ] "object" in
  Alcotest.(check (list string))
    "object stays concrete" [ "C(object:identifier)" ] (show toks)

(* Object literal with two single metavars. *)
let test_object_literal () =
  let toks =
    tokenize ~language:"typescript" ~single:[ "$x"; "$y" ] "{a: $x, b: $y}"
  in
  Alcotest.(check (list string))
    "{a: $x, b: $y}"
    [
      "C({:{)";
      "C(a:property_identifier)";
      "C(:::)";
      "S($x)";
      "C(,:,)";
      "C(b:property_identifier)";
      "C(:::)";
      "S($y)";
      "C(}:})";
    ]
    (show toks)

(* Member access: both object and method are metavars. *)
let test_member_access () =
  let toks =
    tokenize ~language:"typescript" ~single:[ "$obj"; "$m" ] "$obj.$m()"
  in
  Alcotest.(check (list string))
    "$obj.$m()"
    [ "S($obj)"; "C(.:.)"; "S($m)"; "C((:()"; "C():))" ]
    (show toks)

(* Sequence metavar becomes a Siblings wildcard. *)
let test_sequence_metavar () =
  let toks =
    tokenize ~language:"typescript" ~sequence:[ "$args" ] "foo($args)"
  in
  Alcotest.(check (list string))
    "foo($args)"
    [ "C(foo:identifier)"; "C((:()"; "Q($args)"; "C():))" ]
    (show toks)

(* A name declared as both? Single takes precedence in our lookup order;
   not a real scenario but pins the behavior. *)
let test_single_precedence_over_sequence () =
  let toks =
    tokenize ~language:"typescript" ~single:[ "$x" ] ~sequence:[ "$x" ] "$x"
  in
  Alcotest.(check (list string)) "single wins" [ "S($x)" ] (show toks)

(* Comments in the pattern are extras and must not become tokens. *)
let test_comment_is_filtered () =
  let toks =
    tokenize ~language:"typescript" ~single:[ "$x" ] "foo(/* hi */ $x)"
  in
  Alcotest.(check (list string))
    "comment skipped"
    [ "C(foo:identifier)"; "C((:()"; "S($x)"; "C():))" ]
    (show toks)

(* ========================================================================= *)
(* Ellipsis                                                                  *)
(* ========================================================================= *)

(* `...` in argument position becomes an anonymous-but-uniquely-named
   Siblings wildcard. *)
let test_ellipsis_in_args () =
  let toks = tokenize ~language:"typescript" "foo(...)" in
  Alcotest.(check (list string))
    "foo(...)"
    [ "C(foo:identifier)"; "C((:()"; "Q(..._0)"; "C():))" ]
    (show toks)

(* `...` in statement position. The placeholder must survive as a leaf
   without introducing a spurious `;` token. *)
let test_ellipsis_in_block () =
  let toks = tokenize ~language:"typescript" "function f() { ... }" in
  Alcotest.(check (list string))
    "function f() { ... }"
    [
      "C(function:function)";
      "C(f:identifier)";
      "C((:()";
      "C():))";
      "C({:{)";
      "Q(..._0)";
      "C(}:})";
    ]
    (show toks)

(* Two ellipses get distinct synthetic names. *)
let test_multiple_ellipsis () =
  let toks =
    tokenize ~language:"typescript" ~single:[ "$x" ] "f(..., $x, ...)"
  in
  Alcotest.(check (list string))
    "f(..., $x, ...)"
    [
      "C(f:identifier)";
      "C((:()";
      "Q(..._0)";
      "C(,:,)";
      "S($x)";
      "C(,:,)";
      "Q(..._1)";
      "C():))";
    ]
    (show toks)

(* A spread/rest operator (`...` immediately followed by an identifier or
   `$`) is preserved as a literal `...`, not treated as ellipsis. *)
let test_spread_preserved () =
  let toks =
    tokenize ~language:"typescript" ~sequence:[ "$args" ] "f(...$args)"
  in
  (* `...` stays a concrete spread token; `$args` is the sequence metavar. *)
  Alcotest.(check (list string))
    "f(...$args)"
    [ "C(f:identifier)"; "C((:()"; "C(...:...)"; "Q($args)"; "C():))" ]
    (show toks)

(* ========================================================================= *)
(* Round-trip: tokenize then match against real source                       *)
(* ========================================================================= *)

module M = Stmatch.Make (Tree_sitter_cursor)

let find_in pattern_tokens ~language source =
  let tree = Tree.parse ~ctx ~language source in
  let cursor = Tree_sitter_cursor.of_tree tree in
  M.find_matches pattern_tokens cursor

(* Tokenize `foo($x)` and match it against a source with two foo calls. *)
let test_roundtrip_finds_calls () =
  let toks = tokenize ~language:"typescript" ~single:[ "$x" ] "foo($x)" in
  let results = find_in toks ~language:"typescript" "foo(1); bar(2); foo(3);" in
  Alcotest.(check int) "two foo(_) matches" 2 (List.length results)

(* The sigil-free pattern `foo(obj)` and the sigil pattern `foo($x)` produce
   matchers that behave identically — the name is just a binding label. *)
let test_roundtrip_sigil_free_equivalent () =
  let sigil = tokenize ~language:"typescript" ~single:[ "$x" ] "foo($x)" in
  let bare = tokenize ~language:"typescript" ~single:[ "obj" ] "foo(obj)" in
  let src = "foo(42);" in
  let n_sigil = List.length (find_in sigil ~language:"typescript" src) in
  let n_bare = List.length (find_in bare ~language:"typescript" src) in
  Alcotest.(check int) "sigil matches" 1 n_sigil;
  Alcotest.(check int) "sigil-free matches identically" n_sigil n_bare

(* Round-trip with a binding extracted via byte_range. *)
let test_roundtrip_binding () =
  let toks = tokenize ~language:"typescript" ~single:[ "$x" ] "foo($x)" in
  let src = "foo(c > 0);" in
  match find_in toks ~language:"typescript" src with
  | [ r ] -> (
      let bound =
        List.find_map
          (function
            | M.Single { name = "$x"; cursor } -> Some cursor | _ -> None)
          r.M.bindings
      in
      match bound with
      | None -> Alcotest.fail "no binding for $x"
      | Some c ->
          let s, e = Tree_sitter_cursor.byte_range c in
          Alcotest.(check string)
            "$x bound to 'c > 0'" "c > 0"
            (String.sub src s (e - s)))
  | _ -> Alcotest.fail "expected exactly one match"

(* Ellipsis round-trip: `foo(...)` matches a call with any arguments. *)
let test_roundtrip_ellipsis () =
  let toks = tokenize ~language:"typescript" "foo(...)" in
  let results = find_in toks ~language:"typescript" "foo(1, 2, 3); bar();" in
  Alcotest.(check int) "one foo(...) match" 1 (List.length results)

(* tokenize_with_lines pairs each token with its 0-based line index. *)
let describe_lined toks =
  List.map
    (fun (t, line) ->
      let k =
        match t with
        | Stmatch.Concrete { text; _ } -> text
        | Stmatch.Subtree { name } -> Option.value name ~default:"_"
        | Stmatch.Siblings { name } -> Option.value name ~default:"_"
      in
      (k, line))
    toks

let test_with_lines_basic () =
  let toks =
    Tokenize.tokenize_with_lines ~ctx ~language:"typescript"
      ~single_metavars:[ "$x" ] ~sequence_metavars:[] "foo(\n$x\n)"
  in
  Alcotest.(check (list (pair string int)))
    "tokens carry their line index"
    [ ("foo", 0); ("(", 0); ("$x", 1); (")", 2) ]
    (describe_lined toks)

(* Ellipsis preprocessing rewrites `...` to a longer placeholder, but it
   preserves newlines — so a token after a `...` line keeps its true line
   index (the role-tagging invariant). *)
let test_with_lines_ellipsis_preserves_line () =
  let toks =
    Tokenize.tokenize_with_lines ~ctx ~language:"typescript"
      ~single_metavars:[ "$x" ] ~sequence_metavars:[] "f(\n...\n$x\n)"
  in
  let line_of name =
    List.find_map
      (fun (t, line) ->
        match t with
        | Stmatch.Subtree { name = Some n } when n = name -> Some line
        | _ -> None)
      toks
  in
  Alcotest.(check (option int)) "$x is on line 2 despite the `...` rewrite"
    (Some 2) (line_of "$x")

let tests =
  let open Alcotest in
  [
    test_case "tokenize_with_lines: basic line indices" `Quick
      test_with_lines_basic;
    test_case "tokenize_with_lines: ellipsis preserves line" `Quick
      test_with_lines_ellipsis_preserves_line;
    test_case "call with sigil metavar" `Quick test_call_with_sigil_metavar;
    test_case "call with sigil-free metavar" `Quick
      test_call_with_sigil_free_metavar;
    test_case "non-metavar identifier stays concrete" `Quick
      test_non_metavar_identifier_is_concrete;
    test_case "object literal" `Quick test_object_literal;
    test_case "member access" `Quick test_member_access;
    test_case "sequence metavar" `Quick test_sequence_metavar;
    test_case "single precedence over sequence" `Quick
      test_single_precedence_over_sequence;
    test_case "comment is filtered" `Quick test_comment_is_filtered;
    test_case "ellipsis in args" `Quick test_ellipsis_in_args;
    test_case "ellipsis in block" `Quick test_ellipsis_in_block;
    test_case "multiple ellipsis" `Quick test_multiple_ellipsis;
    test_case "spread preserved" `Quick test_spread_preserved;
    test_case "round-trip ellipsis" `Quick test_roundtrip_ellipsis;
    test_case "round-trip finds calls" `Quick test_roundtrip_finds_calls;
    test_case "round-trip sigil-free equivalent" `Quick
      test_roundtrip_sigil_free_equivalent;
    test_case "round-trip binding via byte_range" `Quick test_roundtrip_binding;
    test_case "bare-keyword fragment '} else {'" `Quick
      test_bare_keyword_fragment;
    test_case "bare-punctuation fragment 'for ('" `Quick
      test_bare_punctuation_fragment;
  ]
