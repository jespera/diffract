(** STMatch algorithm tests.

    Two layers of tests:

    1. Diffract-specific: covers strict literal matching with both text and
    node-type comparison, subtree wildcards, sibling wildcards (which the Python
    prototype lacks), and edge cases.

    2. stsearch cross-validation: direct port of the assertions from
    [stsearch/algorithm.py:74-105], to verify our implementation matches the
    reference's semantics on the same fixtures. *)

open Diffract
open Test_cursor

(* ========================================================================= *)
(* Matcher instantiation                                                     *)
(* ========================================================================= *)

module M = Stmatch.Make (Test_cursor)

let matches pattern tree =
  let cursor = Test_cursor.of_tree tree in
  match M.match_at pattern cursor with Some _ -> true | None -> false

(* ========================================================================= *)
(* Pattern construction helpers                                              *)
(* ========================================================================= *)

(* Concrete tokens default to node_type "identifier", matching the
   leaves built by [id] in test_cursor.ml. *)
let con text : Stmatch.pattern_token =
  Concrete { text; node_type = "identifier" }

let sub : Stmatch.pattern_token = Subtree { name = None }
let sib : Stmatch.pattern_token = Siblings { name = None }

(* Concrete with explicit type, for node-type discrimination tests. *)
let con_t text node_type : Stmatch.pattern_token = Concrete { text; node_type }

(* String shorthand to build a sequence of one-char concrete tokens. *)
let cons s =
  let result = ref [] in
  String.iter (fun c -> result := con (String.make 1 c) :: !result) s;
  List.rev !result

(* ========================================================================= *)
(* Layer 1: diffract-specific tests                                          *)
(* ========================================================================= *)

(* ----- Strict literal matching: text required ----- *)

let test_literal_single_match () =
  Alcotest.(check bool) "a matches a" true (matches [ con "a" ] (id "a"))

let test_literal_single_mismatch () =
  Alcotest.(check bool) "a doesn't match b" false (matches [ con "a" ] (id "b"))

let test_literal_descends_to_leaf () =
  Alcotest.(check bool)
    "a matches nd[a]" true
    (matches [ con "a" ] (nd [ id "a" ]))

let test_literal_sequence_matches () =
  Alcotest.(check bool)
    "a;b;c matches nd[a;b;c]" true
    (matches (cons "abc") (nd [ id "a"; id "b"; id "c" ]))

let test_literal_sequence_mismatches_at_middle () =
  Alcotest.(check bool)
    "a;X;c doesn't match nd[a;b;c]" false
    (matches [ con "a"; con "X"; con "c" ] (nd [ id "a"; id "b"; id "c" ]))

let test_pattern_shorter_than_tree () =
  Alcotest.(check bool)
    "[a] matches nd[a;b] (pattern shorter ok)" true
    (matches [ con "a" ] (nd [ id "a"; id "b" ]))

let test_pattern_longer_than_tree () =
  Alcotest.(check bool)
    "[a;b] doesn't match nd[a]" false
    (matches (cons "ab") (nd [ id "a" ]))

(* ----- Strict literal matching: node_type required ----- *)

let test_node_type_discrimination_same_text () =
  (* The KEY diffract extension: same text, different node_type, must fail. *)
  let tree = leaf "string_content" "a" in
  let pattern = [ con_t "a" "identifier" ] in
  Alcotest.(check bool)
    "identifier doesn't match string_content" false (matches pattern tree)

let test_node_type_discrimination_matching_type () =
  let tree = leaf "string_content" "a" in
  let pattern = [ con_t "a" "string_content" ] in
  Alcotest.(check bool)
    "string_content matches string_content" true (matches pattern tree)

let test_node_type_discrimination_in_sequence () =
  (* nd[id_a; str_a]: pattern [identifier a; string_content a] should
     match, while [identifier a; identifier a] should not. *)
  let tree = nd [ leaf "identifier" "a"; leaf "string_content" "a" ] in
  Alcotest.(check bool)
    "mixed types in order" true
    (matches [ con_t "a" "identifier"; con_t "a" "string_content" ] tree);
  Alcotest.(check bool)
    "all identifier doesn't match" false
    (matches [ con_t "a" "identifier"; con_t "a" "identifier" ] tree)

(* ----- Subtree wildcards ----- *)

let test_subtree_wildcard_matches_leaf () =
  Alcotest.(check bool) "[_] matches a leaf" true (matches [ sub ] (id "a"))

let test_subtree_wildcard_matches_internal_node () =
  Alcotest.(check bool)
    "[_] matches nd[a;b]" true
    (matches [ sub ] (nd [ id "a"; id "b" ]))

let test_subtree_wildcard_with_literals_around () =
  (* [a;_;c] against nd[a;b;c]: _ binds b *)
  Alcotest.(check bool)
    "[a;_;c] matches nd[a;b;c]" true
    (matches [ con "a"; sub; con "c" ] (nd [ id "a"; id "b"; id "c" ]))

let test_subtree_wildcard_binds_subtree () =
  (* [a;_;c] against nd[a; nd[x;y]; c]: _ binds the inner nd. *)
  Alcotest.(check bool)
    "wildcard binds compound subtree" true
    (matches
       [ con "a"; sub; con "c" ]
       (nd [ id "a"; nd [ id "x"; id "y" ]; id "c" ]))

let test_subtree_wildcard_descends_on_failure () =
  (* [a;_;c] against nd[a;b;c]: _ tentatively binds compound first;
     when surrounding match fails it should descend. We force this by
     putting _ at the boundary of a compound subtree:
     [_;b;c] against nd[nd[a;b];c] — _ must bind 'a' specifically. *)
  Alcotest.(check bool)
    "wildcard descends left spine" true
    (matches [ sub; con "b"; con "c" ] (nd [ nd [ id "a"; id "b" ]; id "c" ]))

(* ----- Sibling wildcards ----- *)

let test_siblings_wildcard_matches_zero () =
  Alcotest.(check bool)
    "[a;...;c] matches nd[a;c]" true
    (matches [ con "a"; sib; con "c" ] (nd [ id "a"; id "c" ]))

let test_siblings_wildcard_matches_one () =
  Alcotest.(check bool)
    "[a;...;c] matches nd[a;b;c]" true
    (matches [ con "a"; sib; con "c" ] (nd [ id "a"; id "b"; id "c" ]))

let test_siblings_wildcard_matches_many () =
  Alcotest.(check bool)
    "[a;...;c] matches nd[a;x;y;z;c]" true
    (matches
       [ con "a"; sib; con "c" ]
       (nd [ id "a"; id "x"; id "y"; id "z"; id "c" ]))

let test_siblings_wildcard_at_start () =
  Alcotest.(check bool)
    "[...;c] matches nd[a;b;c]" true
    (matches [ sib; con "c" ] (nd [ id "a"; id "b"; id "c" ]))

let test_siblings_wildcard_at_end () =
  Alcotest.(check bool)
    "[a;...] matches nd[a;b;c]" true
    (matches [ con "a"; sib ] (nd [ id "a"; id "b"; id "c" ]))

let test_siblings_wildcard_only () =
  Alcotest.(check bool)
    "[...] matches nd[a;b;c]" true
    (matches [ sib ] (nd [ id "a"; id "b"; id "c" ]))

let test_siblings_wildcard_fails_when_anchor_missing () =
  Alcotest.(check bool)
    "[a;...;c] doesn't match nd[a;b]" false
    (matches [ con "a"; sib; con "c" ] (nd [ id "a"; id "b" ]))

(* ----- Mixed wildcard patterns ----- *)

let test_subtree_then_siblings () =
  (* [_;...;c] against nd[a;b;d;c]: _ binds 'a', siblings absorbs b,d. *)
  Alcotest.(check bool)
    "[_;...;c] matches nd[a;b;d;c]" true
    (matches [ sub; sib; con "c" ] (nd [ id "a"; id "b"; id "d"; id "c" ]))

let test_siblings_then_subtree () =
  Alcotest.(check bool)
    "[a;...;_] matches nd[a;b;c]" true
    (matches [ con "a"; sib; sub ] (nd [ id "a"; id "b"; id "c" ]))

(* ----- Edge cases ----- *)

let test_empty_pattern_against_leaf () =
  (* Empty pattern against any cursor: in our model match_at with []
     trivially succeeds (returns Some cursor) — the for loop doesn't
     execute. This deviates slightly from stsearch's [match([], None)]
     semantics, but is consistent and useful. *)
  Alcotest.(check bool) "empty pattern matches" true (matches [] (id "a"))

let test_literal_doesnt_match_internal_node_via_first_leaf () =
  (* [n] (looking for "n" as identifier) against nd[a;b]: first_leaf
     descends to 'a', compared to "n", fails. *)
  Alcotest.(check bool)
    "[n-as-identifier] doesn't match nd[a;b]" false
    (matches [ con "n" ] (nd [ id "a"; id "b" ]))

let test_double_sibling_wildcards () =
  (* [...;b;...] against nd[a;b;c]: should match with both siblings
     non-empty. *)
  Alcotest.(check bool)
    "[...;b;...] matches nd[a;b;c]" true
    (matches [ sib; con "b"; sib ] (nd [ id "a"; id "b"; id "c" ]))

(* ========================================================================= *)
(* Layer 2: stsearch algorithm.py cross-validation                           *)
(* ========================================================================= *)

(* Helpers that mirror the Python fixture style:
   - one-letter identifier leaves with their letter as text;
   - internal nodes labelled by lowercase letter (the label doesn't
     affect matching since the matcher only compares leaves).

   Python: a, b, c, r, s, t = map(T, 'abc' + 'rst')
   - In Python, a/b/c are used as leaves in the trees AND as the
     letter values compared by token().
   - r, s, t are used as internal-node labels.

   Our OCaml maps:
   - a, b, c → id "a", id "b", id "c" (leaves)
   - r, s, t → nd "r", nd "s", nd "t" (we use a single nd type since
     the label doesn't matter for the algorithm). *)

let a = id "a"
let b = id "b"
let c = id "c"

(* Internal node helpers; the Python label is informational only. *)
let r children = nd children
let s children = nd children
let t children = nd children

(* Python pattern strings → OCaml token lists. *)
let pa = con "a"
let pb = con "b"
let pc = con "c"
let _w : Stmatch.pattern_token = sub (* Python's [_] = Wildcard = our Subtree *)

(* The assertions from algorithm.py:74-105. *)

let positive_fixtures =
  [
    ( "match([], None) -- empty pattern matches empty cursor (we use \
       single-leaf)",
      true,
      (* Python: match([], None). Our model: empty pattern always
       succeeds; treat single-leaf as the analogue. *)
      [],
      a );
    ("match('a', a.cursor())", true, [ pa ], a);
    ("match('a', t(a).cursor())", true, [ pa ], t [ a ]);
    ("match('aa', t(a, a).cursor())", true, [ pa; pa ], t [ a; a ]);
    ("match('ab', t(a, b).cursor())", true, [ pa; pb ], t [ a; b ]);
    ("match('ab', r(a, t(b)).cursor())", true, [ pa; pb ], r [ a; t [ b ] ]);
    ( "match('ab', r(s(a), t(b)).cursor())",
      true,
      [ pa; pb ],
      r [ s [ a ]; t [ b ] ] );
    ("match('abc', t(a, b, c).cursor())", true, [ pa; pb; pc ], t [ a; b; c ]);
    ( "match('abc', r(t(a, b), c).cursor())",
      true,
      [ pa; pb; pc ],
      r [ t [ a; b ]; c ] );
    ( "match('abc', r(s(a, t(b)), c).cursor())",
      true,
      [ pa; pb; pc ],
      r [ s [ a; t [ b ] ]; c ] );
    ( "match('abc', r(s(a), s(t(b, c))).cursor())",
      true,
      [ pa; pb; pc ],
      r [ s [ a ]; s [ t [ b; c ] ] ] );
    ("match([_], a.cursor())", true, [ _w ], a);
    ("match([_], t(a).cursor())", true, [ _w ], t [ a ]);
    ("match([_], t(a, b).cursor())", true, [ _w ], t [ a; b ]);
    ("match(['a', _], t(a, b).cursor())", true, [ pa; _w ], t [ a; b ]);
    ("match([_, 'b'], t(a, b).cursor())", true, [ _w; pb ], t [ a; b ]);
    ( "match(['a', _, 'c'], t(a, b, c).cursor())",
      true,
      [ pa; _w; pc ],
      t [ a; b; c ] );
    ( "match([_, 'b', 'c'], r(t(a, b), c).cursor())",
      true,
      [ _w; pb; pc ],
      r [ t [ a; b ]; c ] );
    ( "match(['a', _, 'c'], r(a, t(b, c)).cursor())",
      true,
      [ pa; _w; pc ],
      r [ a; t [ b; c ] ] );
  ]

let negative_fixtures =
  [
    (* Skipped: match([], a.cursor()) — our empty-pattern semantics
     differ (we succeed). Documented at test_empty_pattern_against_leaf
     above. *)
    ("not match(['a'], b.cursor())", false, [ pa ], b);
    ("not match(['a', 'b'], a.cursor())", false, [ pa; pb ], a);
    ("not match(['a', 'b'], b.cursor())", false, [ pa; pb ], b);
    ("not match(['a', 'a'], t(a, b).cursor())", false, [ pa; pa ], t [ a; b ]);
    ("not match(['a', _], t(a).cursor())", false, [ pa; _w ], t [ a ]);
    ("not match([_, 'b'], t(b).cursor())", false, [ _w; pb ], t [ b ]);
    ( "not match(['a', _, 'c'], t(a, c).cursor())",
      false,
      [ pa; _w; pc ],
      t [ a; c ] );
    ( "not match([_, 'b', 'c'], r(t(b), c).cursor())",
      false,
      [ _w; pb; pc ],
      r [ t [ b ]; c ] );
    ( "not match(['a', _, 'c'], r(a, t(b)).cursor())",
      false,
      [ pa; _w; pc ],
      r [ a; t [ b ] ] );
  ]

let make_fixture_test (name, expected, pat, tree) =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (matches pat tree))

let stsearch_fixture_tests =
  List.map make_fixture_test (positive_fixtures @ negative_fixtures)

(* ========================================================================= *)
(* Layer 3: named wildcards, bindings, non-linearity                         *)
(* ========================================================================= *)

(* Helpers for inspecting bindings. *)
let bindings_of pattern tree =
  match M.match_at pattern (Test_cursor.of_tree tree) with
  | Some (_, bs) -> Some bs
  | None -> None

(* Extract the leaf text of the cursor bound by a Single binding. *)
let single_binding_text bs name =
  List.find_map
    (function
      | M.Single { name = n; cursor } when n = name ->
          let leaf = Test_cursor.move_first_leaf cursor in
          Some (Test_cursor.leaf_text leaf)
      | _ -> None)
    bs

let sequence_binding_count bs name =
  List.find_map
    (function
      | M.Sequence { name = n; cursors } when n = name ->
          Some (List.length cursors)
      | _ -> None)
    bs

let sequence_binding_texts bs name =
  List.find_map
    (function
      | M.Sequence { name = n; cursors } when n = name ->
          Some
            (List.map
               (fun c -> Test_cursor.leaf_text (Test_cursor.move_first_leaf c))
               cursors)
      | _ -> None)
    bs

let nx name : Stmatch.pattern_token = Subtree { name = Some name }
let nseq name : Stmatch.pattern_token = Siblings { name = Some name }

(* ----- Named single wildcards bind correctly. ----- *)

let test_named_subtree_records_binding () =
  let bs = bindings_of [ con "a"; nx "x" ] (nd [ id "a"; id "b" ]) in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check (option string))
        "x bound to b" (Some "b")
        (single_binding_text bs "x")

let test_named_subtree_binds_compound_subtree () =
  (* x should bind to the entire compound subtree nd[b;c], not just b. *)
  let bs =
    bindings_of [ con "a"; nx "x" ] (nd [ id "a"; nd [ id "b"; id "c" ] ])
  in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs -> (
      (* The bound cursor's leftmost leaf is b — the cursor IS positioned
         at the compound subtree, not at b. *)
      match single_binding_text bs "x" with
      | None -> Alcotest.fail "x not bound"
      | Some leftmost ->
          Alcotest.(check string)
            "compound subtree's leftmost leaf is b" "b" leftmost)

let test_anonymous_subtree_records_no_binding () =
  let bs = bindings_of [ con "a"; sub ] (nd [ id "a"; id "b" ]) in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check int)
        "no bindings for anonymous wildcard" 0 (List.length bs)

(* ----- Named siblings wildcards bind a list. ----- *)

let test_named_siblings_records_sequence_one () =
  let bs =
    bindings_of [ con "a"; nseq "P"; con "c" ] (nd [ id "a"; id "b"; id "c" ])
  in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check (option (list string)))
        "P contains b" (Some [ "b" ])
        (sequence_binding_texts bs "P")

let test_named_siblings_records_sequence_many () =
  let bs =
    bindings_of
      [ con "a"; nseq "P"; con "z" ]
      (nd [ id "a"; id "b"; id "c"; id "d"; id "z" ])
  in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check (option (list string)))
        "P contains b, c, d"
        (Some [ "b"; "c"; "d" ])
        (sequence_binding_texts bs "P")

let test_named_siblings_records_sequence_zero () =
  let bs = bindings_of [ con "a"; nseq "P"; con "b" ] (nd [ id "a"; id "b" ]) in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check (option int))
        "P empty" (Some 0)
        (sequence_binding_count bs "P")

let test_anonymous_siblings_records_no_binding () =
  let bs =
    bindings_of [ con "a"; sib; con "c" ] (nd [ id "a"; id "b"; id "c" ])
  in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check int)
        "no bindings for anonymous siblings" 0 (List.length bs)

(* ----- Non-linearity: same name appears twice in pattern. ----- *)

let test_nonlinear_equal_subtrees_match () =
  (* [x; x] against nd[a, a]. The two a's are structurally equal. *)
  Alcotest.(check bool)
    "non-linear pattern matches with equal subtrees" true
    (Option.is_some (bindings_of [ nx "x"; nx "x" ] (nd [ id "a"; id "a" ])))

let test_nonlinear_different_subtrees_fail () =
  (* [x; x] against nd[a, b]. Different subtrees → fail. *)
  Alcotest.(check bool)
    "non-linear pattern fails with different subtrees" false
    (Option.is_some (bindings_of [ nx "x"; nx "x" ] (nd [ id "a"; id "b" ])))

let test_nonlinear_one_binding_recorded () =
  (* The pattern [x; x] should record ONE binding for x, not two. *)
  let bs = bindings_of [ nx "x"; nx "x" ] (nd [ id "a"; id "a" ]) in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      let n_singles =
        List.length
          (List.filter (function M.Single _ -> true | _ -> false) bs)
      in
      Alcotest.(check int) "one Single binding for x" 1 n_singles

let test_nonlinear_with_compound_subtrees () =
  (* Pattern [x; x] against nd[nd[a,b]; nd[a,b]]. Both compound subtrees
     are structurally equal. *)
  Alcotest.(check bool)
    "non-linear pattern matches structurally-equal compounds" true
    (Option.is_some
       (bindings_of
          [ nx "x"; nx "x" ]
          (nd [ nd [ id "a"; id "b" ]; nd [ id "a"; id "b" ] ])))

(* ----- Non-linearity for Siblings (sequence equality). ----- *)

let test_nonlinear_siblings_equal_sequences () =
  (* Pattern [P; foo; P; bar] against source with two identical [a, b]
     sequences separated by foo and followed by bar. *)
  let foo = leaf "kw" "foo" in
  let bar = leaf "kw" "bar" in
  let foo_pat : Stmatch.pattern_token =
    Concrete { text = "foo"; node_type = "kw" }
  in
  let bar_pat : Stmatch.pattern_token =
    Concrete { text = "bar"; node_type = "kw" }
  in
  let tree = nd [ id "a"; id "b"; foo; id "a"; id "b"; bar ] in
  Alcotest.(check bool)
    "non-linear siblings matches equal sequences" true
    (Option.is_some (bindings_of [ nseq "P"; foo_pat; nseq "P"; bar_pat ] tree))

let test_nonlinear_siblings_different_sequences_fail () =
  (* Same pattern, but the second sequence is [a, c] instead of [a, b].
     Should fail. *)
  let foo = leaf "kw" "foo" in
  let bar = leaf "kw" "bar" in
  let foo_pat : Stmatch.pattern_token =
    Concrete { text = "foo"; node_type = "kw" }
  in
  let bar_pat : Stmatch.pattern_token =
    Concrete { text = "bar"; node_type = "kw" }
  in
  let tree = nd [ id "a"; id "b"; foo; id "a"; id "c"; bar ] in
  Alcotest.(check bool)
    "non-linear siblings fails on different sequences" false
    (Option.is_some (bindings_of [ nseq "P"; foo_pat; nseq "P"; bar_pat ] tree))

let test_nonlinear_siblings_different_lengths_fail () =
  (* First P absorbs [a, b]; second P would need to absorb [a] only (then
     bar matches). Different lengths → fail. *)
  let foo = leaf "kw" "foo" in
  let bar = leaf "kw" "bar" in
  let foo_pat : Stmatch.pattern_token =
    Concrete { text = "foo"; node_type = "kw" }
  in
  let bar_pat : Stmatch.pattern_token =
    Concrete { text = "bar"; node_type = "kw" }
  in
  let tree = nd [ id "a"; id "b"; foo; id "a"; bar ] in
  Alcotest.(check bool)
    "non-linear siblings fails on different lengths" false
    (Option.is_some (bindings_of [ nseq "P"; foo_pat; nseq "P"; bar_pat ] tree))

let test_nonlinear_siblings_records_one_binding () =
  (* Two occurrences of P should produce a single Sequence binding. *)
  let foo = leaf "kw" "foo" in
  let foo_pat : Stmatch.pattern_token =
    Concrete { text = "foo"; node_type = "kw" }
  in
  let tree = nd [ id "a"; id "b"; foo; id "a"; id "b" ] in
  let bs = bindings_of [ nseq "P"; foo_pat; nseq "P" ] tree in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      let n_seqs =
        List.length
          (List.filter (function M.Sequence _ -> true | _ -> false) bs)
      in
      Alcotest.(check int) "one Sequence binding for P" 1 n_seqs

(* ----- Ambiguity: how the matcher resolves multiple valid matches. ----- *)

(* Pattern [...; a; ...; b] against [a, c, a, d, b] has two valid
   interpretations:
   1. First ... absorbs []; "a" matches first a; second ... absorbs [c, a, d].
   2. First ... absorbs [a, c]; "a" matches second a; second ... absorbs [d].
   The algorithm prefers "leftmost minimum" — Siblings defaults to 0 and
   backtracking adds more. So interpretation 1 is the one returned. *)
let test_ambiguous_leftmost_minimum () =
  let source = nd [ id "a"; id "c"; id "a"; id "d"; id "b" ] in
  let bs = bindings_of [ nseq "P"; con "a"; nseq "Q"; con "b" ] source in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check (option (list string)))
        "P absorbs nothing (leftmost minimum)" (Some [])
        (sequence_binding_texts bs "P");
      Alcotest.(check (option (list string)))
        "Q absorbs [c, a, d]"
        (Some [ "c"; "a"; "d" ])
        (sequence_binding_texts bs "Q")

(* The same source matches differently when the pattern requires the
   first wildcard to absorb more. Here the literal "c" forces P to
   absorb [a, c] (since the only "c" in source comes after an "a"). *)
let test_disambiguation_via_anchors () =
  let source = nd [ id "a"; id "c"; id "a"; id "d"; id "b" ] in
  let bs =
    (* Pattern: [P; c; a; Q; b]. The "c" anchors P at [a]. *)
    bindings_of [ nseq "P"; con "c"; con "a"; nseq "Q"; con "b" ] source
  in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check (option (list string)))
        "P absorbs [a]" (Some [ "a" ])
        (sequence_binding_texts bs "P");
      Alcotest.(check (option (list string)))
        "Q absorbs [d]" (Some [ "d" ])
        (sequence_binding_texts bs "Q")

let test_nonlinear_siblings_zero_zero () =
  (* [P; foo; P; bar] against [foo, bar] — P absorbs zero both times. *)
  let foo = leaf "kw" "foo" in
  let bar = leaf "kw" "bar" in
  let foo_pat : Stmatch.pattern_token =
    Concrete { text = "foo"; node_type = "kw" }
  in
  let bar_pat : Stmatch.pattern_token =
    Concrete { text = "bar"; node_type = "kw" }
  in
  let tree = nd [ foo; bar ] in
  Alcotest.(check bool)
    "non-linear siblings matches with zero absorbed on both sides" true
    (Option.is_some (bindings_of [ nseq "P"; foo_pat; nseq "P"; bar_pat ] tree))

(* ----- Bindings rolled back on backtracking. ----- *)

let test_bindings_rolled_back_on_backtrack () =
  (* Pattern [x; b] against nd[nd[a;b]]. Initial Subtree binds nd[a,b];
     concrete b can't pre-advance past the root, so we fail and backtrack.
     The wildcard descends, binds the inner nd[a,b], fails again, descends
     to a, then b after a matches. Final binding: x = a. *)
  let bs = bindings_of [ nx "x"; con "b" ] (nd [ nd [ id "a"; id "b" ] ]) in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check (option string))
        "x bound to a (post-backtrack), not the compound" (Some "a")
        (single_binding_text bs "x")

(* ----- Mixed bindings + match modes. ----- *)

let test_mixed_named_and_anonymous () =
  let bs =
    bindings_of [ nx "x"; sub; nx "y" ] (nd [ id "a"; id "b"; id "c" ])
  in
  match bs with
  | None -> Alcotest.fail "expected match"
  | Some bs ->
      Alcotest.(check int)
        "two named bindings, anonymous excluded" 2 (List.length bs);
      Alcotest.(check (option string))
        "x = a" (Some "a")
        (single_binding_text bs "x");
      Alcotest.(check (option string))
        "y = c" (Some "c")
        (single_binding_text bs "y")

(* ========================================================================= *)
(* Layer 4: outer loop (find_matches)                                        *)
(* ========================================================================= *)

let find_matches_of pattern tree =
  M.find_matches pattern (Test_cursor.of_tree tree)

(* ----- Single match. ----- *)

let test_find_matches_single () =
  let tree = nd [ id "a"; id "b" ] in
  let results = find_matches_of [ con "a" ] tree in
  Alcotest.(check int) "one match" 1 (List.length results)

let test_find_matches_no_match () =
  let tree = nd [ id "a"; id "b" ] in
  let results = find_matches_of [ con "z" ] tree in
  Alcotest.(check int) "no matches" 0 (List.length results)

(* ----- Multiple matches in document order. ----- *)

let test_find_matches_multiple_single_token () =
  (* Pattern [a] matches each "a" subtree in source. *)
  let tree = nd [ id "a"; id "b"; id "a"; id "c"; id "a" ] in
  let results = find_matches_of [ con "a" ] tree in
  Alcotest.(check int) "three a-matches" 3 (List.length results);
  (* The byte ranges should be in document order. *)
  let starts = List.map (fun r -> r.M.start_byte) results in
  Alcotest.(check (list int)) "start bytes ascending" [ 0; 2; 4 ] starts

let test_find_matches_multi_token_pattern () =
  (* Pattern [a; b] matches twice in nd[a, b, c, a, b]. *)
  let tree = nd [ id "a"; id "b"; id "c"; id "a"; id "b" ] in
  let results = find_matches_of [ con "a"; con "b" ] tree in
  Alcotest.(check int) "two matches" 2 (List.length results)

(* ----- Non-overlapping. ----- *)

let test_find_matches_non_overlapping () =
  (* Pattern [a; b] against nd[a, b, a, b] should find two
     non-overlapping matches (positions 0-2 and 2-4), not e.g. three
     overlapping matches. *)
  let tree = nd [ id "a"; id "b"; id "a"; id "b" ] in
  let results = find_matches_of [ con "a"; con "b" ] tree in
  Alcotest.(check int)
    "exactly two non-overlapping matches" 2 (List.length results)

(* ----- Overlapping / nested matches. ----- *)

(* A call-like structure nested inside itself: f(f(1)). With pattern
   [f ( _ )], the non-overlapping search finds only the outer call (it
   resumes past the match), while the overlapping search also finds the
   inner f(1). *)
let test_find_matches_nested () =
  let inner = nd [ id "f"; leaf "(" "("; id "1"; leaf ")" ")" ] in
  let outer = nd [ id "f"; leaf "(" "("; inner; leaf ")" ")" ] in
  let pat = [ con "f"; con_t "(" "("; sub; con_t ")" ")" ] in
  let non_overlap = M.find_matches pat (Test_cursor.of_tree outer) in
  let overlap =
    M.find_matches ~overlapping:true pat (Test_cursor.of_tree outer)
  in
  Alcotest.(check int) "non-overlapping: outer only" 1 (List.length non_overlap);
  Alcotest.(check int) "overlapping: outer + inner" 2 (List.length overlap)

(* Overlapping mode de-duplicates spans: the ancestor chain (root, ...)
   sharing the pattern's first leaf must not report the same span twice. *)
let test_find_matches_overlapping_dedup () =
  (* Single non-nested call: overlapping must still report exactly one. *)
  let call = nd [ id "f"; leaf "(" "("; id "1"; leaf ")" ")" ] in
  let tree = nd [ call ] in
  let pat = [ con "f"; con_t "(" "("; sub; con_t ")" ")" ] in
  let overlap =
    M.find_matches ~overlapping:true pat (Test_cursor.of_tree tree)
  in
  Alcotest.(check int) "no duplicate spans" 1 (List.length overlap)

(* ----- Byte ranges. ----- *)

let test_find_matches_byte_range_single_token () =
  (* The "a" in nd[a, b] is at bytes 0..1. *)
  let tree = nd [ id "a"; id "b" ] in
  match find_matches_of [ con "a" ] tree with
  | [ r ] ->
      Alcotest.(check int) "start_byte" 0 r.M.start_byte;
      Alcotest.(check int) "end_byte" 1 r.M.end_byte
  | _ -> Alcotest.fail "expected exactly one match"

let test_find_matches_byte_range_multi_token () =
  (* Pattern [a; b] against nd[a, b]: covers bytes 0..2. *)
  let tree = nd [ id "a"; id "b" ] in
  match find_matches_of [ con "a"; con "b" ] tree with
  | [ r ] ->
      Alcotest.(check int) "start_byte" 0 r.M.start_byte;
      Alcotest.(check int) "end_byte" 2 r.M.end_byte
  | _ -> Alcotest.fail "expected exactly one match"

(* ----- Bindings flow through. ----- *)

let test_find_matches_bindings_per_result () =
  (* Each match should carry its own bindings. Pattern [x] matches every
     leaf, and binds it to "x" — different cursors per match. *)
  let tree = nd [ id "a"; id "b"; id "c" ] in
  let results = find_matches_of [ nx "x" ] tree in
  (* Two interpretations: x could match the whole nd (one result) or each
     child (three results). The matcher's "advance past match" semantics
     and "leftmost minimum" mean we match the largest possible at the root
     first, advancing past it. So we get a single match for the whole nd. *)
  Alcotest.(check int)
    "single match (root matches first)" 1 (List.length results);
  match results with
  | [ r ] ->
      let leaf =
        match
          List.find_map
            (function
              | M.Single { name = "x"; cursor } -> Some cursor | _ -> None)
            r.bindings
        with
        | None -> Alcotest.fail "no binding for x"
        | Some c -> Test_cursor.leaf_text (Test_cursor.move_first_leaf c)
      in
      Alcotest.(check string) "x bound to leftmost leaf (a)" "a" leaf
  | _ -> Alcotest.fail "expected one match"

(* ----- Lazy iterator stops early. ----- *)

let test_find_matches_iter_lazy () =
  let tree = nd [ id "a"; id "b"; id "a"; id "c"; id "a" ] in
  let seq = M.find_matches_iter [ con "a" ] (Test_cursor.of_tree tree) in
  (* Take first match only — should not consume the entire tree. *)
  match seq () with
  | Seq.Nil -> Alcotest.fail "expected at least one match"
  | Seq.Cons (r, _) ->
      Alcotest.(check int) "first match at byte 0" 0 r.M.start_byte

(* ----- Single-match-only iterator demonstrates take. ----- *)

let test_find_matches_take_two () =
  let tree = nd [ id "a"; id "b"; id "a"; id "c"; id "a" ] in
  let seq = M.find_matches_iter [ con "a" ] (Test_cursor.of_tree tree) in
  let first_two = Seq.take 2 seq |> List.of_seq in
  Alcotest.(check int) "took two matches" 2 (List.length first_two);
  let starts = List.map (fun r -> r.M.start_byte) first_two in
  Alcotest.(check (list int)) "in order" [ 0; 2 ] starts

let outer_loop_tests =
  let open Alcotest in
  [
    test_case "find: single match" `Quick test_find_matches_single;
    test_case "find: no match returns empty" `Quick test_find_matches_no_match;
    test_case "find: multiple matches in document order" `Quick
      test_find_matches_multiple_single_token;
    test_case "find: multi-token pattern, multiple matches" `Quick
      test_find_matches_multi_token_pattern;
    test_case "find: non-overlapping" `Quick test_find_matches_non_overlapping;
    test_case "find: nested (overlapping)" `Quick test_find_matches_nested;
    test_case "find: overlapping de-dups spans" `Quick
      test_find_matches_overlapping_dedup;
    test_case "find: byte_range single token" `Quick
      test_find_matches_byte_range_single_token;
    test_case "find: byte_range multi-token" `Quick
      test_find_matches_byte_range_multi_token;
    test_case "find: bindings flow through" `Quick
      test_find_matches_bindings_per_result;
    test_case "find: iter is lazy" `Quick test_find_matches_iter_lazy;
    test_case "find: iter take 2" `Quick test_find_matches_take_two;
  ]

(* ========================================================================= *)
(* Layer 5: match_prefix                                                     *)
(* ========================================================================= *)

(* [match_prefix] succeeds when the source sub-tree is exhausted, returning
   the unconsumed suffix of the pattern. Intended for partial-mode element
   matching: each pattern element is consumed against a sub-cursor scoped to
   one source named child.

   In these tests, the "scoped sub-cursor" is a [Test_cursor.of_tree] over a
   hand-built fragment representing one element's worth of source. *)

let prefix_of pattern tree = M.match_prefix pattern (Test_cursor.of_tree tree)

(* Exact fit: pattern and sub-tree exhaust together. Remaining is empty. *)
let test_match_prefix_exact_fit () =
  let tree = nd [ id "a"; id "b" ] in
  match prefix_of [ con "a"; con "b" ] tree with
  | None -> Alcotest.fail "expected match"
  | Some (rem, _, _) ->
      Alcotest.(check int) "remaining empty" 0 (List.length rem)

(* Sub-tree exhausts before pattern: pattern has tokens remaining. *)
let test_match_prefix_with_remaining () =
  let tree = nd [ id "a"; id "b" ] in
  match prefix_of [ con "a"; con "b"; con "c"; con "d" ] tree with
  | None -> Alcotest.fail "expected match"
  | Some (rem, _, _) ->
      Alcotest.(check int) "two tokens remaining" 2 (List.length rem)

(* Pattern exhausts before sub-tree: not a prefix match. Must fail. *)
let test_match_prefix_pattern_too_short () =
  let tree = nd [ id "a"; id "b" ] in
  Alcotest.(check bool)
    "pattern too short, source has unconsumed content" false
    (Option.is_some (prefix_of [ con "a" ] tree))

(* Content mismatch: fails (no source-exhausted state to fall back to). *)
let test_match_prefix_mismatch_fails () =
  let tree = nd [ id "a"; id "b" ] in
  Alcotest.(check bool)
    "content mismatch fails" false
    (Option.is_some (prefix_of [ con "z" ] tree))

(* Wildcard at the boundary: the wildcard absorbs the last source subtree,
   pattern exhausts, sub-tree exhausts, remaining is empty. Verifies the
   binding is recorded. *)
let test_match_prefix_wildcard_at_end () =
  let tree = nd [ id "a"; id "b" ] in
  match prefix_of [ con "a"; nx "x" ] tree with
  | None -> Alcotest.fail "expected match"
  | Some (rem, _, bindings) ->
      Alcotest.(check int) "remaining empty" 0 (List.length rem);
      Alcotest.(check (option string))
        "x bound to b" (Some "b")
        (single_binding_text bindings "x")

(* Wildcard mid-pattern with remaining tokens: source exhausts after the
   wildcard's absorption, pattern's trailing tokens become the remaining. *)
let test_match_prefix_wildcard_with_remaining () =
  let tree = nd [ id "a"; id "b" ] in
  match prefix_of [ con "a"; nx "x"; con "c"; con "d" ] tree with
  | None -> Alcotest.fail "expected match"
  | Some (rem, _, bindings) ->
      Alcotest.(check int) "two tokens remaining" 2 (List.length rem);
      Alcotest.(check (option string))
        "x bound to b" (Some "b")
        (single_binding_text bindings "x")

(* The motivating partial-mode example: matching one element pattern out of
   a multi-element pattern against a scoped sub-tree representing one source
   element (a pair-like structure). After consuming the first element's
   worth of tokens, the matcher returns the trailing pattern (separator +
   subsequent elements) for the driver to consume next. *)
let test_match_prefix_partial_element_use_case () =
  (* Sub-tree represents one source pair like `a: 1` — three leaves. *)
  let tree = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pat =
    [
      con "a";
      con_t ":" ":";
      nx "x";
      con_t "," ",";
      con "b";
      con_t ":" ":";
      nx "y";
    ]
  in
  match prefix_of pat tree with
  | None -> Alcotest.fail "expected match"
  | Some (rem, _, bindings) ->
      (* Pattern's [a; :; $x] is consumed against the sub-tree; the trailing
         [,; b; :; $y] is returned for the next call. *)
      Alcotest.(check int)
        "4 tokens remaining (',' b ':' $y)" 4 (List.length rem);
      Alcotest.(check (option string))
        "x bound to 1" (Some "1")
        (single_binding_text bindings "x")

(* Pattern that doesn't match the sub-tree's leading content fails (not a
   prefix match — the first tokens disagree). *)
let test_match_prefix_leading_mismatch_fails () =
  let tree = nd [ id "a"; id "b" ] in
  Alcotest.(check bool)
    "leading mismatch fails" false
    (Option.is_some (prefix_of [ con "a"; con "z"; con "c" ] tree))

(* ========================================================================= *)
(* Layer 6: match_partial_at (MVP)                                           *)
(* ========================================================================= *)

(* MVP: sequential, in-order matching of pattern element prefixes against the
   source's named children. No reordering, no extras, no separator handling.

   Test fixtures are hand-built containers where each direct child is one
   "element" (a pair-shaped sub-tree). Since Test_cursor doesn't model the
   named-vs-anonymous distinction, all direct children of the container are
   treated as named children by [named_children]. *)

(* Test_cursor has no anonymous-vs-named distinction, so its leading/
   trailing anonymous-leaf helpers return [\[\]]. That makes
   [match_partial_at] always fail on these fixtures (no structural
   container delimiters). The fixture tests exercise the inner set-match
   logic, so they call [match_set_at] directly — see
   [match_partial_at]'s doc in [stmatch.mli]. *)
let partial_of pattern tree = M.match_set_at pattern (Test_cursor.of_tree tree)

(* Empty pattern + container with no children: trivially succeeds. *)
let test_partial_empty_pattern_no_children () =
  let tree = nd [] in
  Alcotest.(check bool)
    "empty pattern matches empty container" true
    (Option.is_some (partial_of [] tree))

(* Empty pattern + container with children: succeeds. An empty pattern
   declares no required elements; set semantics says "the empty set is a
   subset of any source." Any container — including non-empty ones — is
   trivially matched. *)
let test_partial_empty_pattern_with_children () =
  let tree = nd [ id "a" ] in
  Alcotest.(check bool)
    "empty pattern trivially matches any container" true
    (Option.is_some (partial_of [] tree))

(* Non-empty pattern + no children: fails (no element to consume the pattern). *)
let test_partial_pattern_no_children () =
  let tree = nd [] in
  Alcotest.(check bool)
    "non-empty pattern fails on empty container" false
    (Option.is_some (partial_of [ con "a" ] tree))

(* Single-element pattern matching a single child. *)
let test_partial_single_element () =
  let tree = nd [ id "a" ] in
  match partial_of [ con "a" ] tree with
  | None -> Alcotest.fail "expected match"
  | Some (_, bindings) ->
      Alcotest.(check int)
        "no bindings (no named wildcards)" 0 (List.length bindings)

(* Two pair-like elements, in-order match. Each element pattern consumes one
   child's worth of tokens; bindings accumulate. *)
let test_partial_two_elements_in_order () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "2" ] in
  let tree = nd [ pair_a; pair_b ] in
  let pat =
    [
      (* element 1: [a, :, $x] *)
      con "a";
      con_t ":" ":";
      nx "x";
      (* element 2: [b, :, $y] *)
      con "b";
      con_t ":" ":";
      nx "y";
    ]
  in
  match partial_of pat tree with
  | None -> Alcotest.fail "expected match"
  | Some (_, bindings) ->
      Alcotest.(check (option string))
        "x bound to 1" (Some "1")
        (single_binding_text bindings "x");
      Alcotest.(check (option string))
        "y bound to 2" (Some "2")
        (single_binding_text bindings "y")

(* First element matches, second fails: whole partial match fails. *)
let test_partial_second_element_fails () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "2" ] in
  let tree = nd [ pair_a; pair_b ] in
  let pat =
    [ con "a"; con_t ":" ":"; nx "x"; con "WRONG"; con_t ":" ":"; nx "y" ]
  in
  Alcotest.(check bool)
    "second element mismatch propagates" false
    (Option.is_some (partial_of pat tree))

(* Pattern names just one element; source has additional children that
   become extras under set semantics. *)
let test_partial_source_has_extra_child () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "2" ] in
  let tree = nd [ pair_a; pair_b ] in
  let pat = [ con "a"; con_t ":" ":"; nx "x" ] in
  match partial_of pat tree with
  | None -> Alcotest.fail "expected match (pair_b is an extra, allowed)"
  | Some (_, bindings) ->
      Alcotest.(check (option string))
        "x bound to 1; pair_b is silently tolerated as an extra" (Some "1")
        (single_binding_text bindings "x")

(* Pattern has more elements than source has children: fails. *)
let test_partial_pattern_longer_than_children () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let tree = nd [ pair_a ] in
  let pat =
    [
      con "a";
      con_t ":" ":";
      nx "x";
      (* second element has no child to match against *)
      con "b";
      con_t ":" ":";
      nx "y";
    ]
  in
  Alcotest.(check bool)
    "pattern with leftover after last child fails" false
    (Option.is_some (partial_of pat tree))

(* Reordering: pattern element for `a` matches source's pair_a even when
   the source has pair_b first. The driver tries each unused child for
   each pattern element. *)
let test_partial_reorder () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "2" ] in
  let tree = nd [ pair_b; pair_a ] in
  let pat =
    [ con "a"; con_t ":" ":"; nx "x"; con "b"; con_t ":" ":"; nx "y" ]
  in
  match partial_of pat tree with
  | None -> Alcotest.fail "expected match on reordered source"
  | Some (_, bindings) ->
      Alcotest.(check (option string))
        "x bound to 1 (matched pair_a despite source order)" (Some "1")
        (single_binding_text bindings "x");
      Alcotest.(check (option string))
        "y bound to 2" (Some "2")
        (single_binding_text bindings "y")

(* Combined: source has extras AND is reordered. *)
let test_partial_extras_and_reorder () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "2" ] in
  let pair_c = nd [ id "c"; leaf ":" ":"; id "3" ] in
  (* Source has all three, in c-b-a order. Pattern claims a and b. *)
  let tree = nd [ pair_c; pair_b; pair_a ] in
  let pat =
    [ con "a"; con_t ":" ":"; nx "x"; con "b"; con_t ":" ":"; nx "y" ]
  in
  match partial_of pat tree with
  | None -> Alcotest.fail "expected match (extras + reorder)"
  | Some (_, bindings) ->
      Alcotest.(check (option string))
        "x bound to 1" (Some "1")
        (single_binding_text bindings "x");
      Alcotest.(check (option string))
        "y bound to 2" (Some "2")
        (single_binding_text bindings "y")

(* Pattern claims an element that doesn't exist in the source: failure. *)
let test_partial_missing_required_element () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let tree = nd [ pair_a ] in
  let pat =
    [ con "a"; con_t ":" ":"; nx "x"; con "b"; con_t ":" ":"; nx "y" ]
  in
  Alcotest.(check bool)
    "pattern requires `b` but source lacks it" false
    (Option.is_some (partial_of pat tree))

(* Greedy-needs-backtrack: a leading-wildcard element could absorb any
   source child. If the greedy first choice leads to a downstream
   mismatch, backtracking is needed to find the correct assignment.

   Pattern: [$x, b, :, $y] — first element is a Subtree wildcard that
   can absorb any child; second element requires "b: $y".

   Source: [pair_b, pair_a]. Greedy first tries $x against pair_b
   (succeeds), then [b, :, $y] against pair_a (fails, "a" != "b").
   Backtracking re-tries $x against pair_a (succeeds), then
   [b, :, $y] against pair_b (succeeds). *)
let test_partial_greedy_needs_backtrack () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "2" ] in
  let tree = nd [ pair_b; pair_a ] in
  let pat = [ nx "x"; con "b"; con_t ":" ":"; nx "y" ] in
  match partial_of pat tree with
  | None ->
      Alcotest.fail "expected match — backtracking should find $x→pair_a, $y→2"
  | Some (_, bindings) ->
      (* x absorbed pair_a (compound); its leftmost leaf is "a". *)
      Alcotest.(check (option string))
        "x absorbed pair_a (leftmost leaf 'a')" (Some "a")
        (single_binding_text bindings "x");
      Alcotest.(check (option string))
        "y bound to 2 (from pair_b)" (Some "2")
        (single_binding_text bindings "y")

(* Cross-element non-linearity success: the same named wildcard appears
   in two elements; the source happens to have structurally-equal values
   bound under those names. *)
let test_partial_nonlinearity_equal () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "v" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "v" ] in
  let tree = nd [ pair_a; pair_b ] in
  (* Both elements bind $x; the source's "v" values are equal so the
     non-linearity check passes. *)
  let pat =
    [ con "a"; con_t ":" ":"; nx "x"; con "b"; con_t ":" ":"; nx "x" ]
  in
  match partial_of pat tree with
  | None -> Alcotest.fail "expected match (both $x bind to equal subtrees)"
  | Some (_, bindings) ->
      (* There may be one or two binding entries for "x" depending on
         implementation; what matters is that the match succeeded
         (cross-element equality was satisfied). *)
      Alcotest.(check (option string))
        "x bound to 'v'" (Some "v")
        (single_binding_text bindings "x")

(* Cross-element non-linearity failure: same named wildcard, but the
   source has different values, so the non-linearity check fires and
   the match fails (no backtracking assignment satisfies the
   constraint either). *)
let test_partial_nonlinearity_unequal () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "v1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "v2" ] in
  let tree = nd [ pair_a; pair_b ] in
  let pat =
    [ con "a"; con_t ":" ":"; nx "x"; con "b"; con_t ":" ":"; nx "x" ]
  in
  Alcotest.(check bool)
    "different values for $x cause failure" false
    (Option.is_some (partial_of pat tree))

(* Non-linearity + reordering: the matcher's backtracking should find
   the assignment that satisfies non-linearity even when the source
   order doesn't naively pair the equal values. *)
let test_partial_nonlinearity_with_reorder () =
  (* Source has three pairs: a → v1, b → v2, c → v1. Pattern wants two
     elements: a and c, both binding $x. Set semantics + backtracking
     should pair pattern[a] with source[pair_a] (v1) and pattern[c]
     with source[pair_c] (v1) — non-linearity satisfied. *)
  let pair_a = nd [ id "a"; leaf ":" ":"; id "v1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "v2" ] in
  let pair_c = nd [ id "c"; leaf ":" ":"; id "v1" ] in
  let tree = nd [ pair_b; pair_c; pair_a ] in
  let pat =
    [ con "a"; con_t ":" ":"; nx "x"; con "c"; con_t ":" ":"; nx "x" ]
  in
  match partial_of pat tree with
  | None ->
      Alcotest.fail "expected match (a→v1 and c→v1 satisfies non-linearity)"
  | Some (_, bindings) ->
      Alcotest.(check (option string))
        "x bound to 'v1'" (Some "v1")
        (single_binding_text bindings "x")

(* Wildcard absorbing a whole element-sized child. *)
let test_partial_wildcard_element () =
  let pair_a = nd [ id "a"; leaf ":" ":"; id "1" ] in
  let pair_b = nd [ id "b"; leaf ":" ":"; id "2" ] in
  let tree = nd [ pair_a; pair_b ] in
  (* Two Subtree wildcards, one per element. *)
  let pat = [ nx "p"; nx "q" ] in
  match partial_of pat tree with
  | None -> Alcotest.fail "expected match"
  | Some (_, bindings) ->
      (* $p absorbed pair_a (its leftmost leaf is "a"); $q absorbed pair_b. *)
      Alcotest.(check (option string))
        "p bound to first pair (leftmost leaf 'a')" (Some "a")
        (single_binding_text bindings "p");
      Alcotest.(check (option string))
        "q bound to second pair (leftmost leaf 'b')" (Some "b")
        (single_binding_text bindings "q")

(* Compile the layer-6 test list. *)
let match_partial_tests =
  let open Alcotest in
  [
    test_case "partial: empty pattern + no children" `Quick
      test_partial_empty_pattern_no_children;
    test_case "partial: empty pattern matches non-empty container" `Quick
      test_partial_empty_pattern_with_children;
    test_case "partial: pattern + no children fails" `Quick
      test_partial_pattern_no_children;
    test_case "partial: single element matches" `Quick
      test_partial_single_element;
    test_case "partial: two elements in order" `Quick
      test_partial_two_elements_in_order;
    test_case "partial: second element mismatch fails" `Quick
      test_partial_second_element_fails;
    test_case "partial: source extras are tolerated" `Quick
      test_partial_source_has_extra_child;
    test_case "partial: pattern longer than children fails" `Quick
      test_partial_pattern_longer_than_children;
    test_case "partial: wildcards absorb whole elements" `Quick
      test_partial_wildcard_element;
    test_case "partial: reorder (pattern order differs from source)" `Quick
      test_partial_reorder;
    test_case "partial: extras + reorder combined" `Quick
      test_partial_extras_and_reorder;
    test_case "partial: missing required element fails" `Quick
      test_partial_missing_required_element;
    test_case "partial: greedy needs backtrack" `Quick
      test_partial_greedy_needs_backtrack;
    test_case "partial: non-linearity (equal values)" `Quick
      test_partial_nonlinearity_equal;
    test_case "partial: non-linearity (unequal values fails)" `Quick
      test_partial_nonlinearity_unequal;
    test_case "partial: non-linearity with reordering" `Quick
      test_partial_nonlinearity_with_reorder;
  ]

(* Compile the layer-5 test list. *)
let match_prefix_tests =
  let open Alcotest in
  [
    test_case "prefix: exact fit (remaining empty)" `Quick
      test_match_prefix_exact_fit;
    test_case "prefix: source exhausts with pattern remaining" `Quick
      test_match_prefix_with_remaining;
    test_case "prefix: pattern too short fails" `Quick
      test_match_prefix_pattern_too_short;
    test_case "prefix: content mismatch fails" `Quick
      test_match_prefix_mismatch_fails;
    test_case "prefix: wildcard at end (remaining empty)" `Quick
      test_match_prefix_wildcard_at_end;
    test_case "prefix: wildcard with remaining" `Quick
      test_match_prefix_wildcard_with_remaining;
    test_case "prefix: partial-element use case (pair + remaining)" `Quick
      test_match_prefix_partial_element_use_case;
    test_case "prefix: leading-mismatch fails" `Quick
      test_match_prefix_leading_mismatch_fails;
  ]

(* Compile the layer-3 test list. *)
let binding_tests =
  let open Alcotest in
  [
    test_case "named subtree records binding" `Quick
      test_named_subtree_records_binding;
    test_case "named subtree binds compound subtree" `Quick
      test_named_subtree_binds_compound_subtree;
    test_case "anonymous subtree records no binding" `Quick
      test_anonymous_subtree_records_no_binding;
    test_case "named siblings records one" `Quick
      test_named_siblings_records_sequence_one;
    test_case "named siblings records many" `Quick
      test_named_siblings_records_sequence_many;
    test_case "named siblings records zero" `Quick
      test_named_siblings_records_sequence_zero;
    test_case "anonymous siblings records no binding" `Quick
      test_anonymous_siblings_records_no_binding;
    test_case "non-linear pattern matches equal subtrees" `Quick
      test_nonlinear_equal_subtrees_match;
    test_case "non-linear pattern fails different subtrees" `Quick
      test_nonlinear_different_subtrees_fail;
    test_case "non-linear pattern records one binding" `Quick
      test_nonlinear_one_binding_recorded;
    test_case "non-linear pattern matches compound subtrees" `Quick
      test_nonlinear_with_compound_subtrees;
    test_case "non-linear siblings matches equal sequences" `Quick
      test_nonlinear_siblings_equal_sequences;
    test_case "non-linear siblings fails different sequences" `Quick
      test_nonlinear_siblings_different_sequences_fail;
    test_case "non-linear siblings fails different lengths" `Quick
      test_nonlinear_siblings_different_lengths_fail;
    test_case "non-linear siblings records one binding" `Quick
      test_nonlinear_siblings_records_one_binding;
    test_case "non-linear siblings matches zero/zero" `Quick
      test_nonlinear_siblings_zero_zero;
    test_case "ambiguous: leftmost minimum semantics" `Quick
      test_ambiguous_leftmost_minimum;
    test_case "disambiguation via anchors" `Quick
      test_disambiguation_via_anchors;
    test_case "bindings rolled back on backtrack" `Quick
      test_bindings_rolled_back_on_backtrack;
    test_case "mixed named and anonymous wildcards" `Quick
      test_mixed_named_and_anonymous;
  ]

(* ========================================================================= *)
(* Test registration                                                         *)
(* ========================================================================= *)

let tests =
  let open Alcotest in
  [
    (* Layer 1 *)
    test_case "literal: single match" `Quick test_literal_single_match;
    test_case "literal: single mismatch" `Quick test_literal_single_mismatch;
    test_case "literal: descends to leaf" `Quick test_literal_descends_to_leaf;
    test_case "literal: sequence match" `Quick test_literal_sequence_matches;
    test_case "literal: sequence mismatch at middle" `Quick
      test_literal_sequence_mismatches_at_middle;
    test_case "literal: pattern shorter than tree" `Quick
      test_pattern_shorter_than_tree;
    test_case "literal: pattern longer than tree" `Quick
      test_pattern_longer_than_tree;
    test_case "node_type: discrimination on single leaf" `Quick
      test_node_type_discrimination_same_text;
    test_case "node_type: matching type succeeds" `Quick
      test_node_type_discrimination_matching_type;
    test_case "node_type: discrimination in sequence" `Quick
      test_node_type_discrimination_in_sequence;
    test_case "subtree wildcard: matches leaf" `Quick
      test_subtree_wildcard_matches_leaf;
    test_case "subtree wildcard: matches internal node" `Quick
      test_subtree_wildcard_matches_internal_node;
    test_case "subtree wildcard: with literals around" `Quick
      test_subtree_wildcard_with_literals_around;
    test_case "subtree wildcard: binds compound subtree" `Quick
      test_subtree_wildcard_binds_subtree;
    test_case "subtree wildcard: descends on failure" `Quick
      test_subtree_wildcard_descends_on_failure;
    test_case "siblings wildcard: zero" `Quick
      test_siblings_wildcard_matches_zero;
    test_case "siblings wildcard: one" `Quick test_siblings_wildcard_matches_one;
    test_case "siblings wildcard: many" `Quick
      test_siblings_wildcard_matches_many;
    test_case "siblings wildcard: at start" `Quick
      test_siblings_wildcard_at_start;
    test_case "siblings wildcard: at end" `Quick test_siblings_wildcard_at_end;
    test_case "siblings wildcard: only" `Quick test_siblings_wildcard_only;
    test_case "siblings wildcard: fails when anchor missing" `Quick
      test_siblings_wildcard_fails_when_anchor_missing;
    test_case "mixed: subtree then siblings" `Quick test_subtree_then_siblings;
    test_case "mixed: siblings then subtree" `Quick test_siblings_then_subtree;
    test_case "edge: empty pattern" `Quick test_empty_pattern_against_leaf;
    test_case "edge: literal text matches identifier leaf only" `Quick
      test_literal_doesnt_match_internal_node_via_first_leaf;
    test_case "edge: double siblings around literal" `Quick
      test_double_sibling_wildcards;
  ]
  @ stsearch_fixture_tests @ binding_tests @ outer_loop_tests
  @ match_prefix_tests @ match_partial_tests
