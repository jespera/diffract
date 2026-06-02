(** Main test runner *)

let () =
  Alcotest.run "diffract"
    [
      ("Node", Test_node.tests);
      ("Tree diff", Test_tree_diff.tests);
      ("Grammar metadata", Test_grammar_metadata.tests);
      ("Raw vs Wrapped", Test_raw_vs_wrapped.tests);
      ("Cursor", Test_cursor.tests);
      ("STMatch", Test_stmatch.tests);
      ("Tree_sitter_cursor", Test_tree_sitter_cursor.tests);
      ("Tokenize", Test_tokenize.tests);
      ("Matcher", Test_matcher.tests);
    ]
