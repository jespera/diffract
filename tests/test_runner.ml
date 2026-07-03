(** Main test runner *)

let () =
  Alcotest.run "diffract"
    [
      ("File scan", Test_file_scan.tests);
      ("Node", Test_node.tests);
      ("Tree diff", Test_tree_diff.tests);
      ("Leaf metric", Test_leaf_metric.tests);
      ("Grammar metadata", Test_grammar_metadata.tests);
      ("Raw vs Wrapped", Test_raw_vs_wrapped.tests);
      ("Cursor", Test_cursor.tests);
      ("STMatch", Test_stmatch.tests);
      ("Tree_sitter_cursor", Test_tree_sitter_cursor.tests);
      ("Tokenize", Test_tokenize.tests);
      ("Matcher", Test_matcher.tests);
      ("Cs_pattern", Test_cs_pattern.tests);
      ("Change summary", Test_change_summary.tests);
    ]
