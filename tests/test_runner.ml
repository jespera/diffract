(** Main test runner *)

let () =
  Alcotest.run "diffract"
    [
      ("Node", Test_node.tests);
      ("Match", Test_match.tests);
      ("Match (Kotlin)", Test_match.kotlin_tests);
      ("Match (PHP)", Test_match.php_tests);
      ("Match (Scala)", Test_match.scala_tests);
      ("Match (Field)", Test_match.field_tests);
      ("Match (Ellipsis)", Test_match.ellipsis_tests);
      ("Transform", Test_match.transform_tests);
      ("Expansion", Test_match.expansion_tests);
      ("Semantics", Test_match.semantics_tests);
      ("Conjunctive", Test_match.conjunctive_tests);
      ("Match (Comments)", Test_match.comment_tests);
      ("Match (Bare-sequence)", Test_match.bare_sequence_tests);
      ("Match (Separator-aware deletion)",
       Test_match.separator_deletion_tests);
      ("Tree diff", Test_tree_diff.tests);
      ("Grammar metadata", Test_grammar_metadata.tests);
    ]
