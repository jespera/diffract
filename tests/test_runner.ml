(** Main test runner *)

let () =
  Alcotest.run "diffract" [
    ("Node", Test_node.tests);
    ("Diff", Test_diff.tests);
    ("Antiunify", Test_antiunify.tests);
    ("Match", Test_match.tests);
    ("Match (Kotlin)", Test_match.kotlin_tests);
    ("Match (PHP)", Test_match.php_tests);
    ("Match (Field)", Test_match.field_tests);
    ("Match (Ellipsis)", Test_match.ellipsis_tests);
  ]
