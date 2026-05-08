(** Smoke tests for the [Grammar_metadata] pipeline.

    These verify that the build pipeline successfully extracts and
    parses each grammar's [node-types.json], and that the structural
    criterion picks up the wrapper types we expect. They do not
    exercise any consumer of the metadata — that lives in whatever
    matcher architecture eventually consumes the pipeline. *)

open Diffract

(** Every registered language should expose a non-empty wrapper set;
    the metadata files are versioned and the parser should always
    succeed. A failure here means the build pipeline didn't deliver
    a usable [node-types.json], or [Grammar_metadata]'s extraction
    rejected everything. *)
let test_every_language_has_wrappers () =
  List.iter
    (fun lang ->
      let wrappers = Grammar_metadata.list_shape_wrappers ~language:lang in
      Alcotest.(check bool)
        (Printf.sprintf "%s: at least one wrapper extracted" lang)
        true
        (wrappers <> []))
    (Grammar_metadata.all_languages ())

(** The structural criterion should pick out file-level and list
    container wrappers we expect to be present in every grammar
    that has them. *)
let test_known_wrappers_present () =
  let cases =
    [
      ("typescript", "program");
      ("tsx", "program");
      ("kotlin", "source_file");
      ("kotlin", "import_list");
      ("php", "program");
      ("scala", "compilation_unit");
    ]
  in
  List.iter
    (fun (lang, t) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s: %s is wrapper" lang t)
        true
        (Grammar_metadata.is_list_shape_wrapper ~language:lang ~node_type:t))
    cases

(** Structured nodes (those that carry meaning beyond a container
    role) should not be flagged as wrappers. We check a couple
    obvious cases — these have field-named children, which excludes
    them by the criterion. *)
let test_structured_nodes_not_wrappers () =
  let cases =
    [
      ("typescript", "if_statement");
      ("typescript", "for_statement");
      ("kotlin", "if_expression");
      ("kotlin", "function_declaration");
    ]
  in
  List.iter
    (fun (lang, t) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s: %s is NOT wrapper" lang t)
        false
        (Grammar_metadata.is_list_shape_wrapper ~language:lang ~node_type:t))
    cases

(** Unknown languages return an empty wrapper list — the runtime
    should degrade gracefully rather than crash on a typo. *)
let test_unknown_language_empty () =
  let wrappers =
    Grammar_metadata.list_shape_wrappers ~language:"esperanto"
  in
  Alcotest.(check (list string))
    "unknown language returns empty wrapper list" [] wrappers

(* ===================================================================== *)
(* DEL definition tests                                                  *)
(* ===================================================================== *)

(** Every registered language should have non-empty bracket pairs.
    The universal triplet ((,)), ([,]), ({,}) is included
    unconditionally; an empty result indicates a broken extractor. *)
let test_brackets_per_language () =
  List.iter
    (fun lang ->
      let def = Grammar_metadata.del_definition ~language:lang in
      Alcotest.(check bool)
        (Printf.sprintf "%s: has bracket pairs" lang)
        true
        (def.bracket_pairs <> []);
      Alcotest.(check bool)
        (Printf.sprintf "%s: includes (,)" lang)
        true
        (List.mem ("(", ")") def.bracket_pairs);
      Alcotest.(check bool)
        (Printf.sprintf "%s: includes [,]" lang)
        true
        (List.mem ("[", "]") def.bracket_pairs);
      Alcotest.(check bool)
        (Printf.sprintf "%s: includes {,}" lang)
        true
        (List.mem ("{", "}") def.bracket_pairs))
    (Grammar_metadata.all_languages ())

(** Languages with generic syntax (TypeScript, Kotlin) get <,> in
    their bracket pairs; PHP and Scala don't. *)
let test_angle_brackets_present_only_where_used () =
  let has_angle lang =
    List.mem ("<", ">")
      (Grammar_metadata.del_definition ~language:lang).bracket_pairs
  in
  Alcotest.(check bool) "typescript: has <,>" true (has_angle "typescript");
  Alcotest.(check bool) "tsx: has <,>" true (has_angle "tsx");
  Alcotest.(check bool) "kotlin: has <,>" true (has_angle "kotlin");
  Alcotest.(check bool) "php: has <,>" false (has_angle "php");
  Alcotest.(check bool) "scala: has <,>" false (has_angle "scala")

(** Each language should expose at least one string definition,
    coming either from auto-derivation (TypeScript, TSX) or from
    per-language extensions (Kotlin, PHP, Scala). *)
let test_strings_per_language () =
  List.iter
    (fun lang ->
      let def = Grammar_metadata.del_definition ~language:lang in
      Alcotest.(check bool)
        (Printf.sprintf "%s: has at least one string definition" lang)
        true
        (def.string_defs <> []))
    (Grammar_metadata.all_languages ())

(** Spot-check specific string definitions per language. *)
let test_known_strings_present () =
  let has_string ~language ~opener =
    let def = Grammar_metadata.del_definition ~language in
    List.exists
      (fun (s : Grammar_metadata.string_def) -> s.opener = opener)
      def.string_defs
  in
  Alcotest.(check bool)
    "typescript: has \"" true
    (has_string ~language:"typescript" ~opener:"\"");
  Alcotest.(check bool)
    "typescript: has '" true
    (has_string ~language:"typescript" ~opener:"'");
  Alcotest.(check bool)
    "typescript: has `" true
    (has_string ~language:"typescript" ~opener:"`");
  Alcotest.(check bool)
    "kotlin: has \" (via extension)" true
    (has_string ~language:"kotlin" ~opener:"\"");
  Alcotest.(check bool)
    "kotlin: has \"\"\" (via extension)" true
    (has_string ~language:"kotlin" ~opener:"\"\"\"");
  Alcotest.(check bool)
    "php: has \" (via extension)" true
    (has_string ~language:"php" ~opener:"\"");
  Alcotest.(check bool)
    "php: has ' (via extension)" true
    (has_string ~language:"php" ~opener:"'")

(** Line comments. // for all C-family languages we support; PHP
    additionally has # via extension. *)
let test_line_comments () =
  let has lang marker =
    List.mem marker
      (Grammar_metadata.del_definition ~language:lang).line_comments
  in
  Alcotest.(check bool) "typescript: //" true (has "typescript" "//");
  Alcotest.(check bool) "kotlin: //" true (has "kotlin" "//");
  Alcotest.(check bool) "php: //" true (has "php" "//");
  Alcotest.(check bool) "php: # (via extension)" true (has "php" "#");
  Alcotest.(check bool) "scala: //" true (has "scala" "//")

(** Block comments. /* ... */ for all C-family languages.
    Kotlin's is from extension (multiline_comment external). *)
let test_block_comments () =
  let has lang pair =
    List.mem pair
      (Grammar_metadata.del_definition ~language:lang).block_comments
  in
  Alcotest.(check bool)
    "typescript: /* */" true
    (has "typescript" ("/*", "*/"));
  Alcotest.(check bool)
    "kotlin: /* */ (via extension)" true
    (has "kotlin" ("/*", "*/"));
  Alcotest.(check bool) "php: /* */" true (has "php" ("/*", "*/"));
  Alcotest.(check bool) "scala: /* */" true (has "scala" ("/*", "*/"))

(** Unknown languages return an empty DEL definition. *)
let test_unknown_language_empty_del () =
  let def = Grammar_metadata.del_definition ~language:"esperanto" in
  Alcotest.(check bool)
    "unknown language: empty bracket_pairs" true (def.bracket_pairs = []);
  Alcotest.(check bool)
    "unknown language: empty string_defs" true (def.string_defs = []);
  Alcotest.(check bool)
    "unknown language: empty line_comments" true (def.line_comments = []);
  Alcotest.(check bool)
    "unknown language: empty block_comments" true (def.block_comments = [])

let tests =
  [
    Alcotest.test_case "every language has wrappers" `Quick
      test_every_language_has_wrappers;
    Alcotest.test_case "known wrappers present" `Quick
      test_known_wrappers_present;
    Alcotest.test_case "structured nodes not wrappers" `Quick
      test_structured_nodes_not_wrappers;
    Alcotest.test_case "unknown language returns empty" `Quick
      test_unknown_language_empty;
    Alcotest.test_case "brackets per language" `Quick
      test_brackets_per_language;
    Alcotest.test_case "angle brackets only where used" `Quick
      test_angle_brackets_present_only_where_used;
    Alcotest.test_case "strings per language" `Quick
      test_strings_per_language;
    Alcotest.test_case "known strings present" `Quick
      test_known_strings_present;
    Alcotest.test_case "line comments" `Quick test_line_comments;
    Alcotest.test_case "block comments" `Quick test_block_comments;
    Alcotest.test_case "unknown language empty del" `Quick
      test_unknown_language_empty_del;
  ]
