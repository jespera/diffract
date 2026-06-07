(** Smoke tests for the [Grammar_metadata] pipeline.

    These verify that the build pipeline successfully extracts and parses each
    grammar's [grammar.json] into DEL definitions (bracket pairs, string
    literals, comments). They do not exercise any consumer of the metadata —
    that lives in whatever matcher architecture eventually consumes the
    pipeline. *)

open Diffract

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
        true (def.bracket_pairs <> []);
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

(** Languages with generic syntax (TypeScript, Kotlin) get <,> in their bracket
    pairs; PHP and Scala don't. *)
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

(** Each language should expose at least one string definition, coming either
    from auto-derivation (TypeScript, TSX) or from per-language extensions
    (Kotlin, PHP, Scala). *)
let test_strings_per_language () =
  List.iter
    (fun lang ->
      let def = Grammar_metadata.del_definition ~language:lang in
      Alcotest.(check bool)
        (Printf.sprintf "%s: has at least one string definition" lang)
        true (def.string_defs <> []))
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
    "kotlin: has $\" multi-dollar variant" true
    (has_string ~language:"kotlin" ~opener:"$\"");
  Alcotest.(check bool)
    "kotlin: has $$\" multi-dollar variant" true
    (has_string ~language:"kotlin" ~opener:"$$\"");
  Alcotest.(check bool)
    "kotlin: has $$$\" multi-dollar variant" true
    (has_string ~language:"kotlin" ~opener:"$$$\"");
  Alcotest.(check bool)
    "kotlin: has $$$$\" multi-dollar variant" true
    (has_string ~language:"kotlin" ~opener:"$$$$\"");
  Alcotest.(check bool)
    "kotlin: has $\"\"\" multi-dollar triple-quoted" true
    (has_string ~language:"kotlin" ~opener:"$\"\"\"");
  Alcotest.(check bool)
    "kotlin: has $$\"\"\" multi-dollar triple-quoted" true
    (has_string ~language:"kotlin" ~opener:"$$\"\"\"");
  Alcotest.(check bool)
    "php: has \" (via extension)" true
    (has_string ~language:"php" ~opener:"\"");
  Alcotest.(check bool)
    "php: has ' (via extension)" true
    (has_string ~language:"php" ~opener:"'")

(** Line comments. // for all C-family languages we support; PHP additionally
    has # via extension. *)
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

(** Block comments. /* ... */ for all C-family languages. Kotlin's is from
    extension (multiline_comment external). *)
let test_block_comments () =
  let has lang pair =
    List.mem pair
      (Grammar_metadata.del_definition ~language:lang).block_comments
  in
  Alcotest.(check bool) "typescript: /* */" true (has "typescript" ("/*", "*/"));
  Alcotest.(check bool)
    "kotlin: /* */ (via extension)" true
    (has "kotlin" ("/*", "*/"));
  Alcotest.(check bool) "php: /* */" true (has "php" ("/*", "*/"));
  Alcotest.(check bool) "scala: /* */" true (has "scala" ("/*", "*/"))

(* Longer string-opener variants come before shorter ones in the
   string_defs list, so a longest-match lexer tries them in list
   order without sorting. For Kotlin: a two-dollar string opener
   must come before the one-dollar opener, which must come before
   the bare double-quote opener, so the lexer recognises a
   two-dollar string as that rather than as a single dollar
   followed by a one-dollar string. *)
let test_kotlin_string_ordering_longest_first () =
  let strs = (Grammar_metadata.del_definition ~language:"kotlin").string_defs in
  let openers =
    List.map (fun (s : Grammar_metadata.string_def) -> s.opener) strs
  in
  let position opener =
    let rec find i = function
      | [] -> -1
      | x :: _ when x = opener -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 openers
  in
  let p_basic = position "\"" in
  let p_dollar = position "$\"" in
  let p_dolldoll = position "$$\"" in
  Alcotest.(check bool)
    "$\" before \" (longer prefix earlier)" true
    (p_dollar >= 0 && p_basic >= 0 && p_dollar < p_basic);
  Alcotest.(check bool)
    "$$\" before $\"" true
    (p_dolldoll >= 0 && p_dollar >= 0 && p_dolldoll < p_dollar)

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
    Alcotest.test_case "brackets per language" `Quick test_brackets_per_language;
    Alcotest.test_case "angle brackets only where used" `Quick
      test_angle_brackets_present_only_where_used;
    Alcotest.test_case "strings per language" `Quick test_strings_per_language;
    Alcotest.test_case "known strings present" `Quick test_known_strings_present;
    Alcotest.test_case "line comments" `Quick test_line_comments;
    Alcotest.test_case "block comments" `Quick test_block_comments;
    Alcotest.test_case "kotlin string ordering longest-first" `Quick
      test_kotlin_string_ordering_longest_first;
    Alcotest.test_case "unknown language empty del" `Quick
      test_unknown_language_empty_del;
  ]
