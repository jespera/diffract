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
  ]
