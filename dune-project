(lang dune 3.0)
(name diffract)
(generate_opam_files true)

(package
 (name diffract)
 (synopsis "Structural diff and pattern inference for source code using tree-sitter")
 (description "A tool for computing structural diffs between source files, inferring transformation patterns from examples, and summarizing code changes using tree-sitter.")
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.0))
  (cmdliner (>= 1.2))
  ctypes
  ctypes-foreign
  (alcotest :with-test)
  (bechamel :with-test)
  (yojson :with-test)))
