(lang dune 3.0)
(name diffract)
(generate_opam_files true)

(maintainers "Jesper Andersen")
(authors "Jesper Andersen")
(homepage "https://github.com/jespera/diffract")
(bug_reports "https://github.com/jespera/diffract/issues")
(source (github jespera/diffract))

(package
 (name diffract)
 (license GPL-3.0-or-later)
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
  (bechamel-notty :with-test)
  (notty :with-test)
  (yojson :with-test)))
