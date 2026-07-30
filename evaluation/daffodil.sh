#!/usr/bin/env bash
# apache/daffodil "Scalafix ProcedureSyntax" — Scala. The same rewrite as
# finagle.sh (`def f(...) { ... }` → `def f(...): Unit = { ... }`) applied
# across a whole codebase in one commit: 312 .scala files, ~5x finagle.
# Scale corpus: same ground truth (one rule + its curried/abstract-def
# variants), so any divergence from finagle's rule shape is a
# size-dependence signal, and the runtime tracks how summarize scales in
# file count. Slow (~3 min/format), so not in all.sh. The corpus that
# motivated the dendrogram bucket cap: ~4500 genuine def-level sites in one
# root-type bucket made the pairwise merge run for hours until clustering
# switched to sampling (discovery from a capped prefix, support from the
# gate). See docs/summarize-corpus-candidates.md.
# PR: https://github.com/apache/daffodil/pull/388
NAME=daffodil
REPO=https://github.com/apache/daffodil
BEFORE_SHA=03a2a5b5e16885cd58812f86a5a8e4aa0e2dbe91
AFTER_SHA=c9ca5d6665d2d17fe637b13595a8b244584fd977
GLOB='*.scala'
LANG_NAME=scala
EXPECTED_PAIRS=312
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
