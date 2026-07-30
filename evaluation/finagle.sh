#!/usr/bin/env bash
# twitter/finagle "remove Procedure syntax using scalafix" — Scala. Scalafix
# ProcedureSyntax over the main source tree: `def f(...) { ... }` →
# `def f(...): Unit = { ... }`. 61 files, +104/−104, zero ride-along.
# Calibration corpus: ground truth = exactly one declaration-anchored rule.
# Also the first Scala corpus in the harness (grammar registered but
# previously barely exercised). Sibling commit on the test tree:
# 1a1af42d31c986ade61f6971b18cbc767f680b2e (64 files). See
# docs/summarize-corpus-candidates.md.
# Commit (no PR — pre-dates finagle's GitHub-PR workflow):
#   https://github.com/twitter/finagle/commit/04f4281fc170990b1be475fc287a03c074c7de17
NAME=finagle
REPO=https://github.com/twitter/finagle
BEFORE_SHA=26a2c3bd621aea1590f8472371cea31a5c8afc38
AFTER_SHA=04f4281fc170990b1be475fc287a03c074c7de17
GLOB='*.scala'
LANG_NAME=scala
EXPECTED_PAIRS=61
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
