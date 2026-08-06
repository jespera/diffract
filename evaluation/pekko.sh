#!/usr/bin/env bash
# apache/pekko "Rename akka package to org.apache.pekko" — Scala. The package
# rename at the root of the Akka→Pekko fork: every source file moves from
# .../scala/akka/... to .../scala/org/apache/pekko/..., its package clause
# follows, and its imports are rewritten.
#
# The rename corpus. Every in-scope file here is *renamed*, so without the
# manifest there is nothing to summarize at all: path-equality pairing sees 131
# unrelated deletions and 131 unrelated additions, produces zero rules, and
# emits 262 whole-file /dev/null residuals. It is the corpus that justifies
# `--pairs` — and, since RENAMES=1 materializes through the shipped extractor,
# the one that keeps scripts/diffract-checkout.sh exercised.
#
# Sliced deliberately. The full commit is 3,474 files (2,622 renamed — 2,292 of
# them Scala, plus 325 Java the harness has no grammar for) and far past what
# summarize can hold in memory at once. Because the rename is uniform, one
# module shows the same rules for a fraction of the cost: akka-cluster's 131
# files exercise every guise of it (package clause, bare identifier, import,
# doc comment, string literal). akka-distributed-data is held out as the
# generalization check — same commit, never seen during derivation.
#
# Expressiveness note. The conceptual ground truth is one rename, `akka` →
# `org.apache.pekko`, but the changed text lives *below* the leaf (inside
# identifiers, string literals and doc comments), which node-granular
# anti-unification cannot generalize over. Hence the residual tail: it is a
# known expressiveness gap, not noise.
#
# The two known holdout regressions are diagnosed, not mysterious. The real
# codemod rewrites imports in two ways depending on the file: fully qualified
# (`import org.apache.pekko.actor.ActorRef`) where there is no alias, and
# short-form under an inserted `import org.apache.pekko` anchor line
# (`import pekko.actor.ActorRef`) where there is. Only the first is expressible
# as a rule today — the second is one insertion plus N shortenings, a
# conjunctive insert the pipeline cannot anchor — so R3 learns the qualified
# form and expands imports that the holdout wanted left short. It overshoots by
# a handful of bytes on 2 of 57 files. A *third* regression would be new
# information; these two are the insert-anchoring gap.
#
# Commit (part of the fork's initial rename campaign, no single PR):
#   https://github.com/apache/pekko/commit/f84e8db3cbdbc05743cb06a2d4264d1a7ce01b96
NAME=pekko
REPO=https://github.com/apache/pekko
BEFORE_SHA=494d62515ec523119b080718820f2817ac5097b6
AFTER_SHA=f84e8db3cbdbc05743cb06a2d4264d1a7ce01b96
GLOB='akka-cluster/*.scala'
FILE_GLOB='*.scala'
HOLDOUT_GLOB='akka-distributed-data/*.scala'
LANG_NAME=scala
RENAMES=1
EXPECTED_PAIRS=131
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
