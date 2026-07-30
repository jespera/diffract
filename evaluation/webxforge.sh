#!/usr/bin/env bash
# sillsdev/web-xforge DestroyRef migration (PR #3066) — TypeScript/Angular.
# The committer's GritQL codemod is in the commit itself (.grit/patterns/),
# giving machine-readable rule ground truth plus documented manual edits.
# PR: https://github.com/sillsdev/web-xforge/pull/3066
NAME=webxforge
REPO=https://github.com/sillsdev/web-xforge
BEFORE_SHA=28e2f0c5463a475108407cf7a35210a69bc80b1f
AFTER_SHA=b6911f236779ce3318fce926e6526aebf630035e
GLOB='*.ts'
LANG_NAME=typescript
EXPECTED_PAIRS=80
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
