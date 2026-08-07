#!/usr/bin/env bash
# SAP/spartacus "Upgrade to Angular 19" (PR #19772) — TypeScript/Angular.
# The official Angular v19 update schematic inserts `standalone: false,` into
# every NgModule-declared `@Component/@Directive/@Pipe({...})` decorator
# literal. Machine-readable ground truth: the migration's source is
# `packages/core/schematics/migrations/explicit-standalone-flag` in
# angular/angular at tag 19.0.0 (the migration was removed from master, so
# cite the tag). PR: https://github.com/SAP/spartacus/pull/19772
#
# The insertion corpus. Every diff line in the derivation slice is literally
# `+  standalone: false,` — 91 insertions across 64 files, zero deletions,
# zero ride-alongs, no pre-existing `standalone` flags, no reflowed sites.
# Conceptual ground truth is three container-anchored insertion rules (one
# per decorator kind). Until summarize can propose insertion rules this
# corpus is a known-fail baseline by design: 0 rules, everything residual.
#
# Sliced deliberately. The full commit touches 916 .ts files with 1,205
# insertions (990 @Component / 93 @Directive / 122 @Pipe), 37 sites that also
# reflow a one-line decorator to multi-line, and ~330 manual ng19-compat
# lines concentrated in projects/schematics. feature-libs/cart is the largest
# perfectly-pure slice; feature-libs/order is held out as the generalization
# check — 83 insertions across 63 files, 3 of them one-line-decorator reflow
# sites concentrated in 2 spec files. Those 2 files are the documented
# holdout regressions: at a one-line decorator the insertion rule's splice is
# inline (there is no line boundary to render against), which can neither
# reproduce prettier's reflow nor synthesize the missing separator. The
# reflow shape is the v2 boundary; the remaining 61 files reconstruct
# byte-exactly.
#
# Guard note: the schematic is conditional (insert only where `standalone` is
# absent) — a negative condition diffract cannot state. It never bites in
# this commit: the only pre-existing `standalone:` occurrences in the changed
# files' before-state are string-literal fixtures in 4 projects/schematics
# spec files, outside both slices and not insert sites themselves.
NAME=spartacus-standalone
REPO=https://github.com/SAP/spartacus
BEFORE_SHA=6c6f0003cf10109bbfb3ad0777e40b00682126c6
AFTER_SHA=53674c5745b8dbb7a3cd9fdfcaa7246636e9713f
GLOB='feature-libs/cart/*.ts'
FILE_GLOB='*.ts'
HOLDOUT_GLOB='feature-libs/order/*.ts'
LANG_NAME=typescript
EXPECTED_PAIRS=64
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
