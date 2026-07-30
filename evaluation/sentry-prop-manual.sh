#!/usr/bin/env bash
# getsentry/sentry PR #114731 — the MANUAL pass after the button-variant
# codemod (contrast corpus to sentry-prop-rename.sh, the automated slice).
# Same `priority=` → `variant=` rename, hand-applied, plus a documented
# ride-along: `playPausePriority` → `playPauseVariant` prop + type edit on
# ReplayPreviewPlayer. Decomposition ground truth: the rename should factor
# as a rule; the ride-along should surface separately (small rule or
# residual). See docs/summarize-corpus-candidates.md for the series anatomy.
# PR: https://github.com/getsentry/sentry/pull/114731
NAME=sentry-prop-manual
REPO=https://github.com/getsentry/sentry
BEFORE_SHA=a1d7ade4da448bb6a9afff9afa7dbb19973a44ab
AFTER_SHA=09cf9c1dcf314f4b8a55c6d7d4896c4bbf993e60
GLOB='*.tsx'
LANG_NAME=tsx
EXPECTED_PAIRS=42
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
