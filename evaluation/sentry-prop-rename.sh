#!/usr/bin/env bash
# getsentry/sentry "codemod(button-variant)" (PR #114521) — TSX. JSX prop
# rename `priority=` → `variant=` on Button/LinkButton, one line out / one
# line in per site. Calibration corpus. Ground truth (established by hand
# rules + apply, 2026-07-28): a metavar rename rule per element form, a
# `priority="default"` → `variant="secondary"` value-remap sub-rule, an
# object-literal form (`priority: v` in props objects), and one deliberate
# exclusion (`<Confirm priority=...>` keeps its prop — over-fire bait for
# the gate). Inferred from the tagged 32-PR series and the pristine diff;
# the codemod source itself is in a private repo
# (getsentry/design-engineering). See docs/summarize-corpus-candidates.md
# for the series anatomy and the manual-pass follow-up candidates.
# PR: https://github.com/getsentry/sentry/pull/114521
NAME=sentry-prop-rename
REPO=https://github.com/getsentry/sentry
BEFORE_SHA=6ae1d2974353e7365212af5d172f3ac2edf39ec0
AFTER_SHA=19b35c757fe2f1481c61c710a95898e8fbaf414d
GLOB='*.tsx'
LANG_NAME=tsx
EXPECTED_PAIRS=36
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
