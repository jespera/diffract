#!/usr/bin/env bash
# symfony/symfony "Leverage str_starts_with(), str_ends_with() and
# str_contains()" — PHP. strpos/substr comparisons → str_contains /
# str_starts_with / str_ends_with, exactly one line out / one line in per
# site. Calibration corpus and the PHP counterweight to the known-fail
# drupal-attr one. Ground truth = 3-4 metavar rules (one per target
# function, with negated variants). The commit's one non-.php file (a
# Resources/bin script) is excluded by the glob. See
# docs/summarize-corpus-candidates.md.
# PR: https://github.com/symfony/symfony/pull/44506
NAME=symfony-str
REPO=https://github.com/symfony/symfony
BEFORE_SHA=6ab662b0a8d5deabe03920a6dc0c7cbdc50a8d21
AFTER_SHA=bbe96c7d7285ea6591c056182615a28e66375ec0
GLOB='*.php'
LANG_NAME=php
EXPECTED_PAIRS=94
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
