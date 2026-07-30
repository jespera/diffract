#!/usr/bin/env bash
# Drupal core's PHPUnit annotations→attributes conversion (Rector-driven,
# drupal.org issue #3446693) — PHP. Known-fail stress corpus: the edits live
# in docblock comment tokens and pure attribute insertions, both outside
# summarize's current reach. Slow (~3 min); deliberately not in all.sh.
# Issue (drupal.org is the discussion home; the GitHub repo is a mirror):
#   https://www.drupal.org/project/drupal/issues/3446693
NAME=drupal-attr
REPO=https://github.com/drupal/drupal
BEFORE_SHA=60627bbd140b814fd3bfd20cba5d8d25273fa96d
AFTER_SHA=6221eb3aa2809f8a3b364de96dde8401ca7f0e4a
GLOB='*.php'
LANG_NAME=php
EXTRA_FLAGS=--ignore-formatting
EXPECTED_PAIRS=83
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
