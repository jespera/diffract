#!/usr/bin/env bash
# DuckDuckGo Android's AndroidX migration — Kotlin, one mechanical commit
# ("Feature/migrate to androidx (#394)", Nov 2018) plus real ride-along edits.
# PR: https://github.com/duckduckgo/Android/pull/394
NAME=androidx
REPO=https://github.com/duckduckgo/Android
BEFORE_SHA=bfb88afa3f46878be8d1db180911e501818b1450
AFTER_SHA=15db73acfa8eb1755b90f37093c85697a77db103
GLOB='*.kt'
LANG_NAME=kotlin
EXTRA_FLAGS=--ignore-formatting
EXPECTED_PAIRS=143
source "$(dirname "${BASH_SOURCE[0]}")/common.sh"
