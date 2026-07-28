# Shared driver for the per-corpus evaluation scripts. Not executable on its
# own: a corpus script defines the provenance variables below and sources this
# file, which then fetches the corpus (once), runs `summarize`, and prints a
# single metrics line to stdout. Everything else (timings, revision, progress)
# goes to stderr, so `./evaluation/<corpus>.sh > evaluation/<corpus>.baseline`
# followed by `git diff` is the whole comparison workflow.
#
# Required variables:
#   NAME            corpus name (directory under _work/)
#   REPO            public git URL
#   BEFORE_SHA      full 40-char commit sha (parent)
#   AFTER_SHA       full 40-char commit sha (the codemod commit)
#   GLOB            pathspec glob for in-scope files, e.g. '*.kt'
#   LANG_NAME       summarize -l argument
#   EXPECTED_PAIRS  number of modified in-scope file pairs (sanity check)
# Optional:
#   EXTRA_FLAGS     extra summarize flags, e.g. --ignore-formatting

set -euo pipefail

EVAL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(dirname "$EVAL_DIR")"
WORK="$EVAL_DIR/_work/$NAME"
EXTRA_FLAGS="${EXTRA_FLAGS:-}"

note() { echo "[$NAME] $*" >&2; }

# ── materialize before/ and after/ from the pinned commit pair ──────────
# Only the modified in-scope pairs are checked out (never the whole tree),
# so the corpus is exactly the advertised file set.
if [ ! -d "$WORK/before" ] || [ ! -d "$WORK/after" ]; then
  note "fetching $REPO"
  rm -rf "$WORK"
  mkdir -p "$WORK/repo"
  git -C "$WORK/repo" init -q
  git -C "$WORK/repo" fetch -q --depth 1 "$REPO" "$BEFORE_SHA" "$AFTER_SHA"
  git -C "$WORK/repo" diff --name-only --diff-filter=M \
    "$BEFORE_SHA" "$AFTER_SHA" -- "$GLOB" > "$WORK/pairs.txt"
  while IFS= read -r f; do
    mkdir -p "$WORK/before/$(dirname "$f")" "$WORK/after/$(dirname "$f")"
    git -C "$WORK/repo" show "$BEFORE_SHA:$f" > "$WORK/before/$f"
    git -C "$WORK/repo" show "$AFTER_SHA:$f" > "$WORK/after/$f"
  done < "$WORK/pairs.txt"
  note "materialized $(wc -l < "$WORK/pairs.txt") modified pair(s)"
fi

PAIRS=$(wc -l < "$WORK/pairs.txt")
if [ "$PAIRS" -ne "$EXPECTED_PAIRS" ]; then
  note "ERROR: expected $EXPECTED_PAIRS modified pairs, got $PAIRS"
  exit 1
fi

# ── build and run summarize ──────────────────────────────────────────────
note "building"
(cd "$ROOT" && dune build bin/main.exe) >&2
BIN="$ROOT/_build/default/bin/main.exe"

rev="$(git -C "$ROOT" rev-parse --short HEAD)"
# Only build inputs matter for reproducibility (the repo root carries
# permanently-untracked corpus directories that would otherwise always trip
# this).
if [ -n "$(git -C "$ROOT" status --porcelain -- lib bin grammars dune-project)" ]; then
  note "WARNING: worktree dirty at $rev — baseline not reproducible from history"
else
  note "revision $rev (clean)"
fi

run_summarize() { # $1 = output format, $2 = output file
  local t0 t1
  t0=$(date +%s)
  "$BIN" summarize "$WORK/before" "$WORK/after" \
    -l "$LANG_NAME" -i "$GLOB" $EXTRA_FLAGS --format "$1" > "$2"
  t1=$(date +%s)
  note "summarize --format $1: $((t1 - t0))s"
}

run_summarize text "$WORK/summary.txt"
run_summarize text-minimal "$WORK/summary.min.txt"

# ── metrics ──────────────────────────────────────────────────────────────
RULES=$(grep -c '^# rule ' "$WORK/summary.txt" || true)
RESIDUALS=$(grep -c '^# residual' "$WORK/summary.txt" || true)
RESIDUAL_FILES=$( (grep '^--- a/' "$WORK/summary.txt" || true) | sort -u | wc -l)
FACTORED=$((PAIRS - RESIDUAL_FILES))

# Description length: rendered-summary bytes (minimal render) over raw-diff
# bytes. A corpus-local compression ratio, not a universal MDL metric.
MIN_BYTES=$(wc -c < "$WORK/summary.min.txt")
DIFF_BYTES=$( (diff -ruN "$WORK/before" "$WORK/after" || true) | wc -c)
RATIO=$(awk "BEGIN { printf \"%.2f\", $MIN_BYTES / $DIFF_BYTES }")

printf '%s  %s  %d pairs  %d rules  %d residuals  %d/%d factored  render/diff %s\n' \
  "$NAME" "$LANG_NAME" "$PAIRS" "$RULES" "$RESIDUALS" \
  "$FACTORED" "$PAIRS" "$RATIO"
