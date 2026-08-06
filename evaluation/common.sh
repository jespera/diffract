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
#   EXPECTED_PAIRS  number of in-scope file pairs (sanity check)
# Optional:
#   EXTRA_FLAGS     extra summarize flags, e.g. --ignore-formatting
#   RENAMES         non-empty to pair files through a manifest instead of by
#                   path equality (see "rename-aware corpora" below)
#   FILE_GLOB       include glob handed to the tool, when it must differ from
#                   the git pathspec (a path-scoped GLOB like 'mod/*.scala' is
#                   a pathspec, not a file filter). Defaults to GLOB.
#   HOLDOUT_GLOB    a second pathspec from the SAME commit pair, excluded from
#                   rule derivation and used to check the rules generalize
#                   (see "holdout check" below)
#
# ── rename-aware corpora ────────────────────────────────────────────────
# Two directory trees cannot say "this before-file is that differently-named
# after-file", so a renamed file reads as an unrelated delete plus an unrelated
# add and the systematic edits inside it yield no rules. Corpora whose commit
# renames files therefore set RENAMES=1, which materializes through the shipped
# extractor (scripts/diffract-checkout.sh) and runs `summarize --pairs`. That
# also means the harness exercises the extractor on every run.

set -euo pipefail

EVAL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(dirname "$EVAL_DIR")"
WORK="$EVAL_DIR/_work/$NAME"
EXTRA_FLAGS="${EXTRA_FLAGS:-}"
RENAMES="${RENAMES:-}"
FILE_GLOB="${FILE_GLOB:-$GLOB}"
HOLDOUT_GLOB="${HOLDOUT_GLOB:-}"

note() { echo "[$NAME] $*" >&2; }

fetch_repo() {
  if [ ! -d "$WORK/repo" ]; then
    note "fetching $REPO"
    mkdir -p "$WORK/repo"
    git -C "$WORK/repo" init -q
    git -C "$WORK/repo" fetch -q --depth 1 "$REPO" "$BEFORE_SHA" "$AFTER_SHA"
  fi
}

# Materialize one slice of the commit pair into $1/{before,after} (+ pairs.tsv
# in rename mode), restricted to pathspec $2. Only changed in-scope files are
# checked out, never the whole tree, so a corpus is exactly its advertised file
# set.
materialize() {
  local dest="$1" pathspec="$2"
  mkdir -p "$dest"
  if [ -n "$RENAMES" ]; then
    "$ROOT/scripts/diffract-checkout.sh" -C "$WORK/repo" \
      "$BEFORE_SHA" "$AFTER_SHA" "$dest" "$pathspec" 2>&1 | sed "s/^/[$NAME] /" >&2
    (grep -v '^#' "$dest/pairs.tsv" || true) | awk -F'\t' '$1=="pair"{print $2}' \
      > "$dest/pairs.txt"
  else
    git -C "$WORK/repo" diff --name-only --diff-filter=M \
      "$BEFORE_SHA" "$AFTER_SHA" -- "$pathspec" > "$dest/pairs.txt"
    while IFS= read -r f; do
      mkdir -p "$dest/before/$(dirname "$f")" "$dest/after/$(dirname "$f")"
      git -C "$WORK/repo" show "$BEFORE_SHA:$f" > "$dest/before/$f"
      git -C "$WORK/repo" show "$AFTER_SHA:$f" > "$dest/after/$f"
    done < "$dest/pairs.txt"
  fi
  note "materialized $(wc -l < "$dest/pairs.txt") pair(s) into $(basename "$dest")"
}

# The after-side path a before-side path maps to. Identical unless the file
# moved, which only a manifest can tell us.
after_path() { # $1 = slice dir, $2 = before path
  if [ -n "$RENAMES" ]; then
    awk -F'\t' -v p="$2" '$1=="pair" && $2==p {print $3; exit}' "$1/pairs.tsv"
  else
    echo "$2"
  fi
}

if [ ! -d "$WORK/before" ] || [ ! -d "$WORK/after" ]; then
  rm -rf "$WORK"
  fetch_repo
  materialize "$WORK" "$GLOB"
fi

PAIRS=$(wc -l < "$WORK/pairs.txt")
if [ "$PAIRS" -ne "$EXPECTED_PAIRS" ]; then
  note "ERROR: expected $EXPECTED_PAIRS pairs, got $PAIRS"
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
  if [ -n "$RENAMES" ]; then
    "$BIN" summarize --pairs "$WORK/pairs.tsv" \
      -l "$LANG_NAME" $EXTRA_FLAGS --format "$1" > "$2"
  else
    "$BIN" summarize "$WORK/before" "$WORK/after" \
      -l "$LANG_NAME" -i "$FILE_GLOB" $EXTRA_FLAGS --format "$1" > "$2"
  fi
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

# The corpus's raw unified diff — the denominator of the render/diff ratio.
# Driven off the pairing rather than `diff -ruN` on the two trees, because a
# renamed file has no path-partner there and would be counted twice, once as a
# whole-file add and once as a whole-file delete.
raw_diff() { # $1 = slice dir
  if [ -n "$RENAMES" ]; then
    (grep -v '^#' "$1/pairs.tsv" || true) | while IFS=$'\t' read -r kind a b; do
      case "$kind" in
        pair) diff -u "$1/before/$a" "$1/after/$b" || true ;;
        add)  diff -u /dev/null     "$1/after/$a"  || true ;;
        del)  diff -u "$1/before/$a" /dev/null     || true ;;
      esac
    done
  else
    diff -ruN "$1/before" "$1/after" || true
  fi
}

# Description length: rendered-summary bytes (minimal render) over raw-diff
# bytes. A corpus-local compression ratio, not a universal MDL metric.
MIN_BYTES=$(wc -c < "$WORK/summary.min.txt")
DIFF_BYTES=$(raw_diff "$WORK" | wc -c)
RATIO=$(awk "BEGIN { printf \"%.2f\", $MIN_BYTES / $DIFF_BYTES }")

# ── holdout check ────────────────────────────────────────────────────────
# Every number above measures how well the rules fit the files they were
# derived from, which a sufficiently over-specific rule set always aces. This
# measures something else: take a slice of the same commit that summarize never
# saw, apply the derived rules to its before-state, and compare against its
# real after-state. Rules that captured the systematic change transfer; rules
# that merely memorized their sites do not.
#
# Reconstruction is rarely total — a holdout has its own ride-along edits, and
# a partial migration is the expected outcome, not a failure. The number that
# must stay at zero is *regressed*: a rule that moves a file further from its
# target is over-firing, and that is a correctness signal rather than a
# coverage one.
HOLDOUT_LINE=""
if [ -n "$HOLDOUT_GLOB" ]; then
  H="$WORK/holdout"
  if [ ! -d "$H/before" ]; then
    fetch_repo
    materialize "$H" "$HOLDOUT_GLOB"
  fi

  # Re-consume the emitted rules the way a user would: each rule's pattern
  # text is the block between its "# rule" header and its site list.
  rm -rf "$H/rules" "$H/applied"
  mkdir -p "$H/rules"
  awk -v d="$H/rules" '
    /^# rule /    { n++; f = sprintf("%s/%03d.spatch", d, n); inr = 1; next }
    /^# sites /   { inr = 0; next }
    /^# residual/ { inr = 0 }
    inr           { print > f }
  ' "$WORK/summary.txt"

  # Rule-id order is application order (a later rule may match only the
  # intermediate an earlier one produces), and the filenames preserve it.
  cp -r "$H/before" "$H/applied"
  HFAIL=0
  for spatch in "$H/rules"/*.spatch; do
    [ -e "$spatch" ] || break
    "$BIN" apply --in-place -l "$LANG_NAME" -i "$FILE_GLOB" \
      "$spatch" "$H/applied" >/dev/null 2>&1 || HFAIL=$((HFAIL + 1))
  done
  [ "$HFAIL" -eq 0 ] || note "WARNING: $HFAIL rule(s) failed to apply to the holdout"

  HELD=0; HEXACT=0; HGREW=0; HG0=0; HG1=0
  while IFS= read -r a; do
    b=$(after_path "$H" "$a")
    HELD=$((HELD + 1))
    d0=$( (diff -u "$H/before/$a"  "$H/after/$b" || true) | wc -c)
    d1=$( (diff -u "$H/applied/$a" "$H/after/$b" || true) | wc -c)
    HG0=$((HG0 + d0)); HG1=$((HG1 + d1))
    if cmp -s "$H/applied/$a" "$H/after/$b"; then HEXACT=$((HEXACT + 1)); fi
    if [ "$d1" -gt "$d0" ]; then
      HGREW=$((HGREW + 1))
      note "holdout regression: $a"
    fi
  done < "$H/pairs.txt"

  CLOSED=$(awk "BEGIN { printf \"%.0f\", $HG0 ? 100 * ($HG0 - $HG1) / $HG0 : 0 }")
  HOLDOUT_LINE=$(printf '  holdout %d: %d exact  %s%% closed  %d regressed' \
    "$HELD" "$HEXACT" "$CLOSED" "$HGREW")
fi

printf '%s  %s  %d pairs  %d rules  %d residuals  %d/%d factored  render/diff %s%s\n' \
  "$NAME" "$LANG_NAME" "$PAIRS" "$RULES" "$RESIDUALS" \
  "$FACTORED" "$PAIRS" "$RATIO" "$HOLDOUT_LINE"
