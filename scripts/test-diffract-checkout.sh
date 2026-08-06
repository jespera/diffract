#!/usr/bin/env bash
#
# Self-test for diffract-checkout.sh. Builds throwaway git repos in a temp
# directory and asserts the manifest and extracted trees. Run directly, or via
# `dune test` (scripts/dune wires it to the runtest alias).

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
checkout="$here/diffract-checkout.sh"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

fails=0
ok()   { printf '  [OK]   %s\n' "$1"; }
fail() { printf '  [FAIL] %s\n' "$1" >&2; fails=$((fails + 1)); }
check() { if [ "$2" = "$3" ]; then ok "$1"; else fail "$1: expected '$3', got '$2'"; fi; }

git_q() { git -c user.email=t@t -c user.name=t -C "$1" "${@:2}"; }

# ── a repo exercising every record type, plus a path with a space ────────
repo="$work/repo"
mkdir -p "$repo/dir with space" && git -C "$repo" init -q .
printf 'fun keep() = 1\n'  > "$repo/dir with space/a b.kt"
printf 'fun gone() = 2\n'  > "$repo/doomed.kt"
printf 'fun same() = 3\n'  > "$repo/stable.kt"
git_q "$repo" add -A && git_q "$repo" commit -qm before
git_q "$repo" rm -q doomed.kt
printf 'fun same() = 4\n'  > "$repo/stable.kt"
printf 'fun brand() = 5\n' > "$repo/fresh.kt"
git_q "$repo" mv "dir with space/a b.kt" "dir with space/c d.kt"
git_q "$repo" add -A && git_q "$repo" commit -qm after

dest="$work/cs"
"$checkout" -C "$repo" HEAD~1 HEAD "$dest" >/dev/null 2>&1
man="$dest/pairs.tsv"
recs() { grep -v '^#' "$man" | awk -F'\t' -v k="$1" '$1==k' | wc -l | tr -d ' '; }

echo "manifest records"
check "one rename"                 "$(grep -v '^#' "$man" | awk -F'\t' '$1=="pair" && $2!=$3' | wc -l | tr -d ' ')" "1"
check "one in-place modification"  "$(grep -v '^#' "$man" | awk -F'\t' '$1=="pair" && $2==$3' | wc -l | tr -d ' ')" "1"
check "one addition"               "$(recs add)" "1"
check "one deletion"               "$(recs del)" "1"
check "space in path preserved"    "$(grep -c 'dir with space/a b.kt	dir with space/c d.kt' "$man")" "1"
check "provenance comment present" "$(grep -c '^# refs:' "$man")" "1"

echo "extracted trees"
check "renamed old side in before/" "$([ -f "$dest/before/dir with space/a b.kt" ] && echo y)" "y"
check "renamed new side in after/"  "$([ -f "$dest/after/dir with space/c d.kt" ] && echo y)" "y"
check "deleted file only in before" "$([ -f "$dest/before/doomed.kt" ] && [ ! -f "$dest/after/doomed.kt" ] && echo y)" "y"
check "added file only in after"    "$([ -f "$dest/after/fresh.kt" ] && [ ! -f "$dest/before/fresh.kt" ] && echo y)" "y"

# ── the compatibility invariant ──────────────────────────────────────────
# Detecting renames changes only git's CLASSIFICATION, not which blobs exist
# on either side, so the trees must match what --no-renames produces. Existing
# two-directory invocations therefore keep working unchanged.
echo "backward compatibility"
ref="$work/ref"
mkdir -p "$ref/before" "$ref/after"
while IFS= read -r -d '' st && IFS= read -r -d '' p; do
  case "$st" in
    A*) mkdir -p "$(dirname "$ref/after/$p")";  git -C "$repo" show "HEAD:$p"   > "$ref/after/$p" ;;
    D*) mkdir -p "$(dirname "$ref/before/$p")"; git -C "$repo" show "HEAD~1:$p" > "$ref/before/$p" ;;
    *)  mkdir -p "$(dirname "$ref/before/$p")" "$(dirname "$ref/after/$p")"
        git -C "$repo" show "HEAD~1:$p" > "$ref/before/$p"
        git -C "$repo" show "HEAD:$p"   > "$ref/after/$p" ;;
  esac
done < <(git -C "$repo" diff --no-renames -z --name-status HEAD~1 HEAD)
check "before/ matches a --no-renames run" "$(diff -r "$ref/before" "$dest/before" >/dev/null && echo same)" "same"
check "after/ matches a --no-renames run"  "$(diff -r "$ref/after"  "$dest/after"  >/dev/null && echo same)" "same"

# ── -M threshold is honoured ─────────────────────────────────────────────
# A rename whose content changed a lot scores below git's 50% default and
# falls back to add+delete; a lower threshold recovers it.
echo "-M threshold"
r2="$work/repo2"; mkdir -p "$r2" && git -C "$r2" init -q .
printf 'a\nb\nc\nd\ne\nf\ng\nh\n' > "$r2/old.txt"
git_q "$r2" add -A && git_q "$r2" commit -qm before
git_q "$r2" rm -q old.txt
printf 'a\nb\nc\nX\nY\nZ\nW\nV\n' > "$r2/new.txt"
git_q "$r2" add -A && git_q "$r2" commit -qm after
d50="$work/cs50"; d20="$work/cs20"
"$checkout" -C "$r2" HEAD~1 HEAD "$d50" >/dev/null 2>&1
"$checkout" -C "$r2" -M 20 HEAD~1 HEAD "$d20" >/dev/null 2>&1
check "not a rename at -M50" "$(grep -v '^#' "$d50/pairs.tsv" | awk -F'\t' '$1=="pair"' | wc -l | tr -d ' ')" "0"
check "rename at -M20"       "$(grep -v '^#' "$d20/pairs.tsv" | awk -F'\t' '$1=="pair" && $2!=$3' | wc -l | tr -d ' ')" "1"

# ── the large-changeset note ─────────────────────────────────────────────
# summarize's cost grows with the pair count, so the extractor says so past a
# threshold. The small repo above must stay quiet; a 501-pair one must not.
echo "large-changeset note"
check "quiet on a small changeset" \
  "$("$checkout" -C "$repo" HEAD~1 HEAD "$work/cs-quiet" 2>&1 >/dev/null | grep -c 'large changeset')" "0"
r3="$work/repo3"; mkdir -p "$r3" && git -C "$r3" init -q .
for i in $(seq 1 501); do printf 'fun f%d() = 1\n' "$i" > "$r3/f$i.kt"; done
git_q "$r3" add -A && git_q "$r3" commit -qm before
for i in $(seq 1 501); do printf 'fun f%d() = 2\n' "$i" > "$r3/f$i.kt"; done
git_q "$r3" add -A && git_q "$r3" commit -qm after
check "warns on a large changeset" \
  "$("$checkout" -C "$r3" HEAD~1 HEAD "$work/cs-big" 2>&1 >/dev/null | grep -c 'large changeset')" "1"

echo "bad input"
check "rejects non-numeric -M" "$("$checkout" -C "$repo" -M x HEAD~1 HEAD "$work/nope" >/dev/null 2>&1; echo $?)" "2"
check "rejects a non-repo"     "$("$checkout" -C "$work" HEAD~1 HEAD "$work/nope" >/dev/null 2>&1; echo $?)" "1"

if [ "$fails" -gt 0 ]; then echo "$fails check(s) failed" >&2; exit 1; fi
echo "all checks passed"
